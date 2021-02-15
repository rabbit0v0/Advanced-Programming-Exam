-- This is a suggested skeleton for your main black-box tests. You are not
-- required to use Tasty, but be sure that your test suite can be build
-- and run against any implementation of the APQL APIs.
  
import Types
import Parser
import Preprocessor
import Engine
-- Do not import from the XXXImpl modules here!

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = myTest -- replace this

testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Right a -> assertFailure $ "Unexpected success: " ++ show a
      Left (EUser _) -> return () -- any message is fine
      Left em -> assertFailure $ "Error: " ++ show em


myTest :: TestTree
myTest = 
  testGroup "my tests"
  [
    testCase "parse Rule=Atom" $
      parseString "p(x)." @?= Right [Rule (Atom "p" [TVar "x"]) CTrue],
    testCase "parse Rule=Atom if Cond, Cond=Atom" $
      parseString "p(x) if q(x)." @?= Right [Rule (Atom "p" [TVar "x"]) (CAtom (Atom "q" [TVar "x"]))],
    testCase "parse Rule=Atom if Cond, Cond=is and isnot (), varname isnot, TData & TVar" $
      parseString "p(x) if v is \"u\" and (k is not isnot)." @?= Right [Rule (Atom "p" [TVar "x"]) (CAnd (CEq (TVar "v") (TData "u")) (CNot (CEq (TVar "k") (TVar "isnot"))))],
    testCase "parse Rule=Atom unless, Cond=or implies not false true" $
      parseString "p(x) unless not false implies q(x) or true ." @?= Right [Rule (Atom "p" [TVar "x"]) (CNot (CNot (COr (CNot (CNot CTrue)) (COr (CAtom (Atom "q" [TVar "x"])) CTrue))))],
    testCase "parse fail1" $
      case parseString "p(x)" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
    testCase "parse fail2" $
      case parseString "p(x) if p" of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected parse: " ++ show p,
    testCase "parse comment" $
      parseString "(**)p(x) if(*aaa*)true." @?= Right [Rule (Atom "p" [TVar "x"]) CTrue],
    testCase "clausify if false" $
      clausify [Rule (Atom "p" [TVar "x"]) (CNot CTrue)] @?= Right (IDB [("p",1)] []),
    testCase "clausify not(C and C)" $
      clausify [Rule (Atom "p" [TVar "x"]) (CAnd (CAtom (Atom "q" [TVar "x"])) (CNot (CAnd CTrue (CEq (TVar "x") (TData "true")))))]
        @?= Right (IDB [("p",1)] [Clause (Atom "p" [TVar "x"]) [Atom "q" [TVar "x"]] [TNeq (TVar "x") (TData "true")]]),
    testCase "clausify not(C or C)" $
      clausify [Rule (Atom "p" [TVar "x"]) (CAnd (CAtom (Atom "q" [TVar "x"])) (CNot (COr CTrue (CEq (TVar "x") (TData "true")))))]
        @?= Right (IDB [("p",1)] []),
    testCase "clausify not (not C)" $
      clausify [Rule (Atom "p" []) (CNot (CNot (CAtom (Atom "q" []))))] @?= Right (IDB [("p",0)] [Clause (Atom "p" []) [Atom "q" []] []]),
    testCase "clausify C and false" $
      clausify [Rule (Atom "p" []) (CAnd (CAtom (Atom "q" [])) (CNot CTrue))] @?= Right (IDB [("p",0)] []),
    testCase "clausify C and (C or C)" $
      clausify [Rule (Atom "p" []) (CAnd (CAtom (Atom "q" [])) (COr (CAtom (Atom "r" [])) CTrue))] @?= Right (IDB [("p",0)] [Clause (Atom "p" []) [Atom "q" [],Atom "r" []] [],Clause (Atom "p" []) [Atom "q" []] []]),
    testCase "clausify if false" $
      clausify [Rule (Atom "p" []) (CNot CTrue)] @?= Right (IDB [("p",0)] []),
    testCase "clausify or" $
      clausify [Rule (Atom "p" []) (COr (CAtom (Atom "q" [])) (CAtom (Atom "r" [])))] @?= Right (IDB [("p",0)] [Clause (Atom "p" []) [Atom "q" []] [],Clause (Atom "p" []) [Atom "r" []] []]),
    testCase "clausify fail: variable constrain" $
      case clausify [Rule (Atom "p" [TVar "x"]) CTrue] of
        Left e -> return ()
        Right p -> assertFailure $ "Unexpected clause: " ++ show p,
    testCase "stratify" $
      case stratify myIDB [("r",1)] of
        Right res ->
          if checkPSpec res myStrat
            then return ()
          else
            assertFailure $ "Stratify fail"
        Left e -> assertFailure $ "Stratify fail",
    testCase "stratify fail" $
      case stratify myIDB1 [("r",1)] of
        Left e -> return ()
        Right a -> assertFailure $ "Stratify should fail but did not",
    testCase "execute" $
      fmap M.fromList (execute myIDB myStrat [(("r",1), (S.fromList [["a"], ["b"], ["c"]]))])
        @?= Right (M.fromList myEDB)
  ]
  where
    myIDB = IDB [("p",2),("q",1),("s",1)] 
            [Clause (Atom "p" [TVar "x",TVar "y"]) [Atom "q" [TVar "x"],Atom "r" [TVar "y"]] [],
             Clause (Atom "q" [TData "a"]) [] [],
             Clause (Atom "s" [TVar "x"]) [Atom "r" [TVar "x"]] [TNot (Atom "q" [TVar "x"])]]
    myStrat = [[("p",2),("q",1)],[("s",1)]]
    myIDB1 = IDB [("p",1)] 
                 [Clause (Atom "p" [TVar "x"]) [Atom "q" [TVar "x"]] []]
    myEDB = [(("p",2),S.fromList [["a","a"],["a","b"],["a","c"]]),
             (("q",1),S.fromList [["a"]]),
             (("s",1),S.fromList [["b"],["c"]]),
             (("r",1),S.fromList [["a"],["b"],["c"]])]

checkPSpec :: [[PSpec]] -> [[PSpec]] -> Bool
checkPSpec l1 l2 = 
  if length l1 == length l2
    then checkSta l1 l2
  else False

checkSta :: [[PSpec]] -> [[PSpec]] -> Bool
checkSta [] [] = True
checkSta (s1:ss1) (s2:ss2) = 
  if S.fromList s1 == S.fromList s2
    then checkSta ss1 ss2
  else
    False


rudimentary :: TestTree
rudimentary =
 testGroup "Rudimentary tests"
   [testCase "parse1" $
      parseString pgmStr @?= Right pgmAST,
    testCaseBad "parse2" $
      parseString "p(x) if .",
    testCase "clausify1" $
      clausify pgmAST @?= Right pgmIDB,
    testCaseBad "clausify2" $
      clausify [Rule (Atom "p" [TVar "x"]) CTrue],
    testCase "stratify1" $ -- too strict! other correct answers also possible
      stratify pgmIDB [("r",1)] @?= Right pgmStratX,
    testCaseBad "stratify2" $
      stratify (IDB [("p",0)]
                    [Clause (Atom "p" []) [] [TNot (Atom "p" [])]]) [],
    testCase "execute" $
      fmap M.fromList (execute pgmIDB pgmStratX [(("r",1), pgmExtR)])
        @?= Right (M.fromList pgmEDB) ]
 where
   pgmStr = "p(x,y) if q(x) and r(y). q(\"a\").s(x) if r(x) and not q(x)."
   pgmAST = [Rule (Atom "p" [TVar "x", TVar "y"])
                  (CAnd (CAtom (Atom "q" [TVar "x"]))
                        (CAtom (Atom "r" [TVar "y"]))),
             Rule (Atom "q" [TData "a"])
                  CTrue]
   pgmIDB = IDB [("p", 2), ("q",1)]
                [Clause (Atom "p" [TVar "x", TVar "y"])
                        [Atom "q" [TVar "x"], Atom "r" [TVar "y"]]
                        [],
                 Clause (Atom "q" [TData "a"]) [] []]
   pgmStratX = [[("p",2), ("q",1)]]
   pgmExtR = S.fromList [["b"], ["c"]]
   pgmExtQ = S.fromList [["a"]]
   pgmExtP = S.fromList [["a", "b"], ["a", "c"]]
   pgmEDB = [(("p",2),pgmExtP), (("q",1), pgmExtQ), (("r",1), pgmExtR)]
