-- This is a demo driver program from APQL. Poor error handling!
-- You may or may not find it useful.
module Main (main) where

import Types
import Parser
import Preprocessor
import Engine

import qualified Data.Set as S (toList,fromList)
import System.Exit (die)
import System.Environment (getArgs)

readsRow :: ReadS Row
readsRow "" = [([], "")]
readsRow s = [(a:as, s'') | (a,s') <- reads s, (as,s'') <- readsRow s']

readRows :: String -> [Row]
readRows s = map (\l -> let [(r, "")] = readsRow l in r) (lines s)

loadTable :: FilePath -> PSpec -> IO ETable
loadTable fp (pn,pa) =
  do s <- readFile fp
     let rs = readRows s
     if all (\r -> length r == pa) rs then return $ S.fromList rs
     else die $ "bad row length in file for -i " ++ show (pn,pa)

dumpTable :: ETable -> String
dumpTable r = unlines $ map (\l -> unwords (map show l)) $ S.toList r

errmsg ::  ErrMsg -> String
errmsg (EUser s) = "User error: " ++ s
errmsg (EInternal s) = "Internal error: " ++ s
errmsg (EUnimplemented s) = "Unimplemented: " ++ s

getit :: String -> Either ErrMsg a -> IO a
getit _ (Right a) = return a
getit s (Left em) = die $ s ++ errmsg em

doArgs :: [String] -> EDB -> IO ()

doArgs [] edb = return ()

doArgs ("-i" : pn : pa : fp : args) edb =
  do let ps = (pn,read pa)
     t <- loadTable fp ps
     doArgs args ((ps,t) : edb)

doArgs ("-p" : pn : pa : args) edb =
  do let ps = (pn,read pa)
     t <- case lookup ps edb of
            Just t -> return t
            Nothing -> die $ "no table for -p " ++ show ps
     putStr $ dumpTable t
     doArgs args edb

doArgs ("-o" : pn : pa : fp : args) edb =
  do let ps = (pn,read pa)
     t <- case lookup ps edb of
            Just t -> return t
            Nothing -> die $ "no table for -o " ++ show ps
     writeFile fp $ dumpTable t
     doArgs args edb

doArgs ("-x" : fp : args) edb =
  do s <- readFile fp
     p <- getit "Parsing" $ parseString s
     putStrLn $ "Read " ++ show (length p) ++ " rules"
     idb@(IDB ps cls) <- getit "Clausifying" $ clausify p
     putStrLn $ "Produced " ++ show (length cls) ++ " clauses"
     let ips = map fst edb
     plan <- getit "Stratifying" $ stratify idb ips
     putStrLn $ "Stratifed into " ++ show (length plan) ++ " strata"
     edb' <- getit "Executing" $ execute idb plan edb
     putStrLn "Done!"
     doArgs args edb'

doArgs args _ = die $ "bad arg(s) " ++ show args

usage :: String
usage =
  "usage: apql ACT ...           where each ACT is one of:\n\
  \  -i PRED ARITY FILE.txt      load table for PRED/ARITY from FILE.txt\n\
  \  -x PGM.apql                 parse and execute program from PGM.apql\n\
  \  -p PRED ARITY               print table for PRED/ARITY to stdout\n\
  \  -o PRED ARITY FILE.txt      dump table for PRED/ARITY to FILE.txt\n"

main :: IO ()
main = do args <- getArgs
          if not (null args) then doArgs args []
          else die usage

main1 = doArgs ["-i", "person", "1", "g1-person.txt",
                "-i", "follows", "2", "g1-follows.txt",
                "-x", "examples/instahub.apql",
                "-p", "ignorant", "2"] []
