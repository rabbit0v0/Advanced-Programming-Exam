-- Put your Preprocessor implementation in this file.
module PreprocessorImpl where

import Types
import Data.Set as S
-- Probably more imports here

type Tmp = ([Atom],[Test])

data ReferP = Neg PSpec
	deriving (Eq, Show)

clausify :: Program -> Either ErrMsg IDB
clausify p = 
	let clauses = clausifyP p in
		if checkClauses clauses
			then Right (IDB (makeDB p) clauses)
		else
			Left (EUser "Doesn't obey the variable restrictions.")


stratify :: IDB -> [PSpec] -> Either ErrMsg [[PSpec]]
stratify (IDB ps cs) eps = case strata ps cs [eps] of
	(Left a) -> Left a 
	(Right l) -> Right (tail l)


strata :: [PSpec] -> [Clause] -> [[PSpec]] -> Either ErrMsg [[PSpec]]
strata [] _ sta = Right sta
strata l c sta = 
	let newList = formNewList1 l c sta in
		if newList == []
			then Left (EUser "Stratify empty.")
		else
			case findStabilize newList c sta of
				(Left _) -> Left (EUser "Cannot stabilize.")
				(Right ll) -> strata (getRemaining (sta ++ [ll]) l) c (sta ++ [ll])

getRemaining :: [[PSpec]] -> [PSpec] -> [PSpec]
getRemaining [] l = l
getRemaining (s:ss) l = getRemaining ss (getRemainingHelp s l)


getRemainingHelp :: [PSpec] -> [PSpec] -> [PSpec]
getRemainingHelp [] l = l
getRemainingHelp (x:xs) l = getRemainingHelp xs (removeFromList x l)


formNewList1 :: [PSpec] -> [Clause] -> [[PSpec]] -> [PSpec]
formNewList1 [] _ _ = []
formNewList1 (p:ps) c sta = 
	let ref = lookForNeg p c in
		if canStayNeg ref sta
			then p:(formNewList1 ps c sta)
		else
			formNewList1 ps c sta

-- check empty after doing

formNewList2 :: [PSpec] -> [PSpec] -> [Clause] -> [[PSpec]] -> Either String [PSpec]
formNewList2 [] _ _ _ = Left "OK."
formNewList2 (p:ps) l c sta = 
	let ref = lookForPos p c in
		if canStayPos ref l sta
			then formNewList2 ps l c sta
		else
			Right (removeFromList p l)

findStabilize :: [PSpec] -> [Clause] -> [[PSpec]] -> Either String [PSpec]
findStabilize l c sta = 
	case formNewList2 l l c sta of
		(Left _) -> Right l
		(Right ll) ->
			if ll == []
				then Left "Cannot stabilize."
			else findStabilize ll c sta

removeFromList :: PSpec -> [PSpec] -> [PSpec]
removeFromList p [] = []
-- removeFromList p (p:xs) = xs
removeFromList p (x:xs) = 
	if p == x
		then xs
	else x:(removeFromList p xs)

canStayPos :: [PSpec] -> [PSpec] -> [[PSpec]] -> Bool
canStayPos [] _ _ = True
canStayPos (r:rs) p sta = 
	if isIn r [p] || isIn r sta
		then canStayPos rs p sta
	else
		False

canStayNeg :: [ReferP] -> [[PSpec]] -> Bool
canStayNeg [] _ = True
canStayNeg ((Neg a):rs) p = 
	if isIn a p 
		then canStayNeg rs p
	else
		False


isIn :: PSpec -> [[PSpec]] -> Bool
isIn _ [] = False
isIn a (p:ps) = 
	if a `elem` p
		then True
	else
		isIn a ps

lookForPos :: PSpec -> [Clause] -> [PSpec]
lookForPos _ [] = []
lookForPos (name, i) ((Clause (Atom name1 l) a t):cs) = 
	if name == name1
		then
			if length l == i
				then (atoms2pSpecs a) ++ (lookForPos (name, i) cs)
			else
				lookForPos (name, i) cs
	else
		lookForPos (name, i) cs
-- lookFor p (_:cs) = lookFor p cs

lookForNeg :: PSpec -> [Clause] -> [ReferP]
lookForNeg _ [] = []
lookForNeg (name, i) ((Clause (Atom name1 l) a t):cs) = 
	if name == name1
		then
			if length l == i
				then (lookForNegative t) ++ (lookForNeg (name, i) cs)
			else
				lookForNeg (name, i) cs
	else
		lookForNeg (name, i) cs

lookForNegative :: [Test] -> [ReferP]
lookForNegative [] = []
lookForNegative ((TNot a):xs) = (Neg (atom2pSpec a)):(lookForNegative xs)
lookForNegative (_:xs) = lookForNegative xs

atom2pSpec :: Atom -> PSpec
atom2pSpec (Atom a l) = (a, (length l))

atoms2pSpecs :: [Atom] -> [PSpec]
atoms2pSpecs [] = []
atoms2pSpecs ((Atom a l):as) = (a, (length l)):(atoms2pSpecs as)

checkOne :: [PSpec] -> [PSpec] -> Bool
checkOne [] _ = True
checkOne (x:xs) l = 
	if x `notElem` l
		then checkOne xs l
	else
		False

checkClauses :: [Clause] -> Bool
checkClauses [] = True
checkClauses (c:cs) = 
	if checkVariable c
		then checkClauses cs
	else False

checkVariable :: Clause -> Bool
checkVariable (Clause (Atom name t) pos tes) = 
	checkIn (mergeVName (getVFromAtom t) (getThr tes)) (getSec pos)


checkIn :: [VName] -> [VName] -> Bool
checkIn [] sec = True
checkIn (v:vs) sec =
	if v `elem` sec
		then checkIn vs sec
	else False


getThr :: [Test] -> [VName]
getThr [] = []
getThr ((TEq (TVar v) (TData _)):ts) = mergeVName [v] (getThr ts)
getThr ((TEq (TData _) (TVar v)):ts) = mergeVName [v] (getThr ts)
getThr ((TEq (TVar u) (TVar v)):ts) = mergeVName [u, v] (getThr ts)
getThr ((TEq (TData _) (TData _)):ts) = getThr ts
getThr ((TNeq (TVar v) (TData _)):ts) = mergeVName [v] (getThr ts)
getThr ((TNeq (TData _) (TVar v)):ts) = mergeVName [v] (getThr ts)
getThr ((TNeq (TVar u) (TVar v)):ts) = mergeVName [u, v] (getThr ts)
getThr ((TNeq (TData _) (TData _)):ts) = getThr ts
getThr ((TNot (Atom name t)):ts) = mergeVName (getVFromAtom t) (getThr ts)

getSec :: [Atom] -> [VName]
getSec [] = []
getSec ((Atom name t):as) = mergeVName (getVFromAtom t) (getSec as)

getVFromAtom :: [Term] -> [VName]
getVFromAtom [] = []
getVFromAtom ((TVar vname):ts) = vname:(getVFromAtom ts)
getVFromAtom ((TData _):ts) = getVFromAtom ts

mergeVName :: [VName] -> [VName] -> [VName]
mergeVName l1 l2 = S.elems (S.fromList (l1++l2))


makeDB :: Program -> [PSpec]
makeDB [] = []
makeDB ((Rule (Atom name xs) cond):rs) = 
	let l = (makeDB rs) in
		let i = (name, (length xs)) in
			if i `notElem` l
				then i:l
			else l

clausifyP :: Program -> [Clause]
clausifyP [] = []
clausifyP (r:rs) = (getClause r) ++ (clausifyP rs)

getClause :: Rule -> [Clause]
getClause (Rule atom cond) = 
	case (getTmp cond) of
		(Left _) -> []
		(Right tmps) -> wrapTmp tmps atom


wrapTmp :: [Tmp] -> Atom -> [Clause]
wrapTmp [] _ = []
wrapTmp ((atoms, tests):tmps) atom = (Clause atom atoms tests):(wrapTmp tmps atom)

getTmp :: Cond -> Either String [Tmp]
getTmp (CAtom atom) = Right [([atom],[])]
getTmp (CEq t1 t2) = Right [([],[TEq t1 t2])]
getTmp CTrue = Right [([],[])]
getTmp (CAnd c1 c2) = 
	case (getTmp c1) of
		(Left _) -> Left "false"
		(Right tmps) -> case (getTmp c2) of
			(Left _) -> Left "false"
			(Right tmps1) -> Right (mergeTmp tmps tmps1)
	
getTmp (COr c1 c2) = 
	case (getTmp c1) of
		(Left _) -> case (getTmp c2) of
			(Left _) -> Left "false"
			(Right tmps1) -> Right tmps1
		(Right tmps) -> case (getTmp c2) of
			(Left _) -> Right tmps
			(Right tmps1) -> Right (tmps ++ tmps1)

getTmp (CNot (CAtom atom)) = Right [([],[TNot atom])]
getTmp (CNot (CEq t1 t2)) = Right [([],[TNeq t1 t2])]
getTmp (CNot (CNot cond)) = getTmp cond
getTmp (CNot (CAnd c1 c2)) = getTmp (COr (CNot c1) (CNot c2))
getTmp (CNot (COr c1 c2)) = getTmp (CAnd (CNot c1) (CNot c2))
getTmp (CNot CTrue) = Left "false"


mergeTmp :: [Tmp] -> [Tmp] -> [Tmp]
mergeTmp [] tmps = []
mergeTmp (x:xs) tmps = (connTmp x tmps) ++ (mergeTmp xs tmps)

connTmp :: Tmp -> [Tmp] -> [Tmp]
connTmp (atoms,tests) [] = []
connTmp (atoms,tests) ((a,t):tmps) = ((atoms++a),(tests++t)):(connTmp (atoms,tests) tmps)
