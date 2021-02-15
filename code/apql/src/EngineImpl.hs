-- Put your Preprocessor implementation in this file
module EngineImpl where

import Types
import Data.Set as Set
-- Probably more imports here

execute :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
execute idb [] edb = Right edb
execute (IDB f clauses) (sta:ss) edb = execute1 (IDB f clauses) (sta:ss) (initSta f edb)

-- execute0 :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
-- execute0 idb [] edb = Right edb
-- execute0 idb (sta:ss) edb = execute1 idb (sta:ss) (initSta sta edb)


execute1 :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
execute1 idb [] edb = Right edb
execute1 idb (sta:ss) edb = 
	case doSta idb sta edb of
		(Left a) -> Left a
		(Right newEDB) -> 
			if newEDB == edb
				then execute1 idb ss newEDB
			else
				execute1 idb (sta:ss) newEDB


initSta :: [PSpec] -> EDB -> EDB -- use it!
initSta [] edb = edb
initSta (p:ps) edb = (p, Set.empty):(initSta ps edb)

-- edb has been initialized
doSta :: IDB -> [PSpec] -> EDB -> Either ErrMsg EDB
doSta idb [] edb = Right edb
doSta (IDB f clauses) (p:ps) edb = 
	case doPspec clauses p edb of
		(Left a) -> Left a
		(Right newEDB) ->
			doSta (IDB f clauses) ps newEDB


doPspec :: [Clause] -> PSpec -> EDB -> Either ErrMsg EDB
doPspec [] pspec edb = Right edb
doPspec ((Clause (Atom name terms) atoms tests):cs) pspec edb = 
	let p = (name, length(terms)) in
		if p == pspec
			then 
				case doClause (Clause (Atom name terms) atoms tests) edb of
					(Left a) -> Left a
					(Right newEDB) -> doPspec cs pspec newEDB
		else
			doPspec cs pspec edb


doClause :: Clause -> EDB -> Either ErrMsg EDB
doClause (Clause (Atom pname terms) atoms tests) edb =
	case getETable (Clause (Atom pname terms) atoms tests) edb of
		(Left a) -> Left a
		(Right table) ->
			Right (updateEDB (pname, length(terms)) table edb)

-- after each clause
updateEDB :: PSpec -> ETable -> EDB -> EDB
updateEDB pspec table [] = [(pspec, table)]
updateEDB pspec table ((p, t):es) = 
	if p == pspec
		then (p, Set.union table t):es 
	else
		(p, t):(updateEDB pspec table es)


getETable :: Clause -> EDB -> Either ErrMsg ETable
getETable (Clause (Atom pname terms) atoms tests) edb = 
	if length atoms == 0
		then
			Right (rows2Set [(directAdd terms)])

	else
		case getRows atoms edb of
			(Left a) -> Left a
			(Right (vnames, rows)) -> 
				case refine (vnames, rows) tests edb of
					(Left a) -> Left a
					(Right rs) ->
						case storeRows vnames rs pname terms of
							(Left a) -> Left a
							(Right resRow) ->
								Right (rows2Set resRow)

directAdd :: [Term] -> Row
directAdd [] = []
directAdd ((TData a):ts) = a:(directAdd ts)


-- edb has been initialized 
getRows :: [Atom] -> EDB -> Either ErrMsg ([VName], [Row]) -- [Atom] cannot be empty if there is variable in 1/3
getRows ((Atom pname terms):[]) edb = 
	let pspec = (pname, length(terms)) in
		case getEdb pspec edb of
			(Left a) -> Left a
			(Right tab) -> 
				Right (firstVR terms (getTab terms (Set.elems tab)))
getRows ((Atom pname terms):as) edb = 
	let pspec = (pname, length(terms)) in
		case getEdb pspec edb of
			(Left a) -> Left a
			(Right tab) -> 
				case getRows as edb of
					(Left a) -> Left a
					(Right res) ->
						Right (resAnd res (terms, (getTab terms (Set.elems tab))))

-- after this, do test

rows2Set :: [Row] -> ETable
rows2Set rows = Set.fromList rows

-- row is refined (refine)
storeRows :: [VName] -> [Row] -> PName -> [Term] -> Either ErrMsg [Row]
storeRows vnames [] pname terms = Right []
storeRows vnames (r:rs) pname terms = 
	case store vnames r pname terms of
		(Left a) -> Left a
		(Right row) -> 
			case storeRows vnames rs pname terms of
				(Left a) -> Left a
				(Right rows) -> Right (row:rows)

store :: [VName] -> Row -> PName -> [Term] -> Either ErrMsg Row
store vnames row pname [] = Right []
store vnames row pname (t:ts) = 
	case t of
		(TData d) ->
			case store vnames row pname ts of
				(Left a) -> Left a
				(Right l) -> Right (d:l)
		(TVar v) ->
			case vInVs v vnames row of
				(Left a) -> Left (EInternal "Variable name appear in param1 but not param2.")
				(Right d) -> 
					case store vnames row pname ts of
						(Left a) -> Left a
						(Right l) -> Right (d:l)


-- doing test
refine :: ([VName], [Row]) -> [Test] -> EDB -> Either ErrMsg [Row]
refine (vs, []) tests edb = Right []
refine (vs, (r:rs)) tests edb = 
	case checkStay vs r tests edb of
		(Left a) -> Left a
		(Right False) -> refine (vs, rs) tests edb
		(Right True) ->
			case refine (vs, rs) tests edb of
				(Left a) -> Left a
				(Right rows) -> Right (r:rows)



checkStay :: [VName] -> Row -> [Test] -> EDB -> Either ErrMsg Bool
checkStay vs row [] edb = Right True
checkStay vs row (t:ts) edb = 
	case t of 
		(TNot (Atom pname terms)) -> 
			let pspec = (pname, length(terms)) in
				case getEdb pspec edb of
					(Left a) -> Left a
					(Right tab) -> 
						let (vnames, rows) = (firstVR terms (getTab terms (Set.elems tab))) in
							if calTNot vs row vnames rows
								then Right False
							else checkStay vs row ts edb
		(TEq t1 t2) ->
			case t1 of 
				(TData d1) -> 
					case t2 of 
						(TData d2) -> 
							if d1 == d2
								then checkStay vs row ts edb
							else
								Right False
						(TVar v2) ->
							case (vInVs v2 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d) ->
									if d == d1
										then checkStay vs row ts edb
									else Right False
				(TVar v1) ->
					case t2 of 
						(TData d2) ->
							case (vInVs v1 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d) ->
									if d == d2
										then checkStay vs row ts edb
									else Right False
						(TVar v2) ->
							case (vInVs v1 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d1) ->
									case (vInVs v2 vs row) of
										(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
										(Right d2) ->
											if d1 == d2
												then checkStay vs row ts edb
											else Right False
		(TNeq t1 t2) ->
			case t1 of 
				(TData d1) -> 
					case t2 of 
						(TData d2) -> 
							if d1 == d2
								then Right False
							else
								checkStay vs row ts edb
						(TVar v2) ->
							case (vInVs v2 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d) ->
									if d == d1
										then Right False
									else checkStay vs row ts edb
				(TVar v1) ->
					case t2 of 
						(TData d2) ->
							case (vInVs v1 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d) ->
									if d == d2
										then Right False
									else checkStay vs row ts edb
						(TVar v2) ->
							case (vInVs v1 vs row) of 
								(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
								(Right d1) ->
									case (vInVs v2 vs row) of
										(Left _) -> Left (EInternal "Variable name appear in param3 but not param2.")
										(Right d2) ->
											if d1 == d2
												then Right False
											else checkStay vs row ts edb
						


calTNot :: [VName] -> [Data] -> [VName] -> [Row] -> Bool -- true if inside, which should be removed
calTNot vnames1 row vnames2 [] = False
calTNot vnames1 row vnames2 (r:rs) = 
	if helpTNot vnames1 row vnames2 r
		then True
	else calTNot vnames1 row vnames2 rs

helpTNot :: [VName] -> [Data] -> [VName] -> [Data] -> Bool -- True if the same, should remove
helpTNot [] [] vnames2 row = True
helpTNot (v:vs) (d:ds) vnames2 row = 
	case (vInVs v vnames2 row) of 
		(Left _) -> helpTNot vs ds vnames2 row
	 	(Right d1) -> 
	 		if d1 == d 
	 			then helpTNot vs ds vnames2 row
	 		else False


vInVs :: VName -> [VName] -> [Data] -> Either String Data
vInVs v [] [] = Left "Not in."
vInVs v (v1:vs) (d1:ds) = 
	if v == v1
		then Right d1
	else vInVs v vs ds


getEdb :: PSpec -> EDB -> Either ErrMsg ETable
getEdb pspec [] = Left (EUser "Table not found.")
getEdb pspec ((p, t):ts) = 
	if pspec == p
		then Right t
	else
		getEdb pspec ts

-- where's the first?
firstVR :: [Term] -> [Row] -> ([VName], [Row])
firstVR terms rows = ((combineV [] terms), (andHelp [] [] terms rows))


resAnd :: ([VName], [Row]) -> ([Term], [Row]) -> ([VName], [Row])
resAnd (names, rows1) (terms, rows2) = (combineV names terms, calAnd names rows1 terms rows2)

calAnd :: [VName] -> [Row] -> [Term] -> [Row] -> [Row]
calAnd names [] terms rows = []
calAnd names (r:rs) terms rows = (andHelp names r terms rows)++(calAnd names rs terms rows)

andHelp :: [VName] -> [Data] -> [Term] -> [Row] -> [Row]
andHelp names row1 terms [] = []
andHelp names row1 terms (row2:rs) = 
	if canCom names row1 terms row2
		then 
			(combineD names row1 terms row2):(andHelp names row1 terms rs)
	else andHelp names row1 terms rs

addvr2Row :: ([VName], [Data]) ->  ([VName], [Row]) ->  ([VName], [Row])
addvr2Row (vs1, r) (vs2, rs) = (vs1, (r:rs))

canCom :: [VName] -> [Data] -> [Term] -> [Data] -> Bool
canCom [] [] _ _ = True
canCom (v:vs) (d1:ds) terms row = 
	case vIsInTerm v terms row of
		Left _ -> canCom vs ds terms row
		Right d -> 
			if d == d1
				then canCom vs ds terms row
			else False


vIsInTerm :: VName -> [Term] -> [Data] -> Either String Data
vIsInTerm v [] [] = Left "Not in."
vIsInTerm v ((TVar vv):ts) (d:ds) = 
	if v == vv
		then Right d
	else
		vIsInTerm v ts ds
vIsInTerm v ((TData d1):ts) (d:ds) = vIsInTerm v ts ds

combineD :: [VName] -> [Data] -> [Term] -> [Data] -> [Data]
combineD vlist dlist [] [] = dlist
combineD vlist dlist (t:ts) (d:ds) = case t of
		(TData _) -> combineD vlist dlist ts ds
		(TVar v) -> 
			if v `elem` vlist
				then combineD vlist dlist ts ds
			else
				d:(combineD vlist dlist ts ds)

combineV :: [VName] -> [Term] -> [VName]
combineV vlist [] = vlist
combineV vlist (t:ts) = case t of
		(TData _) -> combineV vlist ts
		(TVar v) -> 
			if v `elem` vlist
				then combineV vlist ts
			else
				v:(combineV vlist ts)

addTo :: (VName, Data) -> ([VName], [Data]) -> ([VName], [Data])
addTo (v, d) (vl, dl) = (v:vl, d:dl)


-- Set.elems (some Set of row) -> List of row
getTab :: [Term] -> [Row] -> [Row] -- the term of an atom (PSpec), and corresponding ETable
getTab terms [] = []
getTab terms (r:rs) = 
	if moveIn terms r []
		then r:(getTab terms rs)
	else
		getTab terms rs


moveIn :: [Term] -> [Data] -> [(VName, Data)] -> Bool -- [term] and [data] should have the same length
moveIn [] [] _ = True
moveIn ((TData d):ts) (d1:rs) tmp = 
	if d == d1
		then moveIn ts rs tmp
	else False
moveIn ((TVar v):ts) (d1:rs) tmp = 
	case inTemp v tmp of
		Left _ -> moveIn ts rs ((v, d1):tmp)
		Right d ->
			if d == d1
				then moveIn ts rs tmp
			else False

inTemp :: VName -> [(VName, Data)] -> Either String Data
inTemp v [] = Left "."
inTemp v ((name, d):tmps) = 
	if v == name
		then Right d
	else inTemp v tmps




