-- Put your Parser implmenetation in this file.
module ParserImpl where

import Text.ParserCombinators.ReadP as RP
import Data.Char
import Control.Applicative
import Types
-- probably more imports here

reserveWord :: [String]
reserveWord = ["and", "false", "if", "implies", "in", "is", "not", "or", "true", "unless"]

lexeme :: ReadP a -> ReadP a
lexeme p = do skipSpaces;
		skipMany pComment;
		a <- p;
		skipMany pComment;
		skipSpaces;
		return a

lexeme1 :: ReadP a -> ReadP a
lexeme1 p = do skipSpaces;
		skipMany pComment;
		a <- p;
		skipMany1 pComment;
		skipSpaces;
		return a

pComment :: ReadP String
pComment = (do
	s <- RP.between (string "(*") (string "*)") (RP.many get);
	return s)

pProgram :: ReadP Program
pProgram = (do
	rule <- pRule;
	_ <- lexeme (string ".");
	rules <- pProgram;
	return (rule:rules))
	<++ (do return [])

pRule :: ReadP Rule
pRule = (do
	atom <- pAtom;
	_ <- lexeme (string "if ");
	con <- pCond;
	return (Rule atom con))
	<++ (do
		atom <- pAtom;
		_ <- lexeme1 (string "if");
		con <- pCond;
		return (Rule atom con))
	<++ (do
		atom <- pAtom;
		_ <- lexeme (string "unless ");
		con <- pCond;
		return (Rule atom (CNot con)))
	<++ (do
		atom <- pAtom;
		_ <- lexeme1 (string "unless");
		con <- pCond;
		return (Rule atom (CNot con)))
	<++ (do
		atom <- pAtom;
		return (Rule atom CTrue))

pCond :: ReadP Cond
pCond = (do 
	con <- pCond1;
	pCond0 con)
	<++ pCond1

pCond0 :: Cond -> ReadP Cond
pCond0 con = (do
	_ <- lexeme (string "and ");
	c <- pCond1;
	pCond0 (CAnd con c))
	<++ (do
		_ <- lexeme1 (string "and");
		c <- pCond1;
		pCond0 (CAnd con c)
		)
	<++ (do
		_ <- lexeme (string "or ");
		c <- pCond1;
		pCond0 (COr con c))
	<++ (do
		_ <- lexeme1 (string "or");
		c <- pCond1;
		pCond0 (COr con c))
	<++ (do
		_ <- lexeme (string "implies ");
		c <- pCond;
		pCond0 (COr (CNot con) c))
	<++ (do
		_ <- lexeme1 (string "implies");
		c <- pCond;
		pCond0 (COr (CNot con) c))
	<++ return con


pCond1 :: ReadP Cond
pCond1 = (do
	atom <- pAtom;
	return (CAtom atom))
	<++ (do
		t1 <- pTerm;
		_ <- lexeme (string "is ");
		t2 <- pTerm;
		return (CEq t1 t2))
	<++ (do
		t1 <- pTerm;
		_ <- lexeme1 (string "is");
		t2 <- pTerm;
		return (CEq t1 t2))
	<++ (do
		t1 <- pTerm;
		_ <- lexeme (string "is not ");
		t2 <- pTerm;
		return (CNot (CEq t1 t2)))
	<++ (do
		t1 <- pTerm;
		_ <- lexeme1 (string "is not");
		t2 <- pTerm;
		return (CNot (CEq t1 t2)))
	<++ (do
		_ <- lexeme (string "true");
		return CTrue)
	<++ (do
		_ <- lexeme (string "false");
		return (CNot CTrue))
	<++ (do
		_ <- lexeme (string "not ");
		con <- pCond;
		return (CNot con))
	<++ (do
		_ <- lexeme1 (string "not");
		con <- pCond;
		return (CNot con))
	<++ (do
		_ <- lexeme (string "(");
		con <- pCond;
		_ <- lexeme (string ")");
		return con)

pAtom :: ReadP Atom
pAtom = do 
	name <- pName;
	_ <- lexeme (char '(');
	term <- pTermz;
	_ <- lexeme (char ')');
	return (Atom name term)

pTermz :: ReadP [Term]
pTermz = pTerms
	<++ (do return [])

pTerms :: ReadP [Term]
pTerms = (do
	t <- pTerm;
	_ <- lexeme (char ',');
	ts <- pTerms;
	return (t:ts))
	<++(do
		t <- pTerm;
		return [t])

pTerm :: ReadP Term
pTerm = (do
	vName <- pName;
	return (TVar vName))
	<++
	(do
		tData <- pConstant;
		return (TData tData))

pName :: ReadP String
pName = lexeme $ do
	x <- satisfy (\c -> isLetter c);
	xs <- munch (\c -> isLetter c || isDigit c || c == '_');
	let vName = x:xs;
	if vName `notElem` reserveWord
		then return vName
	else
		fail "Use reserved word as an VName."

pConstant ::  ReadP Data
pConstant = lexeme $ do
	_ <- string "\"";
	content <- RP.many (
		satisfy (\x -> isPrint x && x /= '\"')
		<|> (do _ <- string "\"\""; return ('\"'))
		);
	_ <- string "\"";
	let c = [x | x <- content, x `notElem` "\NUL"];
	return c

parseString :: String -> Either ErrMsg Program
parseString s = if (readP_to_S pProgram s == [])
	then
		Left (EInternal "Parse failed, no results get.")
	else (do
		let tmp = [x | x <- readP_to_S pProgram s, snd x == ""];
		if tmp == []
			then
				Left (EUser "Parse failed. Illegal program expression.")
			else
				Right (fst (head tmp))
		)

seeParse :: String -> [(Program, String)]
seeParse s = [x | x <- readP_to_S pProgram s, snd x == ""]

seeParses :: String -> [(Program, String)]
seeParses s = [x | x <- readP_to_S pProgram s]