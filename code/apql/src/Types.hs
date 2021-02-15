-- Do not modify anything in this file!
module Types where

import qualified Data.Set as S

-- Abstract syntax

type Program = [Rule]

data Rule = Rule Atom Cond
  deriving (Eq, Show)

data Cond =
    CAtom Atom
  | CTrue
  | CEq Term Term
  | CAnd Cond Cond
  | COr Cond Cond
  | CNot Cond
  deriving (Eq, Show)

data Atom = Atom PName [Term]
  deriving (Eq, Show)

data Term =
    TVar VName
  | TData Data
  deriving (Eq, Show)

type PName = String
type VName = String
type Data = String

-- Clausal form

data IDB = IDB [PSpec] [Clause]
  deriving (Eq, Show)

type PSpec = (PName, Int)

data Clause = Clause Atom [Atom] [Test]
  deriving (Eq, Show)

data Test =
   TNot Atom
 | TEq Term Term
 | TNeq Term Term
 deriving (Eq, Show)

-- Execution
type Row = [Data]

type ETable = S.Set Row

type EDB = [(PSpec, ETable)]

-- Errors
data ErrMsg =
    EUser String
  | EInternal String
  | EUnimplemented String
  deriving (Eq, Show)
