module MHaskell.Micro where

type Name = String

type Prog = [Stmt]

data Type
  = TInt
  | TBool
  | TFun Type Type
  deriving (Show,Eq)

data Stmt 
  = SFunDef Name [Name] Expr
  | STypeSig Name Type
  deriving (Show,Eq)

data Expr
  = EVar Name
  | ELitInt Int
  | ELitBool Bool
  | ELitNil
  | ELitList [Expr]
  | EBinOp Expr BinOp Expr
  | EApp Expr Expr
  | EFun Name Expr
  | ELet Name Expr Expr
  | EMatch Expr [CaseAlt]
  | EIf Expr Expr Expr
  deriving (Show,Eq)

data BinOp 
  = Add
  | Mul
  | Sub
  | Div
  | Cons
  deriving (Show,Eq)

type CaseAlt = (Pattern,Expr)

data Pattern
  = PAny
  | PVar Name
  | PCons Name [Pattern]
  deriving (Show,Eq)