module MHaskell.Micro where


type Name = String

type Prog = [Stmt]

data Stmt 
  = SFunDef Name [Name] Expr
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

