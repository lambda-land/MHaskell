module MHaskell.Micro.Syntax where

type Name = String

type Prog = [Stmt]

data Type
  = TInt
  | TBool
  | TIntList
  | TBoolList
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
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Cons
  | And
  | Or
  deriving (Show,Eq)

binOpPrec :: 

instance Ord BinOp where
  o1 <= o2 | o1 == o2 = True


allBinOps :: [BinOp]
allBinOps = [Add,Mul,Sub,Div,Eq,Ne,Lt,Gt,Le,Ge,Cons,And,Or]

binOpPP :: BinOp -> String
binOpPP Add = "+"
binOpPP Mul = "*"
binOpPP Sub = "-"
binOpPP Div = "/"
binOpPP Eq = "=="
binOpPP Ne = "!="
binOpPP Lt = "<"
binOpPP Gt = ">"
binOpPP Le = "<="
binOpPP Ge = ">="
binOpPP Cons = ":"
binOpPP And = "&&"
binOpPP Or = "||"

type CaseAlt = (Pattern,Expr)

data Pattern
  = PAny
  | PVar Name
  | PLitInt Int
  | PLitBool Bool
  | PLitNil
  | PListCons Pattern Pattern
  deriving (Show,Eq)