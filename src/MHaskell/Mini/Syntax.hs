module MHaskell.Mini.Syntax where

{---- Data Definitions ----}

type Name = String

type TypeCons = Name
type TypeVar  = Name

data Type
  = TCons TypeCons
  | TVar TypeVar
  | TApp Type Type
  | TFun Type Type
  deriving (Show,Eq)


type Prog = [Stmt]

data Stmt 
  = SFunDef Name [Name] Expr
  | STypeSig Name Type
  | STypeDef TypeCons [TypeVar] [ConsDef]
  deriving (Show,Eq)

-- Constructor name (Just, Nothing, Right, ...)
type ConsName = Name

-- Constructor definition
type ConsDef = (ConsName,[Type])

data Expr
  = EVar Name
  {-   Written as pattern synonyms
  | ELitInt Int
  | ELitBool Bool
  | ELitNil
  | ELitList [Expr]
  -}
  | ECons ConsName
  | EBinOp Expr BinOp Expr
  | EApp Expr Expr
  | EFun Name Expr
  | ELet Pattern Expr Expr
  | EMatch Expr [CaseAlt]
  | EIf Expr Expr Expr
  deriving (Show,Eq)

data BinOp 
  = Add
  | Mul
  | Sub
  | Div
  | Eq
  | Lt
  | Gt
  | Le
  | Ge
  | Cons
  deriving (Show,Eq)

type CaseAlt = (Pattern,Expr)

data Pattern
  = PAny
  | PVar Name
  | PCons Name [Pattern]
  {- TODO: Write equivalent pattern synonyms for these
  | PCons ConsName
  | PApp Pattern Pattern
  -}
  deriving (Show,Eq)
