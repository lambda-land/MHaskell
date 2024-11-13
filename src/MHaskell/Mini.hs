module MHaskell.Mini where


type Name = String

type TypeCons = Name
type TypeVar  = Name

data Type
  = TInt       -- ?
  | TBool      -- ?
  | TList Type -- ?*
  | TPair Type Type -- ?*
  | TCons TypeCons
  | TVar TypeVar
  -- | TAbs TypeVar Type -- forall a. T ?*
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
  | ELitInt Int     -- ?
  | ELitBool Bool   -- ?
  | ELitNil         -- ?*
  | ELitList [Expr] -- ?*
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
  | PLitInt Int    -- ?
  | PLitBool Bool  -- ?
  | PLitNil        -- ?*
  -- PList [Pattern] ? -- case e of [x,y] -> e
  | PListCons Pattern Pattern -- ?*
  -- | PCons Name [Pattern]      -- Should `[] -> ...` be `PCons "[]" (...)`?
  | PCons ConsName
  | PApp Pattern Pattern -- (1:xs) = ((Cons 1) xs)
  deriving (Show,Eq)