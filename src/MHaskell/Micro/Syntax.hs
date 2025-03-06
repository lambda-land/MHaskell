module MHaskell.Micro.Syntax where

import Text.Read (Read)
import qualified Text.Read as Read
import Data.Maybe (listToMaybe)


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
  | EInt Int
  | EBool Bool
  | ENil
  | EList [Expr]
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

type CaseAlt = (Pattern,Expr)

data Pattern
  = PAny
  | PVar Name
  | PInt Int
  | PBool Bool
  | PNil
  | PListCons Pattern Pattern
  deriving (Show,Eq)


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

allBinOps :: [BinOp]
allBinOps = [Add,Mul,Sub,Div,Eq,Ne,Lt,Gt,Le,Ge,Cons,And,Or]

binOpAssoc :: [(BinOp, String)]
binOpAssoc = [(op,binOpPP op) | op <- allBinOps]

allBinOpAssoc :: ([(BinOp, String)], [(String, BinOp)])
allBinOpAssoc = (binOpAssoc, map swap binOpAssoc)
  where swap (a,b) = (b,a)

-- strToBinOp :: String -> Maybe BinOp
-- strToBinOp s = lookup s $ map swap allBinOpAssoc
--   where swap (a,b) = (b,a)
--         -- find :: (a -> Bool) -> [a] -> Maybe a
--         -- find p = listToMaybe . filter p

instance Read BinOp where
  readPrec :: Read.ReadPrec BinOp
  readPrec = do 
    Read.Symbol s <- Read.lexP
    case lookup s (snd allBinOpAssoc) of
      Just op -> return op
      Nothing -> Read.pfail

strToBinOp :: String -> Maybe BinOp
strToBinOp s = Read.readMaybe s :: Maybe BinOp