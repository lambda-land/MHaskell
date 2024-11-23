{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MHaskell.Mini where

import Text.Read (readMaybe)

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
  -- | ELitInt Int     -- ?
  -- | ELitBool Bool   -- ?
  -- | ELitNil         -- ?*
  -- | ELitList [Expr] -- ?*
  | ECons ConsName
  | EBinOp Expr BinOp Expr
  | EApp Expr Expr
  | EFun Name Expr
  | ELet Pattern Expr Expr
  | EMatch Expr [CaseAlt]
  | EIf Expr Expr Expr
  deriving (Show,Eq)

pattern ELitInt :: Int -> Expr
pattern ELitInt n <- ECons (readMaybe -> Just n)
  where ELitInt n = ECons (show n)

pattern ELitBool :: Bool -> Expr
pattern ELitBool b <- ECons (readMaybe -> Just b)
  where ELitBool b = ECons (show b)

pattern ELitNil :: Expr
pattern ELitNil = ECons "[]"

pattern EListCons :: Expr -> Expr -> Expr
pattern EListCons x xs = EApp (EApp (ECons ":") x) xs

listExpr :: Expr -> Maybe [Expr]
listExpr ELitNil = return []
listExpr (EListCons e e') | Just es <- listExpr e' = return (e:es)
listExpr _ = Nothing

pattern ELitList :: [Expr] -> Expr
pattern ELitList es <- (listExpr -> Just es)
  where ELitList es = foldr EListCons ELitNil es


es :: [Expr]
es = [ELitInt 1, ELitInt 2, ELitInt 3]

es' :: Expr
es' = ELitList es

es'' :: [Expr]
es'' = case es' of
         (ELitList es) -> es



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
  -- | PCons ConsName
  -- | PApp Pattern Pattern
  deriving (Show,Eq)


