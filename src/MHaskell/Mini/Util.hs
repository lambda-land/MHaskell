{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MHaskell.Mini.Util where




import MHaskell.Mini.Syntax

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Read (readMaybe)





{---- Very Smart Constructors ----}

pattern PolyType :: Set TypeVar -> Type -> Type
-- pattern PolyType fv t <- (\t -> (freeTypeVars t,t) -> (NonEmptySet fv,t))
pattern PolyType fv t <- (sidePut freeTypeVars -> (t,NonEmptySet fv))

pattern ELitInt :: Int -> Expr
pattern ELitInt n <- ECons (readMaybe -> Just n)
  where ELitInt n = ECons (show n)

pattern ELitBool :: Bool -> Expr
pattern ELitBool b <- ECons (readMaybe -> Just b)
  where ELitBool b = ECons (show b)

pattern ELitNil :: Expr
pattern ELitNil = ECons "Nil"

pattern EListCons :: Expr -> Expr -> Expr
pattern EListCons x xs = EApp (EApp (ECons "Cons") x) xs

pattern ELitList :: [Expr] -> Expr
pattern ELitList es <- (listExpr -> Just es)
  where ELitList es = foldr EListCons ELitNil es

{- Test cases for ELitList 

es :: [Expr]
es = [ELitInt 1, ELitInt 2, ELitInt 3]

es' :: Expr
es' = ELitList es

es'' :: [Expr]
es'' = case es' of
         (ELitList es) -> es

-- It should be that es == es'' . 
-}

pattern NullSet :: Set a
pattern NullSet <- (Set.null -> True)
  where NullSet = Set.empty

pattern NonEmptySet :: Set a -> Set a
pattern NonEmptySet s <- (maybeGuard Set.null -> Just s)
  where NonEmptySet s = case s of
                          NullSet -> error "Cannot construct NonEmptySet from empty set"
                          _       -> s

newtype Pair a b = MkPair (a,b)

pattern Pair :: a -> b -> Pair a b
pattern Pair a b = MkPair (a,b)

-- fltr f (\a -> (f a,a) -> (b,a')) = Pair b a'

-- vFilter f (\a -> (f a,a) -> ()) = Pair () a

pattern (:+:) :: Expr -> Expr -> Expr
pattern a :+: b = EBinOp a Add b

pattern (:*:) :: Expr -> Expr -> Expr
pattern a :*: b = EBinOp a Mul b

pattern (:-:) :: Expr -> Expr -> Expr
pattern a :-: b = EBinOp a Sub b

pattern (:/:) :: Expr -> Expr -> Expr
pattern a :/: b = EBinOp a Div b

pattern (:==:) :: Expr -> Expr -> Expr
pattern a :==: b = EBinOp a Eq b

pattern (:<:) :: Expr -> Expr -> Expr
pattern a :<: b = EBinOp a Lt b

pattern (:>:) :: Expr -> Expr -> Expr
pattern a :>: b = EBinOp a Gt b

pattern (:<=:) :: Expr -> Expr -> Expr
pattern a :<=: b = EBinOp a Le b

pattern (:>=:) :: Expr -> Expr -> Expr
pattern a :>=: b = EBinOp a Ge b

pattern (:::) :: Expr -> Expr -> Expr
pattern a ::: b = EBinOp a Cons b



{---- Utility Functions ----}

sidePut :: (a -> b) -> a -> (a, b)
sidePut f a = (a,f a)

maybeGuard :: (a -> Bool) -> a -> Maybe a
maybeGuard p a | p a       = Just a
               | otherwise = Nothing

freeTypeVars :: Type -> Set TypeVar
freeTypeVars = Set.fromList . freeTypeVars'

freeTypeVars' :: Type -> [TypeVar]
freeTypeVars' (TCons _) = []
freeTypeVars' (TVar x)  = [x]
freeTypeVars' (TApp t1 t2) = freeTypeVars' t1 ++ freeTypeVars' t2
freeTypeVars' (TFun t1 t2) = freeTypeVars' t1 ++ freeTypeVars' t2

listExpr :: Expr -> Maybe [Expr]
listExpr ELitNil = return []
listExpr (EListCons e e') | Just es <- listExpr e' = return (e:es)
listExpr _ = Nothing


