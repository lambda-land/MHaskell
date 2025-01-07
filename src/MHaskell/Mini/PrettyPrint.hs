{-# LANGUAGE OverloadedStrings #-}
module MHaskell.Mini.PrettyPrint where


import MHaskell.Mini.Syntax

import MHaskell.Mini.Util

import Data.Text (Text)
import qualified Data.Text as T

import Prettyprinter

import Prettyprinter.Render.Text


prettyType :: [Doc ann] -> Doc ann
prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")

prettySig :: Text -> [Doc ann] -> Doc ann
prettySig name ty = pretty name <+> prettyType ty

example :: Doc ann
example = prettySig "example" ["Int", "Bool", "Char", "IO ()"]


instance Pretty Type where
  pretty :: Type -> Doc ann
  pretty (TCons c) = pretty $ T.pack c
  pretty (TVar v)  = pretty $ T.pack v
  pretty (TApp t1 t2) = pretty t1 <+> pretty t2
  pretty (TFun t1 t2) = pretty t1 <+> pretty ("->" :: Text) <+> pretty t2

instance Pretty Expr where
  pretty (EVar n)     = pretty (T.pack n)
  pretty (ELitInt n)  = pretty n
  pretty (ELitBool b) = pretty b
  pretty (ECons c)    = pretty (T.pack c)
  pretty (a :+: b)    = pretty a <+> pretty ("+" :: Text) <+> pretty b
  pretty (a :*: b)    = pretty a <+> pretty ("*" :: Text) <+> pretty b
  pretty (a :-: b)    = pretty a <+> pretty ("-" :: Text) <+> pretty b
  pretty (a :/: b)    = pretty a <+> pretty ("/" :: Text) <+> pretty b
  pretty (a :<: b)    = pretty a <+> pretty ("<" :: Text) <+> pretty b
  pretty (a :>: b)    = pretty a <+> pretty (">" :: Text) <+> pretty b
  pretty (a :==: b)   = pretty a <+> pretty ("==" :: Text) <+> pretty b
  pretty (a :>=: b)   = pretty a <+> pretty (">=" :: Text) <+> pretty b
  pretty (a :<=: b)   = pretty a <+> pretty ("<=" :: Text) <+> pretty b
  pretty (a ::: b)    = pretty a <+> pretty (":" :: Text) <+> pretty b
  pretty (a :&&: b)   = pretty a <+> pretty ("&&" :: Text) <+> pretty b
  pretty (a :||: b)   = pretty a <+> pretty ("||" :: Text) <+> pretty b
  pretty (EBinOp {}) = error "this will never happen"
  pretty (EApp e1 e2@(EBinOp {})) = pretty e1 <+> "(" <> pretty e2 <> ")"
  pretty (EApp e1 (EApp e2 e3)) = pretty e1 <+> "(" <> pretty (EApp e2 e3) <> ")"
  pretty (EApp e1 e2)   = pretty e1 <+> pretty e2
  pretty (ELet x e1 e2) = "let" <+> pretty x <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  pretty (EIf e1 e2 e3) = "if" <+> pretty e1 <+> "then" <+> pretty e2 <+> "else" <+> pretty e3
  pretty (EMatch e cs)  = "case" <+> pretty e <+> "of" <+> (sep $ map prettyCaseAlt cs)
  pretty (EFun x e)     = "(" <> "\\" <+> pretty x <+> "->" <+> pretty e <> ")"


prettyCaseAlt :: CaseAlt -> Doc ann
prettyCaseAlt (p,e) = pretty p <+> "->" <+> pretty e


instance Pretty Pattern where
  pretty :: Pattern -> Doc ann
  pretty PAny = pretty ("_" :: Text)
  pretty (PVar x) = pretty $ T.pack x
  pretty (PCons c ps) = "(" <> foldr (<+>) (pretty c) (map pretty ps) <> ")"

example1 = pretty $ ELitInt 2 :+: ELitInt 3

example2 = pretty $ EApp (EFun "x" (EVar "y")) (ELitInt 1 :+: ELitInt 2)