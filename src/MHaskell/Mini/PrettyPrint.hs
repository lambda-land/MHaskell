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


instance Pretty Expr where
  pretty :: Expr -> Doc ann
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
  pretty (EApp e1 e2) = pretty e1 <+> pretty e2
  pretty (ELet x e1 e2) = "let" <+> pretty (T.pack x) <+> "=" <+> pretty e1 <+> "in" <+> pretty e2
  pretty (EIf e1 e2 e3) = "if" <+> pretty e1 <+> "then" <+> pretty e2 <+> "else" <+> pretty e3


example1 = pretty $ ELitInt 2 :+: ELitInt 3
