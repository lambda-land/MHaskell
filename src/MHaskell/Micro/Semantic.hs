module MHaskell.Micro.Semantic where

import MHaskell.Micro.Syntax

type Env = [(Name,Type)]

eval :: Env -> Expr -> Either String Value