module MHaskell.Micro.Playground where

import MHaskell.Micro.Syntax

import MHaskell.Micro.Parser

{-# NOINLINE ex1 #-}
ex1 = flip parseFileIOProgram "trial.uhs" parseProgram