module MyLib (someFunc) where

import MHaskell.Mini



someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
someFunc = print (ELitInt 1)