module MHaskell.Micro.Lexer where

import Prelude hiding (lex)

import Text.Parsec as P

import Text.Parsec.String

import Text.Parsec.Char

import Text.Parsec.Prim

data BlockState
  = BlockState { indentStack :: [Int]
               , consumedInput :: [[String]]
               } deriving (Show)

initialState :: BlockState
initialState = BlockState { indentStack = [0]
                          , consumedInput = [[]]
                          }

type BlockParser a = Parsec String BlockState a


pushConsumedInput :: String -> BlockParser ()
pushConsumedInput s = modifyState (\st -> st { consumedInput = push (consumedInput st) })
  where push (h:t) = (h ++ [s]) : t
        push [] = [[s]]

blockify :: BlockParser ()
blockify = do
  -- input <- getInput=
  chunk <- manyTill anyChar (try newline)
  pushConsumedInput chunk
  eof <|> blockify
  return ()

blockify' :: BlockParser (String,BlockState)
blockify' = do
  blockify
  s <- getState
  input <- getInput
  return (input,s)




lex :: String -> IO ()
lex input = do
  case runParser blockify' initialState "" input of
    Left err -> print err
    Right a -> print a


{-# NOINLINE lexFile #-}
lexFile :: String -> IO ()
lexFile f = do
  src <- readFile f
  lex src

-- lexFile :: Parser  -> String -> a
-- lexFile p f = parseString p src
--   where !src = unsafePerformIO (readFile f)
