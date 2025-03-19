module MHaskell.Micro.Lexer where

import Prelude hiding (lex)

import Text.Parsec as P

import Text.Parsec.String

import Text.Parsec.Char

import Text.Parsec.Prim
import Control.Monad (unless)


data Tok
  = TokInt Int
  | TokBool Bool
  | TokIdent String
  | TokOp String
  deriving (Show,Eq,Ord)

data Token
  = Token { tok :: Tok
          , pos :: SourcePos
          } deriving (Show)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]


data BlockState
  = BlockState { indentStack :: [Int]
               , consumedInput :: [[String]]
               , newlineTally :: [SourcePos]
               } deriving (Show)

initialState :: BlockState
initialState = BlockState { indentStack = []
                          , consumedInput = []
                          , newlineTally = []
                          }

type BlockParser a = Parsec String BlockState a


pushConsumed :: String -> BlockParser ()
pushConsumed s = do
  modifyState (\st -> st { consumedInput = push (consumedInput st) })
  where push [] = [[s]]
        push xs = snoc xs [s]


blockify :: BlockParser ()
blockify = do
  chunk <- manyTill (noneOf "\n\r") (lookAhead newline)
  pos <- getPosition
  modifyState (\st -> st { newlineTally = snoc (newlineTally st) pos })
  newline
  unless (null chunk) (pushConsumed chunk)
  eof <|> blockify
  return ()

blockify' :: BlockParser (String,BlockState)
blockify' = do
  blockify
  s <- getState
  input <- getInput
  return (input,s)


weird :: String -> Parser String
weird xs = tokens id const xs

weird' :: String -> Parser String
weird' xs = tokens' id const xs


runP :: Show a => Parser a -> String -> IO ()
runP p input = do
  case parse p "" input of
    Left err -> print err
    Right a -> print a


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
