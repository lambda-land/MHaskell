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


blockify :: BlockParser String
blockify = do
  chunk <- manyTill (noneOf "\n\r") (lookAhead newline)
  pos <- getPosition
  modifyState (\st -> st { newlineTally = snoc (newlineTally st) pos })
  unless (null chunk) (pushConsumed chunk)
  nl <- newline
  next <- (eof >> return "") <|> blockify
  let rest = nl : next
  if null chunk then do
    return rest
  else do
    return $ chunk ++ ";" ++ rest

blockify' :: BlockParser (String,String,BlockState)
blockify' = do
  processed <- blockify
  s <- getState
  input <- getInput
  return (input,processed,s)


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
    Right (inp,pro,s) -> do
      print inp
      print s
      print pro
      print "----"
      putStr pro
      print "----"


{-# NOINLINE lexFile #-}
lexFile :: String -> IO ()
lexFile f = do
  src <- readFile f
  lex src

-- lexFile :: Parser  -> String -> a
-- lexFile p f = parseString p src
--   where !src = unsafePerformIO (readFile f)
