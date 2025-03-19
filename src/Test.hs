module Test where

import qualified Text.Parsec as P

import Text.Parsec.String (Parser)
import Text.Parsec.Char ()

import Control.Monad (void)

import Text.Parsec (many, many1, oneOf, satisfy, (<|>))

import Data.Char (isDigit)



regularParse :: Parser a -> String -> Either P.ParseError a
regularParse p = P.parse p ""



data SimpleExpr 
  = Num Int
  | Var String
  | Add SimpleExpr SimpleExpr
  | Parens SimpleExpr
  deriving (Eq,Show)


whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"


lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whitespace
  return x

digit :: Parser Char
digit = satisfy isDigit

num :: Parser Int
num = do
  digits <- many1 digit
  return $ read digits

str :: Parser String
str = do
  oneOf "\""
  s <- many $ oneOf ['a'..'z']
  oneOf "\""
  return s

numOrStr :: Parser (Either Int String)
numOrStr = (Left <$> num) <|> (Right <$> str)


