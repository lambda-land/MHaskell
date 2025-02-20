module MHaskell.Micro.Parser where
 
import Text.Parsec as P
import qualified Text.Parsec.Language as Lang
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec ((<|>),(<?>),eof)
import Text.Parsec.Expr
import Text.Parsec.Char

import Data.Functor.Identity (Identity)

import MHaskell.Micro.Syntax

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void)


languageDef :: Tok.LanguageDef ()
languageDef = Lang.haskellDef
-- languageDef = haskellStyle
--   { Tok.reservedNames = ["Int", "Bool", "if", "then", "else", "let", "in", "match", "with", "True", "False", "Nil"]
--   , Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", ":", "->", "=", "::", "\\", "_"]
--   , Tok.identStart = letter <|> char '_'
--   , Tok.identLetter = alphaNum <|> oneOf "_'"
--   }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

operator :: String -> Parser ()
operator = Tok.reservedOp lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

charLiteral :: Parser Char
charLiteral = Tok.charLiteral lexer

stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer

natural :: Parser Integer
natural = Tok.natural lexer

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = Tok.naturalOrFloat lexer

decimal :: Parser Integer
decimal = Tok.decimal lexer

hexadecimal :: Parser Integer
hexadecimal = Tok.hexadecimal lexer

octal :: Parser Integer
octal = Tok.octal lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

angles :: Parser a -> Parser a
angles = Tok.angles lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

squares :: Parser a -> Parser a
squares = Tok.brackets lexer

semi :: Parser String
semi = Tok.semi lexer

comma :: Parser String
comma = Tok.comma lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

binOp opf ops assoc = Infix (reservedOp ops >> return (\lhs rhs -> EBinOp lhs opf rhs)) assoc

type OpTable a = OperatorTable String () Identity a

operatorTable :: OpTable Expr
operatorTable = [[binOp Add "+" AssocLeft,binOp Add "+" AssocLeft]
                ,[binOp Mul "*" AssocLeft,binOp Div "/" AssocLeft]]

eol :: Parser ()
eol = do
  _ <- char '\n'
  return ()

endOfStmt :: Parser ()
endOfStmt = do
  void endOfLine


intLiteral :: Parser Int
intLiteral = fromIntegral <$> integer

boolLiteral :: Parser Bool
boolLiteral = true <|> false
  where true  = reserved "True" >> return True
        false = reserved "False" >> return False

nilLiteral :: Parser ()
nilLiteral = reserved "[]"

literalExpr :: Parser Expr
literalExpr = ELitInt <$> intLiteral 
       <|> ELitBool <$> boolLiteral 
       <|> const ELitNil <$> nilLiteral
       <?> "literal"

variableExpr :: Parser Expr
variableExpr = EVar <$> identifier

lambdaExpr :: Parser Expr
lambdaExpr = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return $ EFun x e

-- atom' :: Parser Expr
-- atom' = variableExpr
--     <|> literalExpr
--     <|> parens atom'
--     <?> "atom'"

atom :: Parser Expr
atom = variableExpr
    <|> literalExpr
    <|> parens expr
    <?> "atom"

listExpr :: Parser Expr
listExpr = do
  es <- brackets $ commaSep expr
  return $ ELitList es

exprFactor :: Parser Expr
exprFactor = atom
          <|> lambdaExpr
          <|> listExpr
          <|> parens expr
          <?> "factor"
-- exprFactor = parens expr
--           <|> literal
--           <|> variable
--           <|> lambdaExpr
--           <|> listExpr
--           -- <|> letExpr
--           -- <|> ifExpr
--           -- <|> matchExpr

exprApp :: Parser Expr
exprApp = do
  es <- many exprFactor
  -- lookAhead (try (void stmt) <|> try (eof)) -- might need this
  return $ foldl1 EApp es

expr :: Parser Expr
expr = exprFactor
    <|> exprApp
    <?> "expression"




funDef = do
  name <- identifier <?> "function name"
  args <- many identifier <?> "function arguments"
  reservedOp "=" <?> "function definition"
  body <- expr <?> "function body"
  return $ SFunDef name args body


typeLit = (reserved "Int" >> return TInt <?> "Int")
          <|> (reserved "Bool" >> return TBool <?> "Bool")
          <|> (reserved "IntList" >> return TIntList <?> "IntList")
          <|> (reserved "BoolList" >> return TBoolList <?> "BoolList")

typeFun = do
  t1 <- typeP <?> "function type (input)"
  reserved "->"
  t2 <- typeP <?> "function type (output)"
  return $ TFun t1 t2

typeP :: Parser Type
typeP = (typeLit <?> "lit type") <|> (typeFun <?> "fun type")

typeSig = do
  name <- identifier <?> "function name"
  reservedOp "::"
  -- ty <- typeP <?> "function type"
  ty <- typeParser <?> "function type"
  return $ STypeSig name ty

-- Type parser
-- typeSig :: Parser Stmt
-- typeSig = do
--   name <- identifier
--   symbol "::"
--   ty <- typeParser
--   return $ STypeSig name ty

-- Type parser
typeParser :: Parser Type
typeParser = buildExpressionParser typeOperators typeTerm

typeOperators :: OperatorTable String () Identity Type
typeOperators = [[Infix ((reservedOp "->" >> return TFun) <?> "type function ->") AssocRight]]

typeTerm :: Parser Type
typeTerm = parens typeParser
       <|> ((reserved "Int" >> return TInt) <?> "type Int")
       <|> ((reserved "Bool" >> return TBool) <?> "type Bool")
       <|> ((brackets (reserved "Int") >> return TIntList) <?> "type IntList")
       <|> ((brackets (reserved "Bool") >> return TBoolList) <?> "type BoolList")

stmt :: Parser Stmt
stmt = do
  whiteSpace
  choice [ try (typeSig <?> "function signature")
         , funDef <?> "function def"
         ] <?> "statement"
-- stmt = ((try (typeSig <?> "function signature")) <|> (funDef <?> "function def")) <?> "statement"
-- stmt = (try (typeSig <?> "function signature")) <|> (try (funDef <?> "function def")) <?> "statement"

-- stmt' = do
--   whiteSpace
--   ss <- (stmt << whiteSpace) <?> "statement'"
--   whiteSpace
--   return ss
stmt' = stmt

program :: Parser [Stmt]
program = many stmt'


parseString :: Parser a -> String -> a
parseString p s = case parse p "" s of
                    (Left e)  -> error (show e)
                    (Right a) -> a


{-# NOINLINE parseFile #-}
parseFile :: Parser a -> String -> a
parseFile p f = parseString p src
  where !src = unsafePerformIO (readFile f)

{-# NOINLINE parseFileIO #-}
parseFileIO :: Show a => Parser a -> String -> IO ()
parseFileIO p f = do
  -- src <- readFile f
  let !src = unsafePerformIO (readFile f)
  case parse p "" src of
    (Left e)  -> print e
    (Right a) -> print a

