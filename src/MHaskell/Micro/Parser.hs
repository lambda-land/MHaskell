module MHaskell.Micro.Parser where
 
import Text.Parsec as P
import qualified Text.Parsec.Language as Lang
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

import Text.Parsec ((<|>),(<?>),eof)
import Text.Parsec.Expr


import Data.Functor.Identity (Identity)

import MHaskell.Micro.Syntax

import System.IO.Unsafe (unsafePerformIO)


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


literal :: Parser Expr
literal = intLiteral <|> boolLiteral
                                    -- <|> nilLiteral
                                    -- <|> listLiteral

intLiteral :: Parser Expr
intLiteral = do
  n <- integer
  return $ ELitInt (fromIntegral n)

boolLiteral :: Parser Expr
boolLiteral = (reserved "True" >> return (ELitBool True))
          <|> (reserved "False" >> return (ELitBool False))


binOpP :: BinOp -> (Expr -> Expr -> Expr) -> Parser (Expr -> Expr -> Expr)
binOpP op f = reservedOp (show op) >> return f

type OpTable a = OperatorTable String () Identity a

operatorTable :: OpTable Expr
operatorTable = [[Infix (reservedOp "+" >> return (\lhs rhs -> EBinOp lhs Add rhs)) AssocLeft
                 ,Infix (reservedOp "-" >> return (\lhs rhs -> EBinOp lhs Sub rhs)) AssocLeft]
                ]
{-
  operatorTable = [
    [Infix (reservedOp ":" >> return (\lhs rhs -> EBinOp lhs Cons rhs)) AssocRight],
    
    [Infix (reservedOp "*" >> return (\lhs rhs -> EBinOp lhs Mul rhs)) AssocLeft,
    Infix (reservedOp "/" >> return (\lhs rhs -> EBinOp lhs Div rhs)) AssocLeft],

    [Infix (reservedOp "+" >> return (\lhs rhs -> EBinOp lhs Add rhs)) AssocLeft,
    Infix (reservedOp "-" >> return (\lhs rhs -> EBinOp lhs Sub rhs)) AssocLeft],
    
    [Infix (reservedOp "==" >> return (\lhs rhs -> EBinOp lhs Eq rhs)) AssocNone,
    Infix (reservedOp "<" >> return (\lhs rhs -> EBinOp lhs Lt rhs)) AssocNone,
    Infix (reservedOp ">" >> return (\lhs rhs -> EBinOp lhs Gt rhs)) AssocNone,
    Infix (reservedOp "<=" >> return (\lhs rhs -> EBinOp lhs Le rhs)) AssocNone,
    Infix (reservedOp ">=" >> return (\lhs rhs -> EBinOp lhs Ge rhs)) AssocNone]
    ]
-}


expr :: Parser Expr
expr = buildExpressionParser operatorTable exprTerm

exprTerm :: Parser Expr
exprTerm = do
  es <- many1 exprFactor
  return $ foldl1 EApp es

exprFactor :: Parser Expr
exprFactor = parens expr
          <|> literal
          -- <|> variable
          -- <|> lambdaExpr
          -- <|> letExpr
          -- <|> ifExpr
          -- <|> matchExpr






typeLit = (reserved "Int" >> return TInt)
          <|> (reserved "Bool" >> return TBool)
          <|> (reserved "IntList" >> return TIntList)
          <|> (reserved "BoolList" >> return TBoolList)

typeFun = do
  t1 <- typeP
  reserved "->"
  t2 <- typeP
  return $ TFun t1 t2

typeP :: Parser Type
typeP = typeLit <|> typeFun

funDef = do
  name <- identifier <?> "function name"
  args <- many identifier <?> "function arguments"
  reservedOp "="
  body <- expr <?> "function body"
  return $ SFunDef name args body

typeSig = do
  name <- identifier <?> "function name"
  reservedOp "::"
  ty <- typeP <?> "function type"
  return $ STypeSig name ty

stmt :: Parser Stmt
stmt = funDef <|> typeSig <?> "statement"

program :: Parser [Stmt]
program = whiteSpace >> stmts <* eof
  where stmts = many stmt


parseString :: Parser a -> String -> a
parseString p s = case parse p "" s of
                    (Left e)  -> error (show e)
                    (Right a) -> a


parseFile :: Parser a -> String -> a
parseFile p f = parseString p src
  where !src = unsafePerformIO (readFile f)


parseFileIO :: Show a => Parser a -> String -> IO ()
parseFileIO p f = do
  src <- readFile f
  case parse p "" src of
    (Left e)  -> print e
    (Right a) -> print a

