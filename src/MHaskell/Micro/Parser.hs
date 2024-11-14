module MHaskell.Micro.Parser where
 
{-
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)

import MHaskell.Micro

-- Lexer definitions
languageDef :: Tok.LanguageDef ()
languageDef = haskellStyle
  { Tok.reservedNames = ["Int", "Bool", "if", "then", "else", "let", "in", "match", "with", "True", "False", "Nil"]
  , Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", ":", "->", "=", "::", "\\", "_"]
  , Tok.identStart = letter <|> char '_'
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser languageDef

-- Token parsers
identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

integer :: Parser Integer
integer = Tok.integer lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Operator table for expressions
operatorTable :: OperatorTable String () Identity Expr
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

-- Expression parser
expr :: Parser Expr
expr = buildExpressionParser operatorTable exprTerm

exprTerm :: Parser Expr
exprTerm = do
  es <- many1 exprFactor
  return $ foldl1 EApp es

exprFactor :: Parser Expr
exprFactor = parens expr
          <|> literal
          <|> variable
          <|> lambdaExpr
          <|> letExpr
          <|> ifExpr
          <|> matchExpr

-- Literal parsers
literal :: Parser Expr
literal = intLiteral
      <|> boolLiteral
      <|> nilLiteral
      <|> listLiteral

intLiteral :: Parser Expr
intLiteral = do
  n <- integer
  return $ ELitInt (fromIntegral n)

boolLiteral :: Parser Expr
boolLiteral = (reserved "True" >> return (ELitBool True))
          <|> (reserved "False" >> return (ELitBool False))

nilLiteral :: Parser Expr
nilLiteral = reserved "Nil" >> return ELitNil

listLiteral :: Parser Expr
listLiteral = do
  es <- brackets $ commaSep expr
  return $ ELitList es

-- Variable parser
variable :: Parser Expr
variable = do
  x <- identifier
  return $ EVar x

-- Lambda expression parser
lambdaExpr :: Parser Expr
lambdaExpr = do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return $ EFun x e

-- Let expression parser
letExpr :: Parser Expr
letExpr = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet x e1 e2

-- If expression parser
ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- expr
  reserved "then"
  e1 <- expr
  reserved "else"
  e2 <- expr
  return $ EIf cond e1 e2

-- Match expression parser
matchExpr :: Parser Expr
matchExpr = do
  reserved "match"
  e <- expr
  reserved "with"
  alts <- many1 caseAlt
  return $ EMatch e alts

caseAlt :: Parser CaseAlt
caseAlt = do
  symbol "|"
  p <- pattern
  reservedOp "->"
  e <- expr
  return (p, e)

-- Pattern parser
pattern :: Parser Pattern
pattern = try patternCons
      <|> patternTerm

patternCons :: Parser Pattern
patternCons = do
  ps <- sepBy1 patternTerm (reservedOp ":")
  return $ foldr1 PListCons ps

patternTerm :: Parser Pattern
patternTerm = parens pattern
          <|> wildcardPattern
          <|> varPattern
          <|> intPattern
          <|> boolPattern
          <|> nilPattern

wildcardPattern :: Parser Pattern
wildcardPattern = reservedOp "_" >> return PAny

varPattern :: Parser Pattern
varPattern = do
  x <- identifier
  return $ PVar x

intPattern :: Parser Pattern
intPattern = do
  n <- integer
  return $ PLitInt (fromIntegral n)

boolPattern :: Parser Pattern
boolPattern = (reserved "True" >> return (PLitBool True))
          <|> (reserved "False" >> return (PLitBool False))

nilPattern :: Parser Pattern
nilPattern = reserved "Nil" >> return PLitNil

-- Statement parser
stmt :: Parser Stmt
stmt = try funDef <|> typeSig

funDef :: Parser Stmt
funDef = do
  name <- identifier
  args <- many identifier
  symbol "="
  body <- expr
  return $ SFunDef name args body

typeSig :: Parser Stmt
typeSig = do
  name <- identifier
  symbol "::"
  ty <- typeParser
  return $ STypeSig name ty

-- Type parser
typeParser :: Parser Type
typeParser = buildExpressionParser typeOperators typeTerm

typeOperators :: OperatorTable String () Identity Type
typeOperators = [[Infix (reservedOp "->" >> return TFun) AssocRight]]

typeTerm :: Parser Type
typeTerm = parens typeParser
       <|> (reserved "Int" >> return TInt)
       <|> (reserved "Bool" >> return TBool)
       <|> (brackets (reserved "Int") >> return TIntList)
       <|> (brackets (reserved "Bool") >> return TBoolList)

-- Program parser
prog :: Parser Prog
prog = whiteSpace >> many stmt

-- Parsing function
parseProg :: String -> Either ParseError Prog
parseProg input = parse prog "<stdin>" input


test = do
  let input = "f x = x + 1\nf :: Int -> Int"
  case parseProg input of
    Left err -> print err
    Right progAST -> print progAST
-}