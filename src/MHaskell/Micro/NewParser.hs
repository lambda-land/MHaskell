module MHaskell.Micro.NewParser where

-- import Text.Parsers as P

import Text.Trifecta

import Control.Applicative

import MHaskell.Micro.Syntax

-- import Text.Parser.Token as PT
import Text.Parser.Token.Style (haskellIdents,emptyIdents)
import Text.Parser.LookAhead (LookAheadParsing(lookAhead))


identifier :: Parser String
identifier = do
  l <- lower <|> char '_'
  r <- many (alphaNum <|> char '_')
  return (l:r)

variable :: Parser Name
variable = do
  x <- identifier
  _ <- many space
  -- manyTill anyChar (lookAhead (noneOf " \n"))
  -- many space
  return x
-- variable = do
--   lookAhead (noneOf "\n")
--   ident haskellIdents

parseInt :: Parser Expr
parseInt = EInt . fromIntegral <$> integer

parseBool :: Parser Expr
parseBool = EBool <$> (True <$ symbol "True" <|> False <$ symbol "False")

parseVar :: Parser Expr
parseVar = EVar <$> variable

parseOp :: Parser BinOp
parseOp = foldr1 (<|>) $ [op <$ symbol (binOpPP op) | op <- allBinOps]

parseOperator :: BinOp -> Parser BinOp
parseOperator op = op <$ symbol (binOpPP op)

parseBinOp :: Parser Expr
parseBinOp = do
  e1 <- parseExpr
  op <- parseOp
  e2 <- parseExpr
  return $ EBinOp e1 op e2

binOpPrec :: [[BinOp]]
binOpPrec = [[Or],[And],[Eq,Ne],[Lt,Gt,Le,Ge],[Add,Sub],[Mul,Div]]

parseOpExprPrec :: Int -> Parser Expr
parseOpExprPrec n | n < length binOpPrec = case binOpPrec !! n of
  [] -> parseAtom
  ops -> pBinOps ops
  where pBinOps ops = foldl1 (<|>) (map pBinOp' ops)
        pBinOp op = do
          e1 <- parseOpExprPrec (n + 1)
          _ <- parseOperator op
          e2 <- parseOpExprPrec n
          return $ EBinOp e1 op e2
        pBinOp' op = try (pBinOp op) <|> parseOpExprPrec (n + 1)
parseOpExprPrec n | n == length binOpPrec = try parseApp <|> parseOpExprPrec (n + 1)
  where parseApp = do 
          e1 <- parseOpExprPrec (n + 1)
          e2 <- parseOpExprPrec n
          return $ EApp e1 e2
parseOpExprPrec _ = parseAtom


parseAbs :: Parser Expr
parseAbs = do
  _ <- symbolic '\\'
  name <- variable
  _ <- symbol "->"
  body <- parseExpr
  return $ EFun name body

parseExpr :: Parser Expr
parseExpr = parseOpExprPrec 0 <|> parseAbs <|> parseAtom


parseAtom :: Parser Expr
parseAtom = parseVar <|> parseInt <|> parseBool <|> parens parseExpr


-- parseString parseExpr mempty "1 * 2 + 1 * 2"
-- parseString parseExpr mempty "f (x + 1)"

parseStmt :: Parser Stmt
parseStmt = do
  name <- variable
  args <- many variable
  _ <- symbol "="
  body <- parseExpr
  return $ SFunDef name args body


parseFile :: Parser a -> String -> IO (Result a)
parseFile p fname = do
  src <- readFile fname
  return $ parseString p mempty src

