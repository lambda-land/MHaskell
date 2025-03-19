module MHaskell.Micro.NewParser where

-- import Text.Parsers as P

import Text.Trifecta

import Control.Applicative

import MHaskell.Micro.Syntax

-- import Text.Parser.Token as PT
import Text.Parser.Token.Style (haskellIdents)


parseInt :: Parser Expr
parseInt = EInt . fromIntegral <$> integer

parseBool :: Parser Expr
parseBool = EBool <$> (True <$ symbol "True" <|> False <$ symbol "False")

parseVar :: Parser Expr
parseVar = EVar <$> (ident haskellIdents)

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
parseOpExprPrec _ = parseAtom


parseExpr :: Parser Expr
parseExpr = parseOpExprPrec 0 <|> parseAtom

parseAtom :: Parser Expr
parseAtom = parseVar <|> parseInt <|> parseBool <|> parens parseExpr


-- parseString parseExpr mempty "1 * 2 + 1 * 2"