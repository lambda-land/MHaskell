module MHaskell.Micro.Parser where
 
import Text.Parsec as P
import qualified Text.Parsec.Language as Lang
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocRight, AssocLeft, AssocNone),
      Operator(Infix) )

-- import Text.Parsec ((<|>),(<?>),eof)
-- import Text.Parsec.Expr
-- import Text.Parsec.Char

-- import Data.Functor.Identity (Identity)

import MHaskell.Micro.Syntax

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (void)
import Control.Applicative (some)


languageDef :: Tok.LanguageDef st
languageDef = Lang.haskellDef
-- languageDef = haskellStyle
--   { Tok.reservedNames = ["Int", "Bool", "if", "then", "else", "let", "in", "match", "with", "True", "False", "Nil"]
--   , Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", ":", "->", "=", "::", "\\", "_"]
--   , Tok.identStart = letter <|> char '_'
--   , Tok.identLetter = alphaNum <|> oneOf "_'"
--   }

reservedOpNames :: [String]
reservedOpNames = Tok.reservedOpNames languageDef

reservedNames :: [String]
reservedNames = Tok.reservedNames languageDef


lexer :: Tok.TokenParser st
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

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

int :: Parser Int
int = fromIntegral <$> integer

choices :: [Parser a] -> Parser a
choices ps = lexeme $ choices' (map lexeme ps)
  where choices' [] = error "need at least one parser option for 'choices'."
        choices' [p] = p
        choices' (p:ps) = try p <|> choices' ps

parseProgram :: Parser [Stmt]
-- parseProgram = many (try (whiteSpace >> lexeme parseStmt)) <* eof
parseProgram = many (whiteSpace >> lexeme parseStmt)

-- parseProgram = whiteSpace >> manyTill (try $ lexeme parseStmt) (try eof)
-- parseProgram = whiteSpace >> many (try (try whiteSpace >> parseStmt))

-- parseProgram :: Parser [Stmt]
-- parseProgram = do
--   whiteSpace


parseType :: Parser Type
parseType = parseTypeAtom `chainr1` arrowType <?> "type"
  where
    arrowType = do 
      reservedOp "->"
      return TFun             -- TFun combines two Type values into a function type

parseTypeAtom :: Parser Type
parseTypeAtom = 
      (reserved "Int"  >> return TInt)
  <|> (reserved "Bool" >> return TBool)
  <|> parens parseType

parseBinOpSem :: BinOp -> Parser (Expr -> Expr -> Expr)
parseBinOpSem bo = do
  reservedOp (binOpPP bo) <?> ("binary op '" ++ (binOpPP bo) ++ "'")
  return $ \x y -> EBinOp x bo y

parseBinOp :: Parser BinOp
parseBinOp = choices 
  [ operator "+"  >> return Add
  , operator "-"  >> return Sub
  , operator "*"  >> return Mul
  , operator "/"  >> return Div
  , operator "&&" >> return And
  , operator "||" >> return Or
  , operator "==" >> return Eq
  -- ... other operators as needed
  ] <?> "operator"

-- 5. Infix binary operations (left-associative)
parseOpChain :: Parser Expr
parseOpChain = parseExpr `chainl1` opParser
  where opParser = do
          op <- parseBinOp    -- parse a BinOp
          return (\e1 e2 -> EBinOp e1 op e2)

-- Forward declarations to allow mutual recursion if needed
parseExpr :: Parser Expr
-- parseExpr = choices [parseExprAtom,parseIf, parseLambda, parseLet, parseOpChain,parseExprAtom]
-- parseExpr = choices 
--   [ parseOpChain <?> "binary operation"
--   , parseExprAtom <?> "atom"
--   ]
parseExpr = buildExpressionParser opTable (parseTerm') <?> "expression"
  where parseTerm' :: Parser Expr
        parseTerm' = choices
          [ parens parseExpr, parseApp, parseTerm, parseExprAtom ]

parseTerm :: Parser Expr
parseTerm = choices
  [ parseIf          <?> "if condition"
  , parseLambda      <?> "lambda abstraction"
  , parseLet         <?> "let expression"
  , parseExprAtom    <?> "expression atom"
  ]

-- 1. Atomic expressions and function application
parseExprAtom :: Parser Expr
parseExprAtom = choices
  [ EInt <$> int                    <?> "literal integer"
  , EBool True  <$ reserved "True"  <?> "literal boolean True"
  , EBool False <$ reserved "False" <?> "literal boolean False"
  , EVar <$> identifier             <?> "variable"
  ] <?> "atomic expression"

parseApp :: Parser Expr
parseApp = do
  es <- many1 parseTerm
  return $ foldl1 EApp es

-- parseApp = do 
--   atoms <- some parseExprAtom    -- one or more atomic expressions
--   -- If more than one, fold left into function application nodes
--   return $ foldl1 EApp atoms




-- Define helper to build an infix operator parser


-- Operator precedence table (list of lists). Higher in the list = higher precedence.

-- opTable :: 
opTable = [ [ infixOp Mul AssocLeft                -- multiplication (if included)
            , infixOp Div AssocLeft ]              -- division (if included)
          , [ infixOp Add AssocLeft
            , infixOp Sub AssocLeft ]
          , [ infixOp Eq  AssocNone ]              -- equality, non-associative
          , [ infixOp And AssocRight ]
          , [ infixOp Or  AssocRight ]
          ]
  where infixOp bo assoc = Infix (parseBinOpSem bo) assoc

-- 2. If-then-else expression
parseIf :: Parser Expr
parseIf = do
  reserved "if"
  cond <- parseExpr <?> "condition"
  reserved "then"
  tru <- parseExpr <?> "expression after then"
  reserved "else"
  fls <- parseExpr <?> "expression after else"
  return $ EIf cond tru fls

-- 3. Lambda expression
parseLambda :: Parser Expr
parseLambda = do
  symbol "\\"                      -- lambda starts with backslash
  var <- identifier <?> "variable"
  reservedOp "->"
  body <- parseExpr <?> "lambda body"
  return $ EFun var body

-- 4. (Optional) Let-in expression (if included in language)
parseLet :: Parser Expr
parseLet = do
  reserved "let"
  var <- identifier <?> "variable"
  reservedOp "="
  val <- parseExpr <?> "definition body"
  reserved "in"
  body <- parseExpr <?> "in expression"
  return $ ELet var val body


-- parseOpChain = parseApp `chainl1` opParser
--   where opParser = do
--           op <- parseBinOp    -- parse a BinOp
--           return (\e1 e2 -> EBinOp e1 op e2)


parsePattern :: Parser Pattern
parsePattern = choices
  [ underscorePattern
  , PInt <$> int      -- literal int pattern
  , PBool True  <$ reserved "True"
  , PBool False <$ reserved "False"
  , PVar <$> identifier
  , parens consPattern
  , parens parsePattern
  ] <?> "pattern"
  where
    underscorePattern = symbol "_" >> return PAny
    consPattern = do
      x <- parsePattern
      reservedOp ":"
      xs <- parsePattern
      return $ PListCons x xs

parseStmt :: Parser Stmt
parseStmt = parseDefinition <?> "statement"

parseDefinition :: Parser Stmt
parseDefinition = do
  fname <- identifier <?> "function name"
  vars <- many (identifier <?> "function argument")
  -- vars <- manyTill (try identifier <?> "function argument") (try $ reservedOp "=")
  _ <- reservedOp "=" <?> "function def '='"                     -- if an '=' follows, it's a definition
  rhs <- (try parseExpr) <?> "right-hand side of '='"
  return $ SFunDef fname vars rhs          -- SDef or similar constructor for definitions

-- parseTypeDefinition :: Parser Stmt

-- parseExprStmt :: Parser Stmt
-- parseExprStmt = SExpr <$> parseExpr   -- wrap an expression in a Stmt constructor


{--
  -- | Parse an atomic expression (no infix operators or application at this stage).
  term :: Parser Expr
  term = choice 
    [ ifExpr
    , letExpr
    , matchExpr
    , boolLit
    , intLit
    , listLit
    , varExpr
    , parenExpr
    ]
    <?> "term"

  -- Parse an if-then-else expression
  ifExpr :: Parser Expr
  ifExpr = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr   <- expr
    reserved "else"
    fl   <- expr
    return (EIf cond tr fl)

  -- Parse a let-in expression
  letExpr :: Parser Expr
  letExpr = do
    reserved "let"
    name <- identifier            -- (assuming simple let binding to a variable)
    reservedOp "="
    val  <- expr
    reserved "in"
    body <- expr
    return (ELet name val body)

  -- Parse a match expression (pattern matching, similar to Haskell's case)
  matchExpr :: Parser Expr
  matchExpr = do
    reserved "match"
    scrut <- expr
    reserved "with"
    -- Case alternatives inside braces, separated by semicolons (or on new lines)
    alts <- braces (semiSep caseAlt) <|> sepBy1 caseAlt (Token.semi lexer)
    return (EMatch scrut alts)

  -- Parse a boolean literal
  boolLit :: Parser Expr
  boolLit = (reserved "True"  >> return (ELit (LBool True)))
        <|> (reserved "False" >> return (ELit (LBool False)))

  -- Parse an integer literal (handles optional sign via Token.integer)
  intLit :: Parser Expr
  intLit = do
    n <- Token.integer lexer   -- returns an Integer (or Int) value
    return (ELit (LInt (fromInteger n)))

  -- Parse a list literal, e.g. [expr1, expr2, ...]
  listLit :: Parser Expr
  listLit = do
    elems <- brackets (commaSep expr)
    return (EList elems)

  -- Parse a variable (identifier)
  varExpr :: Parser Expr
  varExpr = do
    name <- identifier
    return (EVar name)

  -- | Parse a sequence of terms as left-associative function application.
  applyExpr :: Parser Expr
  applyExpr = do
    func <- term
    args <- many term
    return $ foldl EApp func args


  -- Define helper to build an infix operator parser
  infixOp opStr constructor assoc = Infix (reservedOp opStr >> return (\x y -> EBinOp constructor x y)) assoc

  -- Operator precedence table (list of lists). Higher in the list = higher precedence.
  opTable = [ [ infixOp "*"  Mul  AssocLeft                -- multiplication (if included)
              , infixOp "/"  Div  AssocLeft ]              -- division (if included)
            , [ infixOp "+"  Add  AssocLeft
              , infixOp "-"  Sub  AssocLeft ]
            , [ infixOp "==" Eq   AssocNone ]              -- equality, non-associative
            , [ infixOp "&&" And  AssocRight ]
            , [ infixOp "||" Or   AssocRight ]
            ]

  -- | Parse a binary operator and return its BinOp AST constructor.
  binOp :: Parser BinOp
  binOp = choice 
    [ reservedOp "+"  >> return Add
    , reservedOp "-"  >> return Sub
    , reservedOp "*"  >> return Mul    -- if multiplication is supported
    , reservedOp "/"  >> return Div    -- if division is supported
    , reservedOp "==" >> return Eq
    , reservedOp "&&" >> return And
    , reservedOp "||" >> return Or
    , reservedOp ":"  >> return Cons
    ]


  -- | Parse a pattern for case/let or function arguments.
  patternParser :: Parser Pattern
  patternParser = consPattern <?> "pattern"
    where
      -- Parse cons patterns right-associatively
      consPattern = patAtom `chainr1` (reservedOp ":" >> return PCons)
      -- Atomic pattern (no cons)
      patAtom = choice 
        [ reserved "_"    >> return PWild      -- wildcard
        , reserved "True" >> return (PLit (LBool True))
        , reserved "False">> return (PLit (LBool False))
        , do { n <- Token.integer lexer; return (PLit (LInt (fromInteger n))) }
        , listPat
        , parens patternParser
        , patVar
        ]
      -- Parse a list pattern [p1, p2, ...]
      listPat = brackets (commaSep patternParser) >>= \ps -> return (PList ps)
      -- Variable pattern (identifier)
      patVar = identifier >>= \name -> return (PVar name)

  -- | Parse a single case alternative: pattern -> expression
  caseAlt :: Parser CaseAlt
  caseAlt = do
    pat <- patternParser
    reservedOp "->"
    rhs <- expr
    return (pat,rhs)



  -- Parse a parenthesized expression
  parenExpr :: Parser Expr
  parenExpr = parens expr

  -- | Parse a full expression, with operators and application.
  expr :: Parser Expr
  expr = buildExpressionParser opTable applyExpr <?> "expression"


  -- | Parse a type.
  typeParser :: Parser Type
  typeParser = arrowType <?> "type"
    where
      -- Parse function arrow types right-associatively
      arrowType = baseType `chainr1` (reservedOp "->" >> return TFun)
      -- Parse a base type (no arrows)
      baseType = choice 
        [ reserved "Int"  >> return TInt
        , reserved "Bool" >> return TBool
        , listType
        , parens typeParser
        ]
      -- List type [t]
      listType = brackets typeParser >>= \t -> return (TList t)


  -- | Parse a top-level statement (function definition or type signature).
  stmt :: Parser Stmt
  stmt = try parseTypeSig <|> parseFunDef <?> "statement"
    where
      parseTypeSig = do
        name <- identifier
        reservedOp "::"
        ty   <- typeParser
        return (STypeSig name ty)
      parseFunDef = do
        name <- identifier
        args <- many patternParser        -- zero or more patterns for function arguments
        reservedOp "="
        body <- expr
        return (SFunDef name args body)

  -- | Parse a full program as a list of statements.
  program :: Parser Prog
  program = whiteSpace *> programBody <* eof
    where 
      programBody = sepEndBy stmt statementSep
      -- Allow semicolon or newline as a statement separator:
      statementSep = optional (Token.semi lexer) >> many1 newline

--}





-- binOp opf ops assoc = Infix (reservedOp ops >> return (\lhs rhs -> EBinOp lhs opf rhs)) assoc

-- type OpTable a = OperatorTable String () Identity a

-- operatorTable :: OpTable Expr
-- operatorTable = [[binOp Add "+" AssocLeft,binOp Add "+" AssocLeft]
--                 ,[binOp Mul "*" AssocLeft,binOp Div "/" AssocLeft]]
{-
eol :: Parser ()
eol = do
  _ <- char '\n'
  return ()

endOfStmt :: Parser ()
endOfStmt = do
  void endOfLine


intLiteral :: Parser Int
intLiteral = fromIntegral <$> integer <?> "integer literal"

boolLiteral :: Parser Bool
boolLiteral = true <|> false <?> "boolean literal"
  where true  = reserved "True" >> return True
        false = reserved "False" >> return False

nilLiteral :: Parser ()
nilLiteral = reserved "[]" <?> "nil literal"

literalExpr :: Parser Expr
literalExpr = ELitInt <$> intLiteral 
       <|> ELitBool <$> boolLiteral 
       <|> const ELitNil <$> nilLiteral
       <?> "literal expression"

variableExpr :: Parser Expr
variableExpr = EVar <$> identifier <?> "variable expression"

lambdaExpr :: Parser Expr
lambdaExpr = (do
  reservedOp "\\"
  x <- identifier
  reservedOp "->"
  e <- expr
  return $ EFun x e) <?> "lambda expression"

-- atom' :: Parser Expr
-- atom' = variableExpr
--     <|> literalExpr
--     <|> parens atom'
--     <?> "atom'"

-- unused
atom :: Parser Expr
atom = variableExpr
    <|> literalExpr
    <|> parens expr
    <?> "atom"

listExpr :: Parser Expr
listExpr = (do
  es <- brackets $ commaSep expr
  return $ ELitList es) <?> "list expression"

ifExpr :: Parser Expr
ifExpr = (do
  reserved "if"
  e1 <- expr
  reserved "then"
  e2 <- expr
  reserved "else"
  e3 <- expr
  return $ EIf e1 e2 e3) <?> "if expression"

letExpr :: Parser Expr
letExpr = (do
  reserved "let"
  x <- identifier
  reserved "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  return $ ELet x e1 e2) <?> "let expression"

appExpr :: Parser Expr
appExpr = (do
  es <- many1 termExpr
  return $ foldl1 EApp es) <?> "application expression"

{-
-- unused
exprFactor :: Parser Expr
exprFactor = parens expr
          <|> literalExpr
          <|> variableExpr
          <|> lambdaExpr
          <|> listExpr
          <|> letExpr
          <|> ifExpr
          -- <|> matchExpr
          <?> "factor"
-}

termExpr :: Parser Expr
termExpr = parens expr
        -- <|> appExpr
        <|> literalExpr
        <|> variableExpr
        <|> lambdaExpr
        <|> listExpr
        <|> letExpr
        <|> ifExpr
        -- <|> matchExpr
        <?> "term expression"

termExpr' :: Parser Expr
termExpr' = (try appExpr) <|> termExpr

type OpTable a = OperatorTable String () Identity a

trying to integrate buildExpressionParser with appExpr. For some reason, the two won't work together. need to figure out!

-- infixOp :: BinOp -> String -> Assoc -> OpTable Expr
infixOp :: BinOp -> String -> Assoc -> Operator String () Identity Expr
infixOp opf ops assoc = Infix (reservedOp ops >> return (\lhs rhs -> EBinOp lhs opf rhs)) assoc

operatorTable :: OpTable Expr
operatorTable = [[infixOp Mul "*" AssocLeft,infixOp Div "/" AssocLeft]
                ,[infixOp Add "+" AssocLeft,infixOp Add "+" AssocLeft]
                ,[infixOp op (binOpPP op) AssocLeft | op <- [Eq,Ne,Lt,Gt,Le,Ge]]
                ,[infixOp Cons ":" AssocRight]
                ,[infixOp And "&&" AssocLeft]
                ,[infixOp Or "||" AssocLeft]]

expr :: Parser Expr
expr = buildExpressionParser operatorTable termExpr' <?> "expression"

-- expr :: Parser Expr
-- expr = appExpr
--     <|> lambdaExpr
--     <|> (buildExpressionParser operatorTable termExpr <?> "binary operation expression")
--     <?> "expression"
-- exprFactor = parens expr
--           <|> literal
--           <|> variable
--           <|> lambdaExpr
--           <|> listExpr
--           -- <|> letExpr
--           -- <|> ifExpr
--           -- <|> matchExpr

{-
--          EXPERIMENTAL
-- experiment (don't use)
binOpSemP :: Parser (Expr -> Expr -> Expr)
binOpSemP = do
  op <- binOp
  return $ \e1 e2 -> EBinOp e1 op e2

-- experiment (don't use)
binOpSem :: Parser (Parser Expr -> Parser Expr -> Parser Expr)
binOpSem = return binOpEx
  where binOpEx p1 p2 = do
          e1 <- p1
          f <- binOpSemP
          e2 <- p2
          return $ f e1 e2

binOpExpr' :: Parser Expr -> Parser Expr -> Parser Expr
binOpExpr' p1 p2 = do
  e1 <- p1
  op <- binOp
  e2 <- p2
  return $ EBinOp e1 op e2
-}


pBinOp :: BinOp -> Parser BinOp
pBinOp op = reserved (binOpPP op) >> return op <?> ("binary operator " ++ binOpPP op)

binOp :: Parser BinOp
binOp = choice (map pBinOp binOps) <?> "binary operator"
  where binOps = [Add,Mul,Sub,Div,Eq,Ne,Lt,Gt,Le,Ge,Cons,And,Or]

-- Parse binary operator expression
pBOExpr :: BinOp -> Parser Expr -> Parser Expr -> Parser Expr
pBOExpr op p1 p2 = do
  e1 <- p1
  _ <- pBinOp op
  e2 <- p2
  return $ EBinOp e1 op e2

  
opTable :: Int -> Parser Expr
opTable 0 = pBOExpr Or (opTable 1) (opTable 0) <|> opTable 1                                      <?> "operation table 0"
opTable 1 = pBOExpr And (opTable 2) (opTable 1) <|> opTable 2                                     <?> "operation table 1"
opTable 2 = choice [pBOExpr op (opTable 3) (opTable 2) | op <- [Eq,Ne,Lt,Le,Gt,Ge]] <|> opTable 3 <?> "operation table 2"
opTable 3 = pBOExpr Cons (opTable 4) (opTable 3) <|> opTable 4                                    <?> "operation table 3"
opTable 4 = choice [pBOExpr op (opTable 5) (opTable 4) | op <- [Add,Sub]] <|> opTable 5           <?> "operation table 4"
opTable 5 = choice [pBOExpr op (opTable 6) (opTable 5) | op <- [Mul,Div]] <|> opTable 6           <?> "operation table 5"
opTable _ = termExpr                                                                            <?> "operation table"

binOpExpr :: Parser Expr
binOpExpr = opTable 0 <?> "binary operator expression"

-- exprApp :: Parser Expr
-- exprApp = do
--   es <- many1 exprFactor <?> "application expression"
--   -- lookAhead (try (void stmt) <|> try (eof)) -- might need this
--   return $ foldl1 EApp es

-- expr :: Parser Expr
-- expr = binOpExpr
--     <|> exprApp
--     <|> exprFactor
--     <?> "expression"




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

-}



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
    (Left e)  -> print e -- throw src e
    (Right a) -> print a
  where throw src e = do
          case parse p' "" src of
            (Left e')  -> do { print "failed really bad. "; print e'}
            (Right a) -> print a
          print e
        p' = do
          () <- (p >> return ()) <|> return ()
          input <- getInput
          state <- getState
          return (input,state)

{-# NOINLINE parseFileIOProgram #-}
parseFileIOProgram :: Show a => Parser [a] -> String -> IO ()
parseFileIOProgram p f = do
  -- src <- readFile f
  let !src = unsafePerformIO (readFile f)
  case parse p "" src of
    (Left e)  -> print e -- throw src e
    (Right a) -> mapM_ putStrLn $ map show a

exprTests :: [String]
exprTests
  = [ "1"
    , "True"
    , "False"
    , "[]"
    , "[1,2,3]"
    , "if True then 1 else 2"
    , "let x = 1 in x"
    , "let x = 1 in let y = 2 in x + y"
    , "let x = 1 in let y = 2 in let z = 3 in x + y + z"
    , "1 + 2"
    , "1 + 2 * 3"
    , "f x y"
    ]

{-# NOINLINE runExprTests #-}
-- runExprTests :: [Expr]
runExprTests :: IO ()
runExprTests = mapM_ print $ map (parseString parseExpr) exprTests