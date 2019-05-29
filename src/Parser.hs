module Parser where

import AST
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["not", "if", "then", "else", "let", "in", "lambda"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

integerLiteral :: Parser Int
integerLiteral = fromInteger <$> lexeme L.decimal

typeTerm :: Parser Type
typeTerm = parens typeParser
      <|> (TBool <$ rword "Bool")
      <|> (TInt <$ rword "Int")
      <|> (TChar <$ rword "Char")

typeParser :: Parser Type
typeParser = makeExprParser typeTerm typeOp

typeOp :: [[Operator Parser Type]]
typeOp = [[InfixR (TArrow <$ symbol "->")]]

-- TODO: ADT

-- TODO: Pattern

exprTerm :: Parser Expr
exprTerm = EIntLit <$> integerLiteral
      <|> ECharLit <$> charLiteral
      <|> EBoolLit True <$ rword "True"
      <|> EBoolLit False <$ rword "False"
      <|> EVar <$> identifier
      <|> try ifExpr
      <|> try lambdaExpr
      <|> try letExpr
      <|> try letRecExpr
      <|> try (parens exprParser)

exprParser :: Parser Expr
exprParser = makeExprParser exprTerm exprOp

exprOp :: [[Operator Parser Expr]]
exprOp = [
    [
      InfixL (EApply <$ symbol "")
    ],
    [
      InfixL (EMul <$ symbol "*"),
      InfixL (EDiv <$ symbol "/"),
      InfixL (EMod <$ symbol "%")
    ],
    [
      InfixL (EAdd <$ symbol "+"),
      InfixL (ESub <$ symbol "-")
    ],
    [
      InfixL (EEq <$ symbol "=="),
      InfixL (ENeq <$ symbol "!="),
      InfixL (ELt <$ symbol "<"),
      InfixL (EGt <$ symbol ">"),
      InfixL (ELe <$ symbol "<="),
      InfixL (EGe <$ symbol ">=")
    ],
    [
      Prefix (ENot <$ rword "not")
    ],
    [
      InfixL (EAnd <$ symbol "&&"),
      InfixL (EOr <$ symbol "||")
    ]
  ]

-- eg: if x>0 then x else x+1
ifExpr :: Parser Expr
ifExpr = do
  rword "if"
  cond <- exprParser
  rword "then"
  expr <- exprParser
  rword "else"
  EIf cond expr <$> exprParser

--eg: lambda x::Int . (x+1)
lambdaExpr :: Parser Expr
lambdaExpr = do
  rword "lambda"
  var <- identifier
  void (symbol "::")
  ty <- typeParser
  void (symbol ".")
  ELambda (var,ty) <$> exprParser

--eg: let n = 1 in (n+1)
letExpr :: Parser Expr
letExpr = do
  rword "let"
  var <- identifier
  void (symbol "=")
  expr <- exprParser
  rword "in"
  ELet (var, expr) <$> exprParser

--eg: let f = lambda x::tx . (x+1)::ty in (f+2)
letRecExpr :: Parser Expr
letRecExpr = do
    rword "let"
    funcName <- identifier
    void (symbol "=")
    rword "lambda"
    var <- identifier
    void (symbol "::")
    tx <- typeParser
    void (symbol ".")
    body <- exprParser
    void (symbol "::")
    ty <- typeParser
    rword "in"
    ELetRec funcName (var, tx) (body, ty) <$> exprParser

--eg: (lambda x::Bool.(x &&False)) False
applyExpr :: Parser Expr
applyExpr = do
  func <- exprParser
  EApply func <$> exprParser

wholeParser :: Parser Expr
wholeParser = between sc eof exprParser

inputParser:: String -> Maybe Expr
inputParser = parseMaybe (sc >> wholeParser)
