module Parser where

import AST
import Data.Map (insert, empty, singleton, union, Map, member, (!), fromList)
import Data.Void
import Data.Char
import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Combinators.Expr
import Text.Megaparsec hiding (State, empty)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as M
import qualified Data.List as L

data ADTContext = ADTContext {
  getVars :: [String],
  getCtors :: Map String (String, [Type])
}

type TParser = ParsecT Void String (State ADTContext)


--消耗所有空白符、换行符
scn :: TParser ()
scn = M.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = M.skipLineComment "//"
    blockCmnt = M.skipBlockComment "/*" "*/"

--不消耗换行符，只消耗空白符和制表符
sc :: TParser ()
sc = M.space (void $ some (char ' ' <|> char '\t')) lineCmnt blockCmnt
  where
    lineCmnt  = M.skipLineComment "//"
    blockCmnt = M.skipBlockComment "/*" "*/"

lexeme :: TParser a -> TParser a
lexeme = M.lexeme sc

symbol :: String -> TParser String
symbol = M.symbol sc

-- | 'parens' parses something between parenthesis.
parens :: TParser a -> TParser a
parens = between (symbol  "(") (symbol ")")

-- |'integerLiteral' parses an integer.
integer :: TParser Integer
integer = lexeme M.decimal

-- 关键字
rword :: String -> TParser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String] -- list of reserved words
rws = ["not","if","then","else","let","in","lambda","case","of","data"]

-- 标识符,首字母小写
identifier :: TParser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> lowerChar <*> many alphaNumChar
    check x = if L.elem x rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- 构造函数名，首字母大写
constructor :: TParser String
constructor = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> upperChar <*> many alphaNumChar
    check x = if L.elem x rws
              then fail $ "keyword " ++ show x ++ " cannot be an constructor"
              else return x
-- 字符
charLiteral :: TParser Char
charLiteral = lexeme $ between (char '\'') (char '\'') M.charLiteral

-- 整数
integerLiteral :: TParser Int
integerLiteral = fromInteger <$> lexeme M.decimal

-- 类型
typeTerm :: TParser Type
typeTerm = parens typeParser
      <|> (TBool <$ rword "Bool")
      <|> (TInt <$ rword "Int")
      <|> (TChar <$ rword "Char")
      <|> adt

adt :: TParser Type
adt = do
  adtName <- constructor
  ctx <- get
  if L.elem adtName (getVars ctx)
    then return (TData adtName)
    else fail $ show adtName ++ "not in scope"


typeParser :: TParser Type
typeParser = makeExprParser typeTerm  typeOp

typeOp :: [[Operator TParser Type]]
typeOp = [[InfixR (TArrow <$ symbol "->")]]

-- 模式
patternParser :: TParser Pattern
patternParser = PIntLit <$> integerLiteral  --整数字面量
          <|> PCharLit <$> charLiteral  --字符字面量
          <|> PBoolLit True <$ rword "True" --"True"
          <|> PBoolLit False <$ rword "False" --"False"
          <|> PVar <$> identifier  -- 变量
          <|> try (parens pADTParser) --有参数的代数数据类型
          <|> do                        --无参数的代数数据类型
                ctorName <- constructor
                return (PData ctorName [])

-- eg: data Maybe = Just Int | Nil
--     data Either = Left Maybe | Right Char
--  case Left (Just 1) of
--       Left (Just 1): 1
--       Left Nil : 2
--       Right 'a' : 3
-- !!!!!!内层的代数数据类型若含有参数要加括号（构造函数左结合，和haskell保持一致）
pADTParser :: TParser Pattern
pADTParser = do
  ctorName <- constructor
  params <- some patternParser
  ctx <- get
  if member ctorName (getCtors ctx)
    then return (PData ctorName params)
    else fail $ show ctorName ++ "not in scope"


-- 代数类型
-- TODO
adtParser :: TParser ADT
adtParser = do
  rword "data"
  adtName <- constructor
  void (symbol "=")
  ctx <- get
  put (ctx{ getVars = (L.insert adtName (getVars ctx))})
  fstCtor <- singleCtor adtName
  ctors <- some (symbol "|" *> (singleCtor adtName)) :: TParser [(String, [Type])]
  return (ADT adtName (fstCtor : ctors))

singleCtor :: String -> TParser (String, [Type])
singleCtor adtName = do
  ctorName <- constructor
  paramsType <- many typeTerm :: TParser [Type]
  ctx <- get
  put (ctx{ getCtors = (insert ctorName (adtName, paramsType) (getCtors ctx))})
  return (ctorName, paramsType)

adtsParser :: TParser [ADT]
adtsParser = many (adtParser  <* symbol ";") :: TParser [ADT]

--表达式
exprTerm :: TParser Expr
exprTerm = EIntLit <$> integerLiteral  --整数字面量
      <|> ECharLit <$> charLiteral  --字符字面量
      <|> EBoolLit True <$ rword "True" --"True"
      <|> EBoolLit False <$ rword "False" --"False"
      <|> EVar <$> identifier --变量
      <|> EVar <$> constructor -- 代数数据类型构造函数
      <|> try ifExpr -- if 表达式
      <|> try lambdaExpr -- lambda表达式
      <|> try letExpr    -- let表达式)
      <|> try letRecExpr -- letRec表达式
      <|> try indentedCaseParser -- 有缩进case表达式
      <|> try unindentedCaseParser --无缩进case表达式
      <|> try (parens exprParser)   -- (算术/逻辑/关系运算)，加括号



exprParser :: TParser Expr
exprParser = makeExprParser exprTerm exprOp

exprOp :: [[Operator TParser Expr]]
exprOp =
  [ [InfixL (EApply <$ symbol "")]
  , [InfixL (EMul <$ symbol "*")
    , InfixL (EDiv <$ symbol "/")
    , InfixL (EMod <$ symbol "%")]
  , [InfixL (EAdd <$ symbol "+")
    , InfixL (ESub <$ symbol "-")]
  , [InfixL (EEq <$ symbol "==")
    , InfixL (ENeq <$ symbol "!=")
    , InfixL (ELt <$ symbol "<")
    , InfixL (EGt <$ symbol ">")
    , InfixL (ELe <$ symbol "<=")
    , InfixL (EGe <$ symbol ">=")]
  , [Prefix (ENot <$ rword "not")]
  , [InfixL (EAnd <$ symbol "&&")
    , InfixL (EOr <$ symbol "||")]
  ]

-- if表达式
-- eg: if x>0 then x else x+1
ifExpr :: TParser Expr
ifExpr = do
  rword "if"
  cond <- exprParser
  rword "then"
  expr1 <- exprParser
  rword "else"
  expr2 <- exprParser
  return (EIf cond expr1 expr2)

--lambda表达式
--eg: lambda x::Int . x+1
lambdaExpr :: TParser Expr
lambdaExpr = do
  rword "lambda"
  var <- identifier
  void (symbol "::")
  ty <- typeParser
  void (symbol ".")
  expr <- exprParser
  return (ELambda (var,ty) expr)

--let表达式
--eg: let n = 1 in n+1
letExpr :: TParser Expr
letExpr = do
  rword "let"
  var <- identifier
  void (symbol "=")
  expr1 <- exprParser
  rword "in"
  expr2 <- exprParser
  return (ELet (var,expr1) expr2)


--letRec表达式
--eg: let f = lambda x::Int . x+1::Int in f+2
letRecExpr :: TParser Expr
letRecExpr = do
    rword "let"
    funcName <- identifier
    void (symbol "=")
    rword "lambda"
    var <- identifier
    void (symbol "::")
    tx <- typeParser
    void (symbol ".")
    expr1 <- exprParser
    void (symbol "::")
    ty <- typeParser
    rword "in"
    expr2 <- exprParser
    return (ELetRec funcName (var,tx) (expr1,ty) expr2)


--函数应用表达式
--eg: lambda x::Bool.x &&False False
applyExpr :: TParser Expr
applyExpr = do
  expr1 <- exprParser
  expr2 <- exprParser
  return (EApply expr1 expr2)


--无缩进的模式匹配
-- case n of 1: n; x: 2-n;
-- 各个case缩进保持一致
unindentedCaseParser :: TParser Expr
unindentedCaseParser = do
  rword "case"
  expr0 <- exprParser
  rword "of"
  pairs <- many (pairParser <* symbol ";" ):: TParser [(Pattern,Expr)]
  return (ECase expr0 pairs)

--有缩进的模式匹配
-- case 2 of
--   1: 1
--   x: 2
-- 各个case缩进保持一致
indentedCaseParser :: TParser Expr -- header and list items
indentedCaseParser = M.nonIndented scn (M.indentBlock scn casePair)
  where
    casePair = do
      rword "case"
      expr0 <- exprParser
      rword "of"
      return (M.IndentSome Nothing (return . (ECase expr0)) pairParser)

pairParser :: TParser (Pattern,Expr)
pairParser = (do
  p <- (pADTParser <|> patternParser)
  void (symbol ":")
  expr <- exprParser
  return (p,expr)) <?> "case pair"


assignParser :: TParser Statement
assignParser = do
  var <- identifier
  void (symbol"=")
  Assign var <$> exprParser

statementParser :: TParser Statement
statementParser = assignParser
  <|> Class <$> adtParser
  <|> Single <$> exprParser


inputParser:: String -> Either String Statement
inputParser inpStr = let ep = evalState (runParserT (sc >> statementParser) "" inpStr) (ADTContext [] empty) in
  case ep of
    Right p -> Right p
    Left msg -> Left (errorBundlePretty msg)




