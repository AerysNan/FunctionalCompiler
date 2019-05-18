module EvalValue where

import AST
import Data.Map
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VFunc String Expr ValueContext
  | VErr
  -- ... more
  deriving (Show, Eq)

data CmpR = CmpGT | CmpLT | CmpEQ | CmpIV

compareValues :: Value -> Value -> CmpR
compareValues (VInt i1) (VInt i2)
  | i1 > i2 = CmpGT
  | i1 == i2 = CmpEQ
  | i1 < i2 = CmpLT
compareValues (VChar c1) (VChar c2)
  | c1 > c2 = CmpGT
  | c1 == c2 = CmpEQ
  | c1 < c2 = CmpLT
compareValues _ _ = CmpIV

type ValueContext = Map String Value

type ValueContextState a = StateT ValueContext Maybe a

getBool :: Expr -> ValueContextState Bool
getBool expr = do
  exprValue <- evalExprValue expr
  case exprValue of
    VBool b -> return b
    _ -> lift Nothing

getInt :: Expr -> ValueContextState Int
getInt expr = do
  exprValue <- evalExprValue expr
  case exprValue of
    VInt i -> return i
    _ -> lift Nothing

evalPatternValues :: Value -> [(Pattern, Expr)] -> ValueContextState Value
evalPatternValues value [] = return VErr
evalPatternValues value (c: cs) = do
  patternValue <- evalPatternValue value c
  case patternValue of
    VErr -> evalPatternValues value cs
    _ -> return patternValue

evalPatternValue :: Value -> (Pattern, Expr) -> ValueContextState Value
evalPatternValue (VBool vb) (PBoolLit pb, expr) =
  if vb == pb
    then evalExprValue expr
    else return VErr
evalPatternValue (VInt vi) (PIntLit pi, expr) =
  if vi == pi
    then evalExprValue expr
    else return VErr
evalPatternValue (VChar vc) (PCharLit pc, expr) =
  if vc == pc
    then evalExprValue expr
    else return VErr
evalPatternValue value (PVar patternName, expr) =
  case value of
    (VBool _) -> withStateT (insert patternName value) (evalExprValue expr)
    (VInt _) -> withStateT (insert patternName value) (evalExprValue expr)
    (VChar _) -> withStateT (insert patternName value) (evalExprValue expr)
    _ -> return VErr
evalPatternValue _ _ =
  return VErr
-- TODO: PData String [Pattern]

evalExprValue :: Expr -> ValueContextState Value
evalExprValue (EBoolLit b) = return $ VBool b
evalExprValue (ENot expr) = do
  exprValue <- getBool expr
  return (VBool $ not exprValue)
evalExprValue (EAnd expr1 expr2) = do
  exprValue <- getBool expr1
  if exprValue
    then evalExprValue expr2
    else return $ VBool False
evalExprValue (EOr expr1 expr2) = do
  exprValue <- getBool expr1
  if exprValue
    then return $ VBool True
    else evalExprValue expr2

evalExprValue (EIntLit i) = return $ VInt i
evalExprValue (EAdd expr1 expr2) = do
  exprValue1 <- getInt expr1
  exprValue2 <- getInt expr2
  return (VInt $ exprValue1 + exprValue2)
evalExprValue (ESub expr1 expr2) = do
  exprValue1 <- getInt expr1
  exprValue2 <- getInt expr2
  return (VInt $ exprValue1 - exprValue2)
evalExprValue (EMul expr1 expr2) = do
  exprValue1 <- getInt expr1
  exprValue2 <- getInt expr2
  return (VInt $ exprValue1 * exprValue2)
evalExprValue (EDiv expr1 expr2) = do
  exprValue1 <- getInt expr1
  exprValue2 <- getInt expr2
  return (VInt $ div exprValue1 exprValue2)
evalExprValue (EMod expr1 expr2) = do
  exprValue1 <- getInt expr1
  exprValue2 <- getInt expr2
  return (VInt $ mod exprValue1 exprValue2)

evalExprValue (ECharLit c) = return $ VChar c

evalExprValue (EEq expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  return (VBool $ exprValue1 == exprValue2)
evalExprValue (ENeq expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  return (VBool $ exprValue1 /= exprValue2)

evalExprValue (ELt expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case compareValues exprValue1 exprValue2 of
    CmpLT -> return (VBool True)
    CmpIV -> lift Nothing
    _ -> return (VBool False)
evalExprValue (EGt expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case compareValues exprValue1 exprValue2 of
    CmpGT -> return (VBool True)
    CmpIV -> lift Nothing
    _ -> return (VBool False)
evalExprValue (ELe expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case compareValues exprValue1 exprValue2 of
    CmpGT -> return (VBool False)
    CmpIV -> lift Nothing
    _ -> return (VBool True)
evalExprValue (EGe expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case compareValues exprValue1 exprValue2 of
    CmpLT -> return (VBool False)
    CmpIV -> lift Nothing
    _ -> return (VBool True)

evalExprValue (EIf expr1 expr2 expr3) = do
  exprValue <- getBool expr1
  if exprValue
    then evalExprValue expr2
    else evalExprValue expr3

evalExprValue (ELambda (argName, _) expr) =
  gets (VFunc argName expr)

evalExprValue (ELet (patternName, patternExpr) expr) = do
  patternValue <- evalExprValue patternExpr
  ctx <- get
  value <- withStateT (insert patternName patternValue) (evalExprValue expr)
  put ctx
  return value

evalExprValue (ELetRec funcName (argName, _) (body, _) expr) = do
  ctx <- get
  value <- withStateT (insert funcName $ VFunc argName body empty) (evalExprValue expr)
  put ctx
  return value

evalExprValue (EVar s) = do
  ctx <- get
  if member s ctx
    then return $ ctx ! s
    else lift Nothing

evalExprValue (EApply expr1 expr2) = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case exprValue1 of
    VFunc argName body functionContext -> do
      ctx <- get
      value <- withStateT (insert argName exprValue2.union functionContext) (evalExprValue body)
      -- insert and union cannot be reversed
      put ctx
      return value
    _ -> lift Nothing

evalExprValue (ECase expr cases) = do
  exprValue <- evalExprValue expr
  patternValue <- evalPatternValues exprValue cases
  case patternValue of
    VErr -> lift Nothing
    _ -> return patternValue

evalProgramValue :: Program -> Maybe Value
evalProgramValue (Program adts body) =
  evalStateT (evalExprValue body) empty

evalValue :: Program -> Result
evalValue p =
  case evalProgramValue p of
    Just (VBool b) -> RBool b
    Just (VInt i) -> RInt i
    Just (VChar c) -> RChar c
    _ -> RInvalid
