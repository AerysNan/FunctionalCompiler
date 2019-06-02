module EvalValue where

import AST
import Data.Map (insert, empty, singleton, union, Map, member, (!), fromList)
import Control.Monad.State
import Debug.Trace

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  | VFunc String Expr ValueContext
  | VAdt String [Value]
  | VAdtFunc String [Value] Int
  -- ... more
  deriving (Show, Eq)

type ValueContext = Map String Value

type ValueContextState a = StateT ValueContext Maybe a

------------------------------------------------------------

withVars :: Map String Value -> ValueContextState a -> ValueContextState a
withVars m = withStateT (union m)

------------------------------------------------------------

withVar :: String -> Value -> ValueContextState a -> ValueContextState a
withVar varName varValue = withStateT (insert varName varValue)

------------------------------------------------------------

getADTs :: [ADT] -> Map String Value
getADTs [] = empty
getADTs (ADT adtName ctors : xs) = getADTCtors adtName ctors `union` getADTs xs

getADTCtors :: String -> [(String, [Type])] -> Map String Value
getADTCtors _ [] = empty
getADTCtors adtName ((ctorName, argTypes) : xs) =
  if null argTypes
    then insert ctorName (VAdt ctorName []) $ getADTCtors adtName xs
    else insert ctorName (VAdtFunc ctorName [] (length argTypes)) $ getADTCtors adtName xs

------------------------------------------------------------

getBoolValue :: Expr -> ValueContextState Bool
getBoolValue expr = do
  exprValue <- evalExprValue expr
  case exprValue of
    VBool b -> return b
    _ -> lift Nothing

------------------------------------------------------------

getIntValue :: Expr -> ValueContextState Int
getIntValue expr = do
  exprValue <- evalExprValue expr
  case exprValue of
    VInt i -> return i
    _ -> lift Nothing

------------------------------------------------------------

getCharValue :: Expr -> ValueContextState Char
getCharValue expr = do
  exprValue <- evalExprValue expr
  case exprValue of
    VChar c -> return c
    _ -> lift Nothing

------------------------------------------------------------

evalAnd :: Expr -> Expr -> ValueContextState Value
evalAnd expr1 expr2 = do
  exprValue1 <- getBoolValue expr1
  if exprValue1
    then evalExprValue expr2
    else return $ VBool False

------------------------------------------------------------

evalOr :: Expr -> Expr -> ValueContextState Value
evalOr expr1 expr2 = do
  exprValue1 <- getBoolValue expr1
  if exprValue1
    then return $ VBool True
    else evalExprValue expr2

------------------------------------------------------------

evalArithmeticOp :: Expr -> Expr -> (Int -> Int -> Int) -> ValueContextState Value
evalArithmeticOp expr1 expr2 f = do
  exprValue1 <- getIntValue expr1
  exprValue2 <- getIntValue expr2
  return $ VInt (f exprValue1 exprValue2)

------------------------------------------------------------

evalEq :: Expr -> Expr -> (Value -> Value -> Bool) -> ValueContextState Value
evalEq expr1 expr2 f = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  return $ VBool (f exprValue1 exprValue2)

------------------------------------------------------------

evalOrd :: Expr -> Expr -> (Int -> Int -> Bool) -> (Char -> Char -> Bool) -> ValueContextState Value
evalOrd expr1 expr2 fI fC = do
  exprValue1 <- evalExprValue expr1
  case exprValue1 of
    (VInt i1) -> do
      i2 <- getIntValue expr2
      return $ VBool (fI i1 i2)
    (VChar c1) -> do
      c2 <- getCharValue expr2
      return $ VBool (fC c1 c2)
    _ -> lift Nothing

------------------------------------------------------------

evalIf :: Expr -> Expr -> Expr -> ValueContextState Value
evalIf expr1 expr2 expr3 = do
  exprValue <- getBoolValue expr1
  if exprValue
    then evalExprValue expr2
    else evalExprValue expr3

------------------------------------------------------------

evalLetRec :: String -> String -> Expr -> Expr -> ValueContextState Value
evalLetRec funcName argName body expr = do
  ctx <- get
  value <- withVar funcName (VFunc argName body empty) (evalExprValue expr)
  put ctx
  return value

------------------------------------------------------------

evalLet :: String -> Expr -> Expr -> ValueContextState Value
evalLet patternName patternExpr expr = do
  patternValue <- evalExprValue patternExpr
  ctx <- get
  value <- withVar patternName patternValue (evalExprValue expr)
  put ctx
  return value

------------------------------------------------------------

evalApply :: Expr -> Expr -> ValueContextState Value
evalApply expr1 expr2 = do
  exprValue1 <- evalExprValue expr1
  exprValue2 <- evalExprValue expr2
  case exprValue1 of
    VFunc argName body functionContext -> do
      ctx <- get
      value <- withVars functionContext $ withVar argName exprValue2 (evalExprValue body)
      put ctx
      return value
    VAdtFunc ctorName values count -> if count == 1
        then return $ VAdt ctorName (values ++ [exprValue2])
        else return $ VAdtFunc ctorName (values ++ [exprValue2]) (count - 1)
    _ -> lift Nothing

------------------------------------------------------------

matchPV :: Pattern -> Value -> Maybe (Map String Value)
matchPV (PBoolLit pBool) (VBool vBool) = if pBool == vBool
  then Just empty
  else Nothing
matchPV (PIntLit pInt) (VInt vInt) = if pInt == vInt
  then Just empty
  else Nothing
matchPV (PCharLit pChar) (VChar vChar) = if pChar == vChar
  then Just empty
  else Nothing
matchPV (PData pCtorName patterns) (VAdt vCtorName values) = if pCtorName == vCtorName
  then foldr ((<*>).(<$>) union) (Just empty) $ zipWith matchPV patterns values
  else Nothing
matchPV (PVar s) value = Just $ singleton s value
matchPV _ _ = Nothing

------------------------------------------------------------

evalCase :: Expr -> [(Pattern, Expr)] -> ValueContextState Value
evalCase expr [] = lift Nothing
evalCase expr ((p, e) : xs) = do
  exprValue <- evalExprValue expr
  case matchPV p exprValue of
    Nothing -> evalCase expr xs
    (Just m) -> do
      ctx <- get
      value <- withVars m (evalExprValue e)
      put ctx
      return value
------------------------------------------------------------

evalVar :: String -> ValueContextState Value
evalVar s = do
  ctx <- get
  if member s ctx
    then return $ ctx ! s
    else lift Nothing

------------------------------------------------------------

evalExprValue :: Expr -> ValueContextState Value
evalExprValue (EBoolLit b) = return $ VBool b
evalExprValue (ENot expr) = do
  exprValue <- getBoolValue expr
  return (VBool $ not exprValue)
evalExprValue (EAnd expr1 expr2) = evalAnd expr1 expr2
evalExprValue (EOr expr1 expr2) = evalOr expr1 expr2
evalExprValue (EIntLit i) = return $ VInt i
evalExprValue (EAdd expr1 expr2) = evalArithmeticOp expr1 expr2 (+)
evalExprValue (ESub expr1 expr2) = evalArithmeticOp expr1 expr2 (-)
evalExprValue (EMul expr1 expr2) = evalArithmeticOp expr1 expr2 (*)
evalExprValue (EDiv expr1 expr2) = evalArithmeticOp expr1 expr2 div
evalExprValue (EMod expr1 expr2) = evalArithmeticOp expr1 expr2 mod
evalExprValue (ECharLit c) = return $ VChar c
evalExprValue (EEq expr1 expr2) = evalEq expr1 expr2 (==)
evalExprValue (ENeq expr1 expr2) = evalEq expr1 expr2 (/=)
evalExprValue (ELt expr1 expr2) = evalOrd expr1 expr2 (<) (<)
evalExprValue (EGt expr1 expr2) = evalOrd expr1 expr2 (>) (>)
evalExprValue (ELe expr1 expr2) = evalOrd expr1 expr2 (<=) (<=)
evalExprValue (EGe expr1 expr2) = evalOrd expr1 expr2 (>=) (>=)
evalExprValue (EVar s) = evalVar s
evalExprValue (EIf expr1 expr2 expr3) = evalIf expr1 expr2 expr3
evalExprValue (EApply expr1 expr2) = evalApply expr1 expr2
evalExprValue (ELambda (argName, _) expr) = gets (VFunc argName expr)
evalExprValue (ELet (patternName, patternExpr) expr) = evalLet patternName patternExpr expr
evalExprValue (ELetRec funcName (argName, _) (body, _) expr) = evalLetRec funcName argName body expr
evalExprValue (ECase expr cases) = evalCase expr cases

------------------------------------------------------------

evalSingleStatementValue :: Expr -> ValueContext -> Maybe Value
evalSingleStatementValue expr =
  evalStateT (evalExprValue expr)

------------------------------------------------------------

evalProgramValue :: Program -> Maybe Value
evalProgramValue (Program adts body) =
  evalSingleStatementValue body (getADTs adts)

------------------------------------------------------------

evalValue :: Program -> Result
evalValue p =
  case evalProgramValue p of
    Just (VBool b) -> RBool b
    Just (VInt i) -> RInt i
    Just (VChar c) -> RChar c
    _ -> RInvalid

