module EvalType where
import AST
import Data.Map
import Control.Monad.State

type TypeContext = Map String Type
type TypeContextState a = StateT TypeContext Maybe a

equatableType = [TInt, TChar, TBool]
comparableType = [TInt, TChar]

typeOf :: Type -> Expr -> TypeContextState Type
typeOf t expr = do
  exprType <- evalExprType expr
  if exprType == t
    then return t
    else lift Nothing

isBool :: Expr -> TypeContextState Type
isBool = typeOf TBool

isInt :: Expr -> TypeContextState Type
isInt = typeOf TInt

isChar :: Expr -> TypeContextState Type
isChar = typeOf TChar

someTypeOf :: [Type] -> Expr -> TypeContextState Type
someTypeOf ts expr = do
  exprType <- evalExprType expr
  if exprType `elem` ts
    then return TBool
    else lift Nothing

isEquatable :: Expr -> TypeContextState Type
isEquatable = someTypeOf equatableType

isComparable :: Expr -> TypeContextState Type
isComparable = someTypeOf comparableType

isOfSameType :: Expr -> Expr -> TypeContextState Type
isOfSameType expr1 expr2 = do
  exprType1 <- evalExprType expr1
  exprType2 <- evalExprType expr2
  if exprType1 == exprType2
    then return TBool
    else lift Nothing

evalPatternType :: Type -> (Pattern, Expr) -> TypeContextState Type
evalPatternType TBool (PBoolLit _, expr) =
  evalExprType expr
evalPatternType TInt (PIntLit _, expr) =
  evalExprType expr
evalPatternType TChar (PCharLit _, expr) =
  evalExprType expr
evalPatternType patternType (PVar patternName, expr) =
  withStateT (insert patternName patternType) (evalExprType expr)
-- TODO: PData String [Pattern]
evalPatternType _ _ = lift Nothing

evalExprType :: Expr -> TypeContextState Type
evalExprType (EBoolLit _) =
  return TBool
evalExprType (ENot expr) =
  isBool expr >> return TBool
evalExprType (EAnd expr1 expr2) =
  isBool expr1 >> isBool expr2 >> return TBool
evalExprType (EOr expr1 expr2) =
  isBool expr1 >> isBool expr2 >> return TBool

evalExprType (EIntLit _) =
  return TInt
evalExprType (EAdd expr1 expr2) =
  isInt expr1 >> isInt expr2 >> return TInt
evalExprType (ESub expr1 expr2) =
  isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EMul expr1 expr2) =
  isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EDiv expr1 expr2) =
  isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EMod expr1 expr2) =
  isInt expr1 >> isInt expr2 >> return TInt
evalExprType (ECharLit _) =
  return TChar

evalExprType (EEq expr1 expr2) =
  isOfSameType expr1 expr2 >> isEquatable expr1 >> return TBool
evalExprType (ENeq expr1 expr2) =
  isOfSameType expr1 expr2 >> isEquatable expr1 >> return TBool

evalExprType (ELt expr1 expr2) =
  isOfSameType expr1 expr2 >> isComparable expr1 >> return TBool
evalExprType (EGt expr1 expr2) =
  isOfSameType expr1 expr2 >> isComparable expr1 >> return TBool
evalExprType (ELe expr1 expr2) =
  isOfSameType expr1 expr2 >> isComparable expr1 >> return TBool
evalExprType (EGe expr1 expr2) =
  isOfSameType expr1 expr2 >> isComparable expr1 >> return TBool

evalExprType (EIf expr1 expr2 expr3) =
  isBool expr1 >> isOfSameType expr2 expr3 >> evalExprType expr2

evalExprType (ELambda (argName, argType) expr) = do
  exprType <- withStateT (insert argName argType) (evalExprType expr)
  return $ TArrow argType exprType

evalExprType (ELet (patternName, patternExpr) expr) = do
  patternType <- evalExprType patternExpr
  withStateT (insert patternName patternType) (evalExprType expr)

evalExprType (ELetRec funcName (argName, argType) (body, bodyType) expr) = do
  exprType <- withStateT (insert argName argType.insert funcName (TArrow argType bodyType)) (evalExprType body)
  if bodyType == exprType
    then withStateT (insert funcName $ TArrow argType bodyType) (evalExprType expr)
    else lift Nothing

evalExprType (EVar s) = do
  ctx <- get
  if member s ctx
    then return $ ctx ! s
    else lift Nothing

evalExprType (EApply expr1 expr2) = do
  exprType1 <- evalExprType expr1
  exprType2 <- evalExprType expr2
  case exprType1 of
    TArrow type1 type2 -> if type1 == exprType2
      then return type2
      else lift Nothing
    _ -> lift Nothing

evalExprType (ECase expr []) =
  lift Nothing
evalExprType (ECase expr [(p, e)]) = do
  exprType <- evalExprType expr
  evalPatternType exprType (p, e)
evalExprType (ECase expr ((p, e): cs)) = do
  exprType <- evalExprType expr
  caseType <- evalPatternType exprType (p, e)
  t <- evalExprType (ECase expr cs)
  if caseType == t
    then return caseType
    else lift Nothing

evalType :: Program -> Maybe Type
evalType (Program adts body)
  = evalStateT (evalExprType body) empty
