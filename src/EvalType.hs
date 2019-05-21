module EvalType where
import AST
import Data.Map (insert, empty, singleton, union, Map, member, (!), fromList)
import Control.Monad.State
import Debug.Trace

data TypeContext = TypeContext {
  getVars :: Map String Type,
  getCtors :: Map String (String, [Type])
}
type TypeContextState a = StateT TypeContext Maybe a

------------------------------------------------------------

withVars :: Map String Type -> TypeContextState a -> TypeContextState a
withVars m = withStateT (\ctx -> ctx {
  getVars = union m $ getVars ctx
})

------------------------------------------------------------

withVar :: String -> Type -> TypeContextState a -> TypeContextState a
withVar varName varType = withStateT (\ctx -> ctx {
  getVars = insert varName varType $ getVars ctx
})

------------------------------------------------------------

isBool :: Expr -> TypeContextState Type
isBool expr = do
  exprType <- evalExprType expr
  if exprType == TBool
    then return TBool
    else lift Nothing

------------------------------------------------------------

isInt :: Expr -> TypeContextState Type
isInt expr = do
  exprType <- evalExprType expr
  if exprType == TInt
    then return TInt
    else lift Nothing

------------------------------------------------------------

isSameType :: Expr -> Expr -> TypeContextState Type
isSameType expr1 expr2 = do
  exprType1 <- evalExprType expr1
  exprType2 <- evalExprType expr2
  if exprType1 == exprType2
    then return TBool
    else lift Nothing

------------------------------------------------------------

evalLetRec :: String -> String -> Type -> Expr -> Type -> Expr -> TypeContextState Type
evalLetRec funcName argName argType body bodyType expr = do
  ctx <- get
  exprType <- withVars (fromList [(argName, argType), (funcName, TArrow argType bodyType)]) (evalExprType body)
  put ctx
  if bodyType == exprType
    then do
      ctx <- get
      t <- withVar funcName (TArrow argType bodyType) (evalExprType expr)
      put ctx
      return t
    else lift Nothing

------------------------------------------------------------

evalApply :: Expr -> Expr -> TypeContextState Type
evalApply expr1 expr2 = do
  exprType1 <- evalExprType expr1
  exprType2 <- evalExprType expr2
  case exprType1 of
    TArrow type1 type2 -> if type1 == exprType2
      then return type2
      else lift Nothing
    _ -> lift Nothing

------------------------------------------------------------

getADTTypes :: [ADT] -> Map String Type
getADTTypes [] = empty
getADTTypes (ADT adtName _ : xs) = insert adtName (TData adtName) $ getADTTypes xs

------------------------------------------------------------

getADTCtors :: [ADT] -> Map String (String, [Type])
getADTCtors = foldr (union.flatten) empty where
  flatten (ADT adtName ctors) = fromList $ map (\x -> (fst x, (adtName, snd x))) ctors

------------------------------------------------------------

matchPT :: Map String (String, [Type]) -> Pattern -> Type -> Maybe (Map String Type)
matchPT m (PData ctorName patterns) (TData adtName) =
  if member ctorName m && fst (m ! ctorName) == adtName
    then foldr ((<*>).(<$>) union) (Just empty) $ zipWith (matchPT m) patterns (snd (m ! ctorName))
    else Nothing
matchPT m (PBoolLit _) TBool = Just empty
matchPT m (PIntLit _) TInt = Just empty
matchPT m (PCharLit _) TChar = Just empty
matchPT m (PVar s) t = Just $ singleton s t
matchPT _ _ _ = Nothing

------------------------------------------------------------

evalOneCase :: Type -> (Pattern, Expr) -> TypeContextState Type
evalOneCase patternType (PVar patternName, expr) = do
  ctx <- get
  t <- withVar patternName patternType (evalExprType expr)
  put ctx
  return t
evalOneCase TBool (PBoolLit _, expr) =
  evalExprType expr
evalOneCase TInt (PIntLit _, expr) =
  evalExprType expr
evalOneCase TChar (PCharLit _, expr) =
  evalExprType expr
evalOneCase (TData adtName) (p, expr) = do
  ctx <- get
  let m = matchPT (getCtors ctx) p (TData adtName) in
    case m of
      Nothing -> lift Nothing
      (Just ctxMap) -> withVars ctxMap (evalExprType expr)
evalOneCase _ _ = lift Nothing

------------------------------------------------------------

evalCase :: Type -> [(Pattern, Expr)] -> TypeContextState Type
evalCase _ [] = lift Nothing
evalCase t [(p, e)] = evalOneCase t (p, e)
evalCase t ((p, e): cs) = do
  caseType <- evalOneCase t (p, e)
  exprType <- evalCase t cs
  if caseType == exprType
    then return caseType
    else lift Nothing

------------------------------------------------------------

evalEq :: Expr -> TypeContextState Type
evalEq expr = do
  exprType <- evalExprType expr
  case exprType of
    TBool -> return TBool
    TInt -> return TInt
    TChar -> return TChar
    _ -> lift Nothing

------------------------------------------------------------

evalOrd :: Expr -> TypeContextState Type
evalOrd expr = do
  exprType <- evalExprType expr
  case exprType of
    TInt -> return TInt
    TChar -> return TChar
    _ -> lift Nothing

------------------------------------------------------------

evalLet :: String -> Expr -> Expr ->TypeContextState Type
evalLet patternName patternExpr expr = do
  patternType <- evalExprType patternExpr
  ctx <- get
  exprType <- withVar patternName patternType (evalExprType expr)
  put ctx
  return exprType

------------------------------------------------------------

evalVar :: String -> TypeContextState Type
evalVar s = do
  ctx <- get
  if member s (getVars ctx)
    then return $ getVars ctx ! s
    else if member s (getCtors ctx)
      then return $ let
        generateCurryType (adtName, []) = TData adtName
        generateCurryType (adtName, x : xs) = TArrow x (generateCurryType (adtName, xs))
        in generateCurryType (getCtors ctx ! s)
      else lift Nothing

------------------------------------------------------------

evalCtors :: String -> TypeContextState (String, [Type])
evalCtors s = do
  ctx <- get
  if member s (getCtors ctx)
    then return $ getCtors ctx ! s
    else lift Nothing

------------------------------------------------------------

evalLambda :: String -> Type -> Expr -> TypeContextState Type
evalLambda argName argType expr = do
  ctx <- get
  exprType <- withVar argName argType (evalExprType expr)
  put ctx
  return $ TArrow argType exprType

------------------------------------------------------------

evalExprType :: Expr -> TypeContextState Type
evalExprType (EBoolLit _) = return TBool
evalExprType (ENot expr) = isBool expr >> return TBool
evalExprType (EAnd expr1 expr2) = isBool expr1 >> isBool expr2 >> return TBool
evalExprType (EOr expr1 expr2) = isBool expr1 >> isBool expr2 >> return TBool
evalExprType (EIntLit _) = return TInt
evalExprType (EAdd expr1 expr2) = isInt expr1 >> isInt expr2 >> return TInt
evalExprType (ESub expr1 expr2) = isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EMul expr1 expr2) = isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EDiv expr1 expr2) = isInt expr1 >> isInt expr2 >> return TInt
evalExprType (EMod expr1 expr2) = isInt expr1 >> isInt expr2 >> return TInt
evalExprType (ECharLit _) = return TChar
evalExprType (EEq expr1 expr2) = isSameType expr1 expr2 >> evalEq expr1 >> return TBool
evalExprType (ENeq expr1 expr2) = isSameType expr1 expr2 >> evalEq expr1 >> return TBool
evalExprType (ELt expr1 expr2) = isSameType expr1 expr2 >> evalOrd expr1 >> return TBool
evalExprType (EGt expr1 expr2) = isSameType expr1 expr2 >> evalOrd expr1 >> return TBool
evalExprType (ELe expr1 expr2) = isSameType expr1 expr2 >> evalOrd expr1 >> return TBool
evalExprType (EGe expr1 expr2) = isSameType expr1 expr2 >> evalOrd expr1 >> return TBool
evalExprType (EIf expr1 expr2 expr3) = isBool expr1 >> isSameType expr2 expr3 >> evalExprType expr2
evalExprType (ELambda (argName, argType) expr) = evalLambda argName argType expr
evalExprType (ELet (patternName, patternExpr) expr) = evalLet patternName patternExpr expr
evalExprType (EVar s) = evalVar s
evalExprType (EApply expr1 expr2) = evalApply expr1 expr2
evalExprType (ELetRec funcName (argName, argType) (body, bodyType) expr) = evalLetRec funcName argName argType body bodyType expr
evalExprType (ECase expr patterns) = do
  exprType <- evalExprType expr
  evalCase exprType patterns

------------------------------------------------------------

evalType :: Program -> Maybe Type
evalType (Program adts body) =
  evalStateT (evalExprType body) $ TypeContext empty (getADTCtors adts)
