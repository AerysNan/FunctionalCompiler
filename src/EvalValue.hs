module EvalValue where

import AST
import Control.Monad.State

data Value
  = VBool Bool
  | VInt Int
  | VChar Char
  -- ... more
  deriving (Show, Eq)

data ValueContext = ValueContext { } deriving (Show, Eq)

type ValueContextState a = StateT ValueContext Maybe a

getBool :: Expr -> ValueContextState Bool
getBool e = do
  ev <- evalExprValue e
  case ev of
    VBool b -> return b
    _ -> lift Nothing

evalExprValue :: Expr -> ValueContextState Value
evalExprValue (EBoolLit b) = return $ VBool b
evalExprValue (ENot e) = getBool e >>= \b -> return (VBool $ not b)
-- ... more
eval _ = undefined

evalProgramValue :: Program -> Maybe Value
evalProgramValue (Program adts body) = evalStateT (evalExprValue body) $ ValueContext { }

evalValue :: Program -> Result
evalValue p = case evalProgramValue p of
  Just (VBool b) -> RBool b
  Just (VInt i) -> RInt i
  Just (VChar c) -> RChar c
  _ -> RInvalid
