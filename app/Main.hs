module Main where

import AST
import Parser
import EvalType
import EvalValue
import System.IO
import Data.Map
import Debug.Trace

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl (TypeContext empty empty) empty

repl :: TypeContext -> ValueContext -> IO ()
repl tCtx vCtx = do
  putStr "> "
  inputString <- getLine
  if inputString == "quit"
    then return ()
    else
      let inputExpr = inputParser inputString in
        case inputExpr of
          (Left errorMsg) -> do
            putStrLn errorMsg
            repl tCtx vCtx
          (Right (Single expr)) ->
            case eval expr tCtx vCtx of
              Nothing -> do
                putStrLn "Invalid expression!"
                repl tCtx vCtx
              (Just (t, v)) -> do
                printValue v
                repl tCtx vCtx
          (Right (Assign name expr)) ->
            case eval expr tCtx vCtx of
              Nothing -> do
                putStrLn "Invalid expression!"
                repl tCtx vCtx
              (Just (t, v)) -> do
                printValue v
                repl (tCtx {getVars = insert name t $ getVars tCtx}) (insert name v vCtx)
          (Right (Class adt)) ->
            repl (tCtx {getCtors = union (EvalType.getADTCtors [adt]) $ getCtors tCtx}) (union (getADTs [adt]) vCtx)

eval :: Expr -> TypeContext -> ValueContext -> Maybe (Type, Value)
eval expr tCtx vCtx =
  let exprType = evalSingleStatementType expr tCtx in
    case exprType of
      Nothing -> Nothing
      (Just t) ->
        let exprValue = evalSingleStatementValue expr vCtx in
          case exprValue of
            Nothing -> Nothing
            (Just v) -> Just (t, v)

toLiteral :: Value -> Maybe String
toLiteral v = case v of
  (VBool vb) -> Just $ show vb
  (VInt vi) -> Just $ show vi
  (VChar vc) -> Just$ show vc
  (VAdt name values) -> Just $ name ++ concatMap (\s -> ' ' : toLiteral s) values
  _ -> Nothing

printValue :: Value -> IO ()
printValue v =
  case toLiteral v of
    (Just s) -> putStrLn s
    Nothing -> return ()