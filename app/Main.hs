module Main where

import AST
import Parser
import EvalType
import EvalValue
import System.IO
import Data.Map

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
          Nothing -> do
            putStrLn "Invalid expression!"
            repl tCtx vCtx
          (Just expr) ->
            case expr of
              (EAssign var body) ->
                case var of
                  (EVar name) ->
                    case eval body tCtx vCtx of
                      Nothing -> do
                        putStrLn "Invalid expression!"
                        repl tCtx vCtx
                      (Just (t, v)) -> do
                        printValue v
                        repl (tCtx {getVars = insert name t $ getVars tCtx}) (insert name v vCtx)
                  _ -> do
                    putStrLn "Invalid expression!"
                    repl tCtx vCtx
              _ -> do
                case eval expr tCtx vCtx of
                  Nothing -> putStrLn "Invalid expression!"
                  (Just (t, v)) -> printValue v
                repl tCtx vCtx

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

printValue :: Value -> IO ()
printValue v = case v of
  (VBool vb) -> print vb
  (VInt vi) -> print vi
  (VChar vc) -> print vc
  _ -> putStrLn "Result not showable."