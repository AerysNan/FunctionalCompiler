module Main where

import AST
import Parser
import EvalType
import EvalValue
import System.IO
import Data.Map (insert, union, empty)
import Data.List (dropWhile, dropWhileEnd)
import Debug.Trace
import Data.List.Split (wordsBy)
import Control.Monad

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  repl (ADTContext [] empty) (TypeContext empty empty) empty

repl :: ADTContext -> TypeContext -> ValueContext -> IO ()
repl adtCtx tCtx vCtx = do
  putStr "> "
  inputString <- getLine
  case trim inputString of
    ":q" -> return ()
    ":{" -> do
      block <- readBlock ""
      deal block adtCtx tCtx vCtx
    _ -> let words = wordsBy (== ' ') inputString in
      if length words == 2 && head words == ":l"
        then do
          content <- readFile $ last words
          deal content adtCtx tCtx vCtx
        else
          deal inputString adtCtx tCtx vCtx

readBlock :: String -> IO String
readBlock acc = do
  putStr "| "
  line <- getLine
  if trim line == ":}"
    then return acc
    else readBlock $ acc ++ line


trim :: String -> String
trim = dropWhileEnd (' ' ==) . dropWhile (' ' ==)

deal :: String -> ADTContext -> TypeContext -> ValueContext -> IO ()
deal inputString adtCtx tCtx vCtx =
  let (inputStmts, newCtx) = inputParser adtCtx inputString in
    case inputStmts of
      (Left errorMsg) -> do
        putStrLn errorMsg
        repl newCtx tCtx vCtx
      (Right [stmt]) -> do
        (newTCtx, newVCtx) <- dealStatement stmt True tCtx vCtx
        repl newCtx newTCtx newVCtx
      (Right stmts) -> do
        (newTCtx, newVCtx) <- dealStatements stmts tCtx vCtx
        repl newCtx newTCtx newVCtx

dealStatements :: [Statement] -> TypeContext -> ValueContext -> IO (TypeContext, ValueContext)
dealStatements [] tCtx vCtx = return (tCtx, vCtx)
dealStatements (stmt : stmts) tCtx vCtx = do
  (newTCtx, newVCtx) <- dealStatement stmt False tCtx vCtx
  dealStatements stmts newTCtx newVCtx

dealStatement :: Statement -> Bool -> TypeContext -> ValueContext -> IO (TypeContext, ValueContext)
dealStatement statement verbose tCtx vCtx =
  case statement of
    (Single expr) ->
      case eval expr tCtx vCtx of
        Nothing -> do
          putStrLn "Invalid expression!"
          return (tCtx, vCtx)
        (Just (t, v)) -> do
          when verbose $ printValue v
          return (tCtx, vCtx)
    (Assign name expr) ->
      case eval expr tCtx vCtx of
        Nothing -> do
          putStrLn "Invalid expression!"
          return (tCtx, vCtx)
        (Just (t, v)) -> do
          when verbose $ printValue v
          return (tCtx {getVars = insert name t $ getVars tCtx}, insert name v vCtx)
    (Class adt) ->
      return (tCtx {getCtors = union (EvalType.getADTCtors [adt]) $ getCtors tCtx}, getADTs [adt] `union` vCtx)

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
  (VChar vc) -> Just $ show vc
  (VAdt name values) -> (++) <$> Just name <*> fmap concat (mapM (fmap ((:) ' ') . toLiteral) values)
  _ -> Nothing

printValue :: Value -> IO ()
printValue v =
  case toLiteral v of
    (Just s) -> putStrLn s
    Nothing -> return ()