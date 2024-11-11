{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Mccarthy.Eval (primitiveEnv, evalAndPrint) where

import Language.Mccarthy.Types (
  LispVal (..),
  showVal,
  EnvCtx,
  LispException (..),
  Eval,
  IOEval )
import Language.Mccarthy.Parser (readExpr)
import Language.Mccarthy.Prim (primitives)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except (throwError, catchError, runExceptT)
import Control.Monad (liftM)
import Data.IORef
import Control.Monad.IO.Class (liftIO)

apply :: LispVal -> [LispVal] -> IOEval LispVal
apply (PrimitiveFunc fn) args = liftEval $ fn args

apply Fn{..} args =
  if length params /= length args
  then throwError $ NumArgs (toInteger . length $ params) args
  else do
    env <- liftIO $ bindVars closure $ zip params args
    liftM last $ mapM (eval env) body

evalAndPrint :: EnvCtx -> Text -> IO ()
evalAndPrint env expr = evalText env expr >>= TIO.putStrLn

evalText :: EnvCtx -> Text -> IO Text
evalText env expr = runIOEval $ liftM (Text.pack . show) $ (liftEval $ readExpr expr) >>= eval env

eval :: EnvCtx -> LispVal -> IOEval LispVal

eval _ T = return T

eval _ Nil = return Nil

eval env (Atom var) = getVar env var

eval _ (List [Atom "quote", expr]) = return expr

eval env (List [Atom "setq", Atom var, form]) = do
  result <- eval env form
  setVar env var result

eval env (List [Atom "defvar", Atom var, form]) = do
  result <- eval env form
  defineVar env var result

eval _ (List [Atom "cond"]) = return Nil
eval env (List (Atom "cond" : pairs)) = do
  let currentClause = head pairs
  testResult <- eval env $ conditionalTest currentClause
  if testResult == T
    then eval env $ action currentClause
    else eval env (List (Atom "cond": tail pairs))
  where
    conditionalTest (List [test, _]) = test
    action (List [_, exec]) = exec

eval env (List (Atom "defun" : Atom name : List params : body)) = do
  fn <- mkFn env params body
  defineVar env name fn

eval env (List (Atom "defun" : Atom name : Nil : body)) = do
  fn <- mkFn env [] body
  defineVar env name fn

eval env (List (Atom "lambda" : List params : body)) = mkFn env params body

eval env (List (Atom "lambda" : Nil : body)) = mkFn env [] body

eval env (List (func : args)) = do
  fn <- eval env func
  args' <- mapM (eval env) args
  apply fn args'

eval _ badForm = throwError . BadSpecialForm . Text.pack . show $ badForm

liftEval :: Eval a -> IOEval a
liftEval (Left err) = throwError err
liftEval (Right expr) = return expr

runIOEval :: IOEval Text -> IO Text
runIOEval action = runExceptT (trapError action) >>= return . extract

isBound :: EnvCtx -> Text -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return . maybe False (const True) . Map.lookup var $ env

getVar :: EnvCtx -> Text -> IOEval LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var) (liftIO . readIORef) (Map.lookup var env)

setVar :: EnvCtx -> Text -> LispVal -> IOEval LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar var) (liftIO . (flip writeIORef value)) (Map.lookup var env)
  return value

defineVar :: EnvCtx -> Text -> LispVal -> IOEval LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef (Map.insert var valueRef env)
      return value

bindVars :: EnvCtx -> [(Text, LispVal)] -> IO EnvCtx
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings env = liftM (Map.union env) $ Map.fromList <$> (mapM addBinding bindings)
    addBinding (var, value) = do ref <- newIORef value
                                 return (var, ref)

extract :: Either a b -> b
extract (Right expr) = expr

trapError action = catchError action (return . Text.pack . show)

nullEnv :: IO EnvCtx
nullEnv = newIORef $ Map.fromList []

primitiveEnv :: IO EnvCtx
primitiveEnv = do
  nullEnv' <- nullEnv
  bindVars nullEnv' $ mkPrimitiveFunc <$> primitives
  where mkPrimitiveFunc (var, fn) = (var, PrimitiveFunc fn)

mkFn :: Monad m => EnvCtx -> [LispVal] -> [LispVal] -> m LispVal
mkFn env params body = return $ Fn (showVal <$> params) body env
