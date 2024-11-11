module Language.Mccarthy.Repl (mainLoop) where

import Language.Mccarthy.Types (EnvCtx)
import Language.Mccarthy.Eval (evalAndPrint, primitiveEnv)
import System.Console.Haskeline (defaultSettings, getInputLine, outputStrLn, runInputT, InputT)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Text (pack)

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = do
  env <- primitiveEnv
  runInputT defaultSettings $ repl env

repl :: EnvCtx -> Repl ()
repl env = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (evalAndPrint env . pack $ input) >> repl env
