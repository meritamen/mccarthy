{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Mccarthy.Types (
  LispVal (..),
  showVal,
  EnvCtx,
  LispException (..),
  Func,
  Eval,
  IOEval ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import Control.Exception (Exception)
import Data.IORef
import Control.Monad.Except (ExceptT)
       
data LispVal = Atom Text
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | T
             | Nil
             | PrimitiveFunc Func
             | Fn { params :: [Text], body :: [LispVal], closure :: EnvCtx }

instance Eq LispVal where
  (Atom lhs) == (Atom rhs) = lhs == rhs
  (List lhs) == (List rhs) = lhs == rhs
  T == T = True
  Nil == Nil = True
  Nil == List [] = True
  _ == _ = False

type EnvCtx = IORef (Map Text (IORef LispVal))

type Func = [LispVal] -> Eval LispVal

type Eval = Either LispException

type IOEval = ExceptT LispException IO

instance Show LispVal where
  show = Text.unpack . showVal

showVal :: LispVal -> Text
showVal (Atom atom)              = atom
showVal (List list)              = Text.concat ["(", unwordsList list, ")"]
showVal (DottedList lhead ltail) = Text.concat ["(", unwordsList lhead, " . ", showVal ltail, ")"]
showVal T                        = "t"
showVal Nil                      = "()"
showVal (PrimitiveFunc _)        = "#<primitive>"
showVal Fn{..}                   = Text.concat ["#<lambda (", unwordsList $ Atom <$> params, ") ...>"]

unwordsList :: [LispVal] -> Text
unwordsList =  Text.unwords . (showVal <$>)

data LispException = NumArgs Integer [LispVal]
                   | LengthOfList Text Int
                   | ExpectedList Text
                   | TypeMismatch Text LispVal
                   | BadSpecialForm Text
                   | NotFunction LispVal
                   | UnboundVar Text
                   | Default LispVal
                   | PError String
                   | IOError Text

instance Exception LispException

instance Show LispException where
  show = Text.unpack . showError

showError :: LispException -> Text
showError (IOError txt)          = Text.concat ["Error reading file: ", txt]
showError (NumArgs int args)     = Text.concat ["Error Number Arguments, expected ", Text.pack $ show int, " recieved args: ", unwordsList args]
showError (LengthOfList txt int) = Text.concat ["Error Length of List in ", txt, " length: ", Text.pack $ show int]
showError (ExpectedList txt)     = Text.concat ["Error Expected List in funciton ", txt]
showError (TypeMismatch txt val) = Text.concat ["Error Type Mismatch: ", txt, showVal val]
showError (BadSpecialForm txt)   = Text.concat ["Error Bad Special Form: ", txt]
showError (NotFunction val)      = Text.concat ["Error Not a Function: ", showVal val]
showError (UnboundVar txt)       = Text.concat ["Error Unbound Variable: ", txt]
showError (PError str)           = Text.concat ["Parser Error, expression cannot evaluate: ",Text.pack str]
showError (Default val)          = Text.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]
