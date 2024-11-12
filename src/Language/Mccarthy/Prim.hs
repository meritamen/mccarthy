{-# LANGUAGE OverloadedStrings #-}

module Language.Mccarthy.Prim (primitives) where

import Data.Text (Text)
import Language.Mccarthy.Types (LispVal (..), Func, LispException (..))
import Control.Monad.Except (throwError)

primitives :: [(Text, Func)]
primitives = [("cons", cons),
              ("car", car),
              ("cdr", cdr),
              ("eq", eq),
              ("atom", atom)]

cons :: Func
cons [lhs, Nil]                    = return $ List [lhs]
cons [lhs, List []]                = return $ List [lhs]
cons [lhs, List rhs]               = return $ List $ lhs:rhs
cons [lhs, DottedList lhead ltail] = return $ DottedList (lhs:lhead) ltail
cons [lhs, rhs]                    = return $ DottedList [lhs] rhs
cons args                          = throwError $ NumArgs 2 args

car :: Func
car [List (x:_)]                   = return $ x
car [List []]                      = return $ Nil
car [DottedList (x:_) _]           = return $ x
car [DottedList [] _]              = return $ Nil
car _                              = throwError $ ExpectedList "car"

cdr :: Func
cdr [List (_:xs)]                  = return $ List xs
cdr [List []]                      = return $ Nil
cdr [DottedList [] ltail]          = return $ ltail
cdr [DottedList [x] ltail]         = return $ ltail
cdr [DottedList (_:xs) ltail]      = return $ DottedList xs ltail
cdr [DottedList [] ltail]          = return $ ltail
cdr _                              = throwError $ ExpectedList "cdr"

eq :: Func
eq [Atom l, Atom r]                = return $ if l == r then T else Nil
eq [T, T]                          = return $ T
eq [Nil, Nil]                      = return $ T
eq [Nil, List []]                  = return $ T
eq [List [], Nil]                  = return $ T
eq [_, _]                          = return $ Nil
eq args                            = throwError $ NumArgs 2 args

atom :: Func
atom [Atom _]                      = return $ T
atom [T]                           = return $ T
atom [Nil]                         = return $ T
atom [List []]                     = return $ T
atom [_]                           = return $ Nil
atom args                          = throwError $ NumArgs 1 args
