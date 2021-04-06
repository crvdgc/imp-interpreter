{-# LANGUAGE LambdaCase #-}
module IMP.Exception where

import           Control.Exception
import           Data.Typeable

type Var = String
type Prg = String
type Trm = String
type Typ = String
type Cfg = String

data IMPException
  = IEVariableNotDeclared Var Prg
  | IEVariableNotInState Var Cfg
  | IETypeError Trm Typ Typ Cfg
  | IEDividedByZero Trm
  | IEStuck Cfg
  | IEDiverge Int Cfg
  deriving (Typeable)

instance Show IMPException where
  show = \case
    IEVariableNotDeclared v prg ->
      "Variable " <> v <> " used, but not declared\nProgram: " <> prg
    IEVariableNotInState v cfg ->
      "Try to read/update variable " <> v <> ", but not found in state\nConfig: " <> cfg
    IETypeError term expected actual cfg ->
      "Term " <> term  <> " type error, expected " <> expected <> ", actual " <> actual <> "\nConfig: " <> cfg
    IEDividedByZero term ->
      "Divide by zero in " <> term
    IEStuck cfg ->
      "No rules to apply, stuck\nConfig: " <> cfg
    IEDiverge n cfg ->
      "Not converge after " <> show n <> " steps, might diverge\nConfig: " <> cfg

instance Exception IMPException


