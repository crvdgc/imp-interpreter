{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
module IMP.Syntax
  ( AExp(..)
  , BExp(..)
  , Block
  , Stmt(..)
  , Pgm(..)
  )
  where

import           Numeric.Natural

-- -------
-- Expressions and Statements
-- -------
data AExp v = ALit Natural | AVar v
            | ANeg Natural
            | ADiv (AExp v) (AExp v)
            | AAdd (AExp v) (AExp v)
  deriving (Functor, Foldable, Traversable)

data BExp v = BLit Bool
            | BLe  (AExp v) (AExp v)
            | BNeg (BExp v)
            | BAnd (BExp v) (BExp v)
  deriving (Functor, Foldable, Traversable)

type Block v = Maybe (Stmt v) -- empty block is Nothing

data Stmt v = SBlock  (Block v)
            | SAssign v (AExp v)
            | SIte    (BExp v) (Block v) (Block v) -- If-then-else
            | SWhile  (BExp v) (Block v)
            | SSeq    (Stmt v) (Stmt v)
  deriving (Functor, Foldable, Traversable)

-- -------
-- Programs
-- -------

data Pgm v = Pgm
  { decls :: [v]
  , stmt  :: Stmt v
  }
  deriving (Show, Functor, Foldable, Traversable)

-- -------
-- Debugging Shows
-- -------
instance Show v => Show (AExp v) where
  show = \case
    ALit n -> show n
    AVar v -> show v
    ANeg n -> "-" <> show n
    ADiv e1 e2 -> show e1 <> " / " <> show e2
    AAdd e1 e2 -> show e1 <> " + " <> show e2

instance Show v => Show (BExp v) where
  show = \case
    BLit True  -> "true"
    BLit False -> "false"
    BLe e1 e2  -> show e1 <> " <= " <> show e2
    BNeg e     -> '!' : show e
    BAnd e1 e2 -> show e1 <> " && " <> show e2

instance Show v => Show (Stmt v) where
  show = \case
    SBlock Nothing  -> "{}"
    SBlock (Just s) -> "{\n" <> show s <> "\n}"
    SAssign v e     -> show v <> " = " <> show e <> ";"
    SIte e s1 s2    -> "if (" <> show e <> ")\nthen {\n" <> show s1 <> "}\nelse {\n" <> show s2 <> "}"
    SWhile e s      -> "while (" <> show e <> ")\n{" <> show s <> "}"
    SSeq s1 s2      -> show s1 <> "\n" <> show s2