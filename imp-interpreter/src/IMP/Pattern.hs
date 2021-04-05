{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}
module IMP.Pattern where

import           Control.Applicative ((<|>))
import           Data.Function       ((&))
import           IMP.Syntax

-- -------
-- Meta-patterns (search strategies)
-- -------

-- | Try to match sub-pattern @a@
-- @Nothing@ means match fails, the overall pattern @s@ doesn't change
-- @Just b@ means match succeeds, and transitions to @b@, @s@ transitions to @t@
type MatchInto s t a b = (a -> Maybe b) -> s -> Maybe t
-- | Special case of @MatchInto@, but the sub-pattern @a@ and the overall pattern @s@ doesn't change type
type MatchInto' s a = MatchInto s s a a
-- | Special case of @MatchInto'@, when itself is matched
type MatchSelf a = MatchInto' a a

-- | A local recursive match and substitution
-- That is, the substitution must be
-- 1. local in scope
-- 2. doesn't change the type
-- The whole match will succeed on first match
class RecursiveMatch a where
  recursiveMatch :: MatchSelf a

-- | For binary recursive constructors, succeed on first match of
-- 1. left sub-term
-- 2. right sub-term
-- 3. the term itself
binaryRec :: (a -> Maybe a) -> (a -> a -> a) -> a -> a -> a -> Maybe a
binaryRec f constr a1 a2 a =
  let aL = flip constr a2 <$> f a1
      aR = constr a1 <$> f a2
      aS = f a
   in aL <|> aR <|> aS

-- | For binary general constructors, succeed on first match of
-- 1. left sub-term
-- 2. right sub-term
binaryConstr :: (a -> Maybe a) -> (a -> a -> b) -> a -> a -> Maybe b
binaryConstr f constr a1 a2 =
  let aL = flip constr a2 <$> f a1
      aR = constr a1 <$> f a2
   in aL <|> aR

instance RecursiveMatch (AExp v) where
  recursiveMatch :: MatchSelf (AExp v)
  recursiveMatch f e = case e of
    ADiv e1 e2 -> binaryRec f ADiv e1 e2 e
    AAdd e1 e2 -> binaryRec f AAdd e1 e2 e
    nonRec     -> f nonRec

instance RecursiveMatch (BExp v) where
  recursiveMatch :: MatchSelf (BExp v)
  recursiveMatch f e = case e of
    BNeg e0    -> BNeg <$> f e0 <|> f e
    BAnd e1 e2 -> binaryRec f BAnd e1 e2 e
    nonRec     -> f nonRec

instance RecursiveMatch (Stmt v) where
  recursiveMatch :: MatchSelf (Stmt v)
  recursiveMatch f s = case s of
    SBlock b     -> SBlock <$> block f b
    SIte c b1 b2 -> binaryConstr (block f) (SIte c) b1 b2
    SWhile c b   -> SWhile c <$> block f b
    SSeq s1 s2   -> flip SSeq s2 <$> f s1  -- cannot match through seq

-- -------
-- Basic patterns
-- -------

-- --------------
-- AExp patterns
-- --------------

aLit :: MatchSelf (AExp v)
aLit f e = case e of
  ALit _ -> f e
  _      -> Nothing

aVar :: MatchSelf (AExp v)
aVar f e = case e of
  AVar _ -> f e
  _      -> Nothing

aNeg :: MatchSelf (AExp v)
aNeg f e = case e of
  ANeg _ -> f e
  _      -> Nothing

aDiv :: MatchSelf (AExp v)
aDiv f e = case e of
  ADiv _ _ -> f e
  _        -> Nothing

aAdd :: MatchSelf (AExp v)
aAdd f e = case e of
  AAdd _ _ -> f e
  _        -> Nothing

-- --------------
-- BExp patterns
-- --------------

bLit :: MatchSelf (BExp v)
bLit f e = case e of
  BLit _ -> f e
  _      -> Nothing

bLe :: MatchSelf (BExp v)
bLe f e = case e of
  BLe _ _ -> f e
  _       -> Nothing

bLeArg :: MatchInto' (BExp v) (AExp v)
bLeArg f = \case
  BLe e1 e2 -> binaryConstr f BLe e1 e2
  _         -> Nothing

bNeg :: MatchSelf (BExp v)
bNeg f e = case e of
  BNeg _ -> f e
  _      -> Nothing

bAnd :: MatchSelf (BExp v)
bAnd f e = case e of
  BAnd _ _ -> f e
  _        -> Nothing

-- --------------
-- Block pattern
-- --------------

block :: MatchInto' (Block v) (Stmt v)
block f b = case b of
  Nothing -> Nothing       -- empty block won't match
  Just s  -> Just <$> f s  -- match only when statement matches

-- --------------
-- Stmt patterns
-- --------------

sBlock :: MatchInto' (Stmt v) (Block v)
sBlock f s = case s of
  SBlock b -> SBlock <$> f b
  _        -> Nothing

sAssign :: MatchSelf (Stmt v)
sAssign f s = case s of
  SAssign _ _ -> f s
  _           -> Nothing

sAssignArg :: MatchInto' (Stmt v) (AExp v)
sAssignArg f = \case
  SAssign v e -> SAssign v <$> f e
  _           -> Nothing

sIte :: MatchSelf (Stmt v)
sIte f s = case s of
  SIte {} -> f s
  _       -> Nothing

sIteCond :: MatchInto' (Stmt v) (BExp v)
sIteCond f = \case
  SIte c b1 b2 -> (\c' -> SIte c' b1 b2) <$> f c
  _            -> Nothing

sIteBlock :: MatchInto' (Stmt v) (Block v)
sIteBlock f = \case
  SIte c b1 b2 -> binaryConstr f (SIte c) b1 b2
  _            -> Nothing

sWhile :: MatchSelf (Stmt v)
sWhile f s = case s of
  SWhile _ _ -> f s
  _          -> Nothing

sWhileCond :: MatchInto' (Stmt v) (BExp v)
sWhileCond f = \case
  SWhile c b -> flip SWhile b <$> f c
  _          -> Nothing

sWhileBlock :: MatchInto' (Stmt v) (Block v)
sWhileBlock f = \case
  SWhile c b -> SWhile c <$> f b
  _          -> Nothing

sSeq :: MatchSelf (Stmt v)
sSeq f s = case s of
  SSeq _ _ -> f s
  _        -> Nothing

-- -------
-- Composed patterns
-- -------

-- | succeeds if any pattern succeeds
possibly :: [MatchInto' s a] -> MatchInto' s a
possibly patterns f s = foldr ((<|>) . (\p -> s & p f)) Nothing patterns

aExpAVar :: MatchInto' (AExp v) (AExp v)
aExpAVar = recursiveMatch . aVar

bExpAExp :: MatchInto' (BExp v) (AExp v)
bExpAExp = recursiveMatch . bLeArg

bExpAVar :: MatchInto' (BExp v) (AExp v)
bExpAVar = bExpAExp . aExpAVar

stmtBExp :: MatchInto' (Stmt v) (BExp v)
stmtBExp = recursiveMatch . possibly
  [ sIteCond
  , sWhileCond
  ]

stmtAExp :: MatchInto' (Stmt v) (AExp v)
stmtAExp = recursiveMatch . possibly
  [ sAssignArg
  , stmtBExp . bExpAExp
  ]

stmtAVar :: MatchInto' (Stmt v) (AExp v)
stmtAVar = stmtAExp . aExpAVar

stmtBlock :: MatchInto' (Stmt v) (Block v)
stmtBlock = recursiveMatch . possibly
  [ sBlock
  , sIteBlock
  , sWhileBlock
  ]

