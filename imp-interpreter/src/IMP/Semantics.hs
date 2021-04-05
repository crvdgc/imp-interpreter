{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module IMP.Semantics where

import           IMP.Exception
import           IMP.Pattern
import           IMP.Syntax

import           Control.Exception (throw)
import           Data.Foldable     (toList)
import           Data.Function     ((&))
import qualified Data.IntMap       as M
import           Data.Maybe        (fromJust)
import qualified Data.Set          as S

data KResult = KRInt  Int
             | KRBool Bool
  deriving (Eq, Show)

type IdKey = Int
type IdMap = M.IntMap
type State = IdMap KResult

data Config k = Config
  { kCell :: k
  , state :: State
  }
  deriving (Eq, Show)

-- -------
-- Id indexing
-- -------

-- | index all occured variables, therefore cannot fail
indexId :: (Ord v, Functor f, Foldable f) => f v -> (f IdKey, IdMap v)
indexId fv = (indexed, idMap)
  where
    idSet = S.fromList . toList $ fv
    idMap = M.fromList . zip [0..] . S.toAscList $ idSet
    indexed = fmap (`S.findIndex` idSet) fv  -- won't fail

-- | index variables with a list of declared variables
-- When find an undelcared variable @v@, error with @Left v@
indexIdWith :: (Ord v, Traversable f) => [v] -> f v -> Either v (f IdKey, IdMap v)
indexIdWith = indexIdFromSet . S.fromList

-- | Similar to @indexIdWith@, but use a set of declared variables
indexIdFromSet :: (Ord v, Traversable f) => S.Set v -> f v -> Either v (f IdKey, IdMap v)
indexIdFromSet idSet fv = (, idMap) <$> indexed
  where
    idMap = M.fromList . zip [0..] . S.toAscList $ idSet
    indexed = mapM toIndex fv
    toIndex v = case S.lookupIndex v idSet of
                  Nothing -> Left v
                  Just ix -> Right ix

deindexId :: (Functor f) => f IdKey -> IdMap v -> f v
deindexId indexed idMap = (idMap M.!) <$> indexed

-- -------
-- Config utils
-- -------

-- | index pgm
indexPgm :: (Ord v, Show v) => Pgm v -> (Stmt IdKey, IdMap v)
indexPgm pgm@Pgm{..} = case indexIdWith decls stmt of
  Left v    -> throw $ IEVariableNotDeclared (show v) (show pgm)
  Right res -> res

-- | initialize a configuration for an indexed pgm
initializeConfig :: Pgm IdKey -> Config (Pgm IdKey)
initializeConfig pgm = Config pgm M.empty

-- | additional pattern for kCell
matchK :: MatchInto' (Config (Stmt v)) (Stmt v)
matchK f Config{..} = flip Config state <$> f kCell

-- -------
-- Rules
-- -------

-- a rule that can be performed on config with an indexed state
-- @Nothing@ means a mismatch
type Rule = Config (Stmt IdKey) -> Maybe (Config (Stmt IdKey))

-- ---------------
-- Variable rules
-- ---------------

-- | initialize declared variables to 0
-- Since indexing already checked variable declarations, we simply discard @decls@
-- When query a missing variable, we use the default value 0
varDecl :: Config (Pgm IdKey) -> Config (Stmt IdKey)
varDecl Config{..} = Config (stmt kCell) state

-- | variable lookup
ruleVarLookup :: Rule
ruleVarLookup cfg@Config{..} = cfg & (matchK . stmtAVar $ rho)
  where
    rho (AVar key) = case M.findWithDefault (KRInt 0) key state of
      KRInt i  -> Just (ALit i)
      KRBool _ -> throw $ IETypeError (show key) "Int" "Bool" (show cfg)
    rho _ = Nothing

-- ---------------
-- Arithmetic rules
-- ---------------

-- | int division
ruleIntDiv :: Rule
ruleIntDiv = matchK . stmtAExp $ \case
  t@(ADiv (ALit _) (ALit 0)) -> throw $ IEDividedByZero (show t)
  (ADiv (ALit i1) (ALit i2)) -> Just $ ALit (i1 `div` i2)
  _                          -> Nothing

-- | int addition
ruleIntAdd :: Rule
ruleIntAdd = matchK . stmtAExp $ \case
  (AAdd (ALit i1) (ALit i2)) -> Just $ ALit (i1 + i2)
  _                          -> Nothing

-- | a negative int literal
ruleIntNeg :: Rule
ruleIntNeg = matchK . stmtAExp . aNeg $ \case
  ANeg i -> Just $ ALit (- i)
  _      -> Nothing

-- ---------------
-- Boolean rules
-- ---------------

-- | boolean less than
ruleBoolLe :: Rule
ruleBoolLe = matchK . stmtBExp $ \case
  BLe (ALit i1) (ALit i2) -> Just $ BLit (i1 <= i2)
  _                       -> Nothing

-- | boolean negation
ruleBoolNeg :: Rule
ruleBoolNeg = matchK . stmtBExp . bNeg $ \case
  BNeg (BLit b) -> Just $ BLit (not b)
  _             -> Nothing

-- | boolean and (strict only on first argument)
ruleBoolAnd :: Rule
ruleBoolAnd = matchK . stmtBExp . bAnd $ \case
  BAnd (BLit True) e  -> Just e
  BAnd (BLit False) _ -> Just $ BLit False
  _                   -> Nothing

-- ---------------
-- Block rule
-- ---------------

ruleBlock :: Rule
ruleBlock = matchK . recursiveMatch $ \case
  SBlock Nothing  -> Just SUnit  -- {}  => .
  SBlock (Just s) -> Just s      -- {S} => S

-- ---------------
-- Assignment rule
-- ---------------

-- | only perform assignment if it's head
ruleAssign :: Rule
ruleAssign cfg@Config{..} = do
  (kCell', state') <- case kCell of
    SSeq (SAssign v (ALit n)) s -> Just (SSeq SUnit s, update n v state)
    _                           -> Nothing
  pure $ Config kCell' state'
  where
    update n v state =
      if v `M.member` state
        then M.update (const . Just $ KRInt n) v state
        else throw $ IEVariableNotInState (show v) (show cfg)

-- ---------------
-- Sequential composition rule
-- ---------------

-- | sequential composition of two statements
-- if first statement is reduced to SUnit, forward the state to the second statement
ruleSeqCompose :: Rule
ruleSeqCompose cfg@Config{..} = do
  kCell' <- case kCell of
    SSeq SUnit s -> Just s
    _            -> Nothing
  pure $ Config kCell' state

-- | if-then-else (only the condition is strict)
ruleIte :: Rule
ruleIte = matchK . recursiveMatch . sIte $ \case
  SIte (BLit True)  b _ -> Just $ SBlock b
  SIte (BLit False) _ b -> Just $ SBlock b
  _                     -> Nothing

-- | unrolling while
ruleWhile :: Rule
ruleWhile = matchK . recursiveMatch . sWhile $ \case
  SWhile c b -> Just $ SIte c b Nothing
  _          -> Nothing

-- -------
-- Search and apply rules
-- -------

structuralRules :: [Rule]
structuralRules =
  [ ruleBlock
  , ruleSeqCompose
  , ruleWhile
  ]

computationRules :: [Rule]
computationRules =
  [ ruleVarLookup
  -- AExp
  , ruleIntDiv
  , ruleIntAdd
  , ruleIntNeg
  -- BExp
  , ruleBoolLe
  , ruleBoolNeg
  , ruleBoolAnd
  -- Stmt
  , ruleAssign
  , ruleIte
  , ruleWhile
  , ruleVarLookup
  ]
