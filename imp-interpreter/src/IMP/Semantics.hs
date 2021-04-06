{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module IMP.Semantics where

import           IMP.Exception
import           IMP.Pattern
import           IMP.Syntax

import           Control.Applicative ((<|>))
import           Control.Exception   (throw)
import           Data.Foldable       (toList)
import           Data.Function       ((&))
import qualified Data.IntMap         as M
import           Data.List           (unfoldr)
import           Data.Maybe          (fromJust, isJust)
import qualified Data.Set            as S

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

deindexId :: (Functor f) => IdMap v -> f IdKey -> f v
deindexId idMap = fmap (idMap M.!)

-- -------
-- Config utils
-- -------

-- | index pgm
indexPgm :: (Ord v, Show v) => Pgm v -> (Stmt IdKey, IdMap v)
indexPgm pgm@Pgm{..} = case indexIdWith decls stmt of
  Left v    -> throw $ IEVariableNotDeclared (show v) (show pgm)
  Right res -> res

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

-- | variable lookup
ruleVarLookup :: Rule
ruleVarLookup cfg@Config{..} = cfg & (matchK . stmtAVar $ rho)
  where
    rho (AVar key) = case M.lookup key state of
      Just (KRInt i)  -> Just (ALit i)
      Just (KRBool _) -> throw $ IETypeError (show key) "Int" "Bool" (show cfg)
      Nothing         -> throw $ IEVariableNotInState (show key) (show cfg)
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
  _               -> Nothing

-- ---------------
-- Assignment rule
-- ---------------

-- | only perform assignment if it's head
ruleAssign :: Rule
ruleAssign cfg@Config{..} = case kCell of
  SSeq (SAssign v (ALit n)) s -> Just $ Config (SSeq SUnit s) (update n v state)
  SAssign v (ALit n)          -> Just $ Config SUnit (update n v state)
  _                           -> Nothing
  where
    update n v state =
      if v `M.member` state
        then M.update (const . Just $ KRInt n) v state
        else throw $ IEVariableNotInState (show v) (show cfg)

-- ---------------
-- Sequential composition rule
-- ---------------

-- | sequential composition of two statements, normalized with monoid composition
-- if first statement is reduced to SUnit, forward the state to the second statement
ruleSeqCompose :: Rule
ruleSeqCompose = matchK $ \case
  SSeq SUnit s         -> Just s                       -- left unit
  SSeq (SSeq s1 s2) s3 -> Just $ SSeq s1 (SSeq s2 s3)  -- normalization
  SSeq s SUnit         -> Just s                       -- right unit, unlikely to see
  _            -> Nothing

-- | if-then-else (only the condition is strict)
ruleIte :: Rule
ruleIte = matchK . recursiveMatch . sIte $ \case
  SIte (BLit True)  b _ -> Just $ SBlock b
  SIte (BLit False) _ b -> Just $ SBlock b
  _                     -> Nothing

-- | unrolling while
ruleWhile :: Rule
ruleWhile = matchK . recursiveMatch . sWhile $ \case
  SWhile c b -> Just $ SIte c (Just (SSeq (SBlock b) (SWhile c b))) Nothing
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

-- | succeeds if any rule is applied
applyRules :: [Rule] -> Rule
applyRules rs cfg = foldr ((<|>) . (cfg &)) Nothing rs

-- | iterately apply a rule
iteratively :: Rule -> Config (Stmt IdKey) -> [Maybe (Config (Stmt IdKey))]
iteratively r cfg = iterate (>>= r) (Just cfg)

-- | find closure of a rule application
closureWithin :: Int -> [Maybe (Config (Stmt IdKey))] -> Config (Stmt IdKey)
closureWithin n = fromJust . last . atMost n . takeWhile isJust

-- | restrict evaluation steps
-- configs must be a @Just@ list
atMost :: Int -> [Maybe (Config (Stmt IdKey))] -> [Maybe (Config (Stmt IdKey))]
atMost n = go n n
  where
    go n 0 ((Just cfg):_) = throw $ IEDiverge n (show cfg)
    go n k (mc:mcs)       = mc : go n (k-1) mcs
    go n _ []             = []

closureRulesWithin :: Int -> [Rule] -> Config (Stmt IdKey) -> Config (Stmt IdKey)
closureRulesWithin n rules = closureWithin n . iteratively (applyRules rules)

applyStructualRules :: Config (Stmt IdKey) -> Config (Stmt IdKey)
applyStructualRules = closureRulesWithin 10000 structuralRules

-- | A computation step is a possible application of a computational rule,
-- with potentially many application of structural rules
nextComputationStep :: Rule
nextComputationStep = applyRules computationRules . applyStructualRules

interpretIndexed :: Config (Stmt IdKey) -> Config (Stmt IdKey)
interpretIndexed cfg =
  let cfg' = applyStructualRules . closureWithin 10000 . iteratively nextComputationStep $ cfg
   in if kCell cfg' == SUnit
         then cfg'
         else throw $ IEStuck (show cfg')

initializeConfig :: (Ord v, Show v) => Pgm v -> (Config (Stmt IdKey), IdMap v)
initializeConfig pgm =
  let (indexed, idMap) = indexPgm pgm
      initCfg = Config indexed $ M.map (const $ KRInt 0) idMap
   in (initCfg, idMap)


interpret :: (Ord v, Show v) => Pgm v -> (IdMap v, Config (Pgm v))
interpret pgm =
  let (initCfg, idMap) = initializeConfig pgm
      resCfg = interpretIndexed initCfg
      resPgm = Pgm [] (deindexId idMap $ kCell resCfg)
   in (idMap, Config resPgm (state resCfg))

