{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module IMP.Semantics where

import           IMP.Syntax

import           Data.Foldable (toList)
import qualified Data.IntMap   as M
import           Data.Maybe    (fromJust)
import qualified Data.Set      as S

data KResult = KRInt  Int
             | KRBool Bool

type IdKey = Int
type IdMap = M.IntMap
type State = IdMap KResult

data Config k = Config
  { kCell :: k
  , state :: State
  }

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
-- Config transition rules
-- -------

-- | initialize a configuration for an indexed pgm
initializeConfig :: Pgm IdKey -> Config (Pgm IdKey)
initializeConfig pgm = Config pgm M.empty

-- | initialize declared variables to 0
-- Since indexing already checked variable declarations, we simply discard @decls@
-- When query a missing variable, we use the default value 0
ruleVarDecl :: Config (Pgm IdKey) -> Config (Stmt IdKey)
ruleVarDecl Config{..} = Config (stmt kCell) state

