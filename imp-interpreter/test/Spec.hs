{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import           IMP.Semantics          (deindexId, indexId)
import           IMP.Syntax
import           Test.Files             (collatzPgm, primesPgm, sumPgm)

import           GHC.Generics
import           Numeric.Natural
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck  as SC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "index should not change identifiers (aexp)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . indexId) (e :: AExp Int) == e
  , SC.testProperty "index should not change identifiers (bexp)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . indexId) (e :: BExp Natural) == e
  , SC.testProperty "index should not change identifiers (stmt)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . indexId) (e :: Stmt Char) == e
  , SC.testProperty "index should not change identifiers (pgm)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . indexId) (e :: Pgm Char) == e
  ]

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

-- -------
-- Resources
-- -------

testfiles = [collatzPgm, primesPgm, sumPgm]

instance Serial m a => Serial m (AExp a)
instance Serial m a => Serial m (BExp a)
instance Serial m a => Serial m (Stmt a)
instance Serial m a => Serial m (Pgm a)

