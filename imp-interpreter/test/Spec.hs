{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
import           IMP.Pattern
import           IMP.Semantics
import           IMP.Syntax

import           Data.Bifunctor         (first)
import qualified Data.IntMap            as M
import qualified Data.Text              as T
import           Data.Tuple             (swap)
import           Numeric.Natural

import           GHC.Generics
import           Test.Files             (collatzPgm, primesPgm, sumPgm)
import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck  as SC

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ -- properties
    unitTests
  , functionalTests
  ]

properties :: TestTree
properties = testGroup "Properties" [scProps]

scProps = testGroup "(checked by SmallCheck)"
  [ SC.testProperty "index should not change identifiers (aexp)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . swap . indexId) (e :: AExp Int) == e
  , SC.testProperty "index should not change identifiers (bexp)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . swap . indexId) (e :: BExp Natural) == e
  , SC.testProperty "index should not change identifiers (stmt)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . swap . indexId) (e :: Stmt Char) == e
  , SC.testProperty "index should not change identifiers (pgm)" . changeDepth (const 3) $
    \e -> (uncurry deindexId . swap . indexId) (e :: Pgm Char) == e
  ]

unitTests = testGroup "Unit tests"
  [ testCase "test ruleSeqCompose" $
      ruleSeqCompose (sc $ SSeq SUnit SUnit) @?= Just (sc SUnit)
  , testCase "test ruleWhile" $
      ruleWhile (sc $ SWhile (BLit True) Nothing) @?= Just (sc $ SIte (BLit True) (Just $ SSeq (SBlock Nothing)
                                                                                               (SWhile (BLit True) Nothing)) Nothing)
  , testCase "test ruleVarLookup" $
      ruleVarLookup (Config (SAssign 1 (AVar 0)) state0)  @?= Just (Config (SAssign 1 (ALit 10)) state0)
  , testCase "test ruleAssign" $
      ruleAssign (Config (SAssign 1 (ALit 10)) state0) @?= Just (Config SUnit state1)
  ]

functionalTests = testGroup "Functional tests"
  [ testCase "test collatz" . assertBool "collatz is consistant with result" $
      collatzPgm `checkAgainst` collatzRes
  , testCase "test prime" . assertBool "prime is consistant with result" $
      primesPgm `checkAgainst` primesRes
  , testCase "test sum" . assertBool "sum is consistant with result" $
      sumPgm `checkAgainst` sumRes
  ]

-- -------
-- Resources
-- -------

testfiles = [collatzPgm, primesPgm, sumPgm]
testConfigs = map (fst . initializeConfig) testfiles

instance Serial m a => Serial m (AExp a)
instance Serial m a => Serial m (BExp a)
instance Serial m a => Serial m (Stmt a)
instance Serial m a => Serial m (Pgm a)

-- | config from a statement
sc :: Stmt v -> Config (Stmt v)
sc s = Config s M.empty

state0 :: State
state0 = M.fromList [(0, KRInt 10), (1, KRInt 0)]
state1 :: State
state1 = M.fromList [(0, KRInt 10), (1, KRInt 10)]

checkId :: Eq v => IdMap v -> IdMap KResult -> [(v, KResult)] -> Bool
checkId idMap state ref =
  let indexRes = M.toList state
      res = map (first (idMap M.!)) indexRes
   in all (`elem` res) ref  -- assert on a subset

checkAgainst :: Pgm T.Text -> [(T.Text, KResult)] -> Bool
checkAgainst pgm ref =
  let (idMap, cfg) = interpret pgm
   in checkId idMap (state cfg) ref

collatzRes :: [(T.Text, KResult)]
collatzRes = [ ("s", KRInt 66)
             , ("m", KRInt 10)
             ]

primesRes :: [(T.Text, KResult)]
primesRes = [ ("s", KRInt 4)
            , ("m", KRInt 10)
            ]

sumRes :: [(T.Text, KResult)]
sumRes = [ ("n", KRInt 0)
         , ("sum", KRInt 5050)
         ]


