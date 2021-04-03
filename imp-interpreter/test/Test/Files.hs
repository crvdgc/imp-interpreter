{-# LANGUAGE OverloadedStrings #-}
module Test.Files (collatzPgm, primesPgm, sumPgm) where

import           IMP.Syntax

import qualified Data.Text       as T
import           Numeric.Natural

-- -------
-- Programs
-- -------

collatzPgm :: Pgm T.Text
collatzPgm = Pgm
  { decls = ["m", "n", "q", "r", "s"]
  , stmt  = stmts
    [ "m" `assignInt` 10
    , SWhile (BNeg $ "m" `leInt` 2) . Just $ stmts
      [ "n" `assignVar` "m"
      , SAssign "m" $ AVar "m" `AAdd` ANeg 1
      , SWhile (BNeg $ "n" `leInt`1) . Just $ stmts
        [ SAssign "s" $ AVar "s" `AAdd` ALit 1
        , SAssign "q" $ AVar "n" `ADiv` ALit 2
        , SAssign "r" $ intMult 2 "q" `AAdd` ALit 1
        , let ifCl   = AVar "r" `BLe` AVar "n"
              thenCl = Just $ SAssign "n" $ intMult 3 "n" `AAdd` ALit 1
              elseCl = Just $ "n" `assignVar` "q"
           in SIte ifCl thenCl elseCl
        ]
      ]
    ]
  }

primesPgm :: Pgm T.Text
primesPgm = Pgm
  { decls = ["i", "m", "n", "q", "r", "s", "t", "x", "y", "z"]
  , stmt  = stmts
    [ "m" `assignInt` 10
    , "n" `assignInt` 2
    , SWhile ("n" `leVar` "m") . Just $ stmts
      [ "i" `assignInt` 2
      , SAssign "q" $ AVar "n" `ADiv` AVar "i"
      , "t" `assignInt` 1
      , SWhile (("i" `leVar` "q") `BAnd` (ALit 1 `BLe` AVar "t")) . Just $ stmts
        [ "x" `assignVar` "i"
        , "y" `assignVar` "q"
        , "z" `assignInt` 0
        , SWhile (BNeg $ "x" `leInt` 0) . Just $ stmts
          [ SAssign "q" $ AVar "x" `ADiv` ALit 2
          , SAssign "r" $ intMult 2 "q" `AAdd` ALit 1
          , let ifCl   = "r" `leVar` "x"
                thenCl = Just $ SAssign "z" $ AVar "z" `AAdd` AVar "y"
             in SIte ifCl thenCl Nothing
          , "x" `assignVar` "q"
          , SAssign "y" $ intMult 2 "y"
          ]
        , let ifCl   = "n" `leVar` "z"
              thenCl = Just $ "t" `assignInt` 0
              elseCl = Just $ stmts
                         [ SAssign "i" $ AVar "i" `AAdd` ALit 1
                         , SAssign "q" $ AVar "n" `ADiv` AVar "i"
                         ]
           in SIte ifCl thenCl elseCl
        ]
      , let ifCl   = ALit 1 `BLe` AVar "t"
            thenCl = Just $ SAssign "s" $ AVar "s" `AAdd` ALit 1
         in SIte ifCl thenCl Nothing
      , SAssign "n" $ AVar "n" `AAdd` ALit 1
      ]
    ]
  }

sumPgm :: Pgm T.Text
sumPgm = Pgm
  { decls = ["n", "sum"]
  , stmt  = stmts
    [ "n" `assignInt` 100
    , "sum" `assignInt` 0
    , SWhile (BNeg $ "n" `leInt` 0) . Just $ stmts
      [ SAssign "sum" $ AVar "sum" `AAdd` AVar "n"
      , SAssign "n" $ AVar "n" `AAdd` ANeg 1
      ]
    ]
  }

-- -------
-- Utils
-- -------

assignInt :: v -> Natural -> Stmt v
assignInt v n = v `SAssign` ALit n

assignVar :: v -> v -> Stmt v
assignVar v1 v2 = v1 `SAssign` AVar v2

leInt :: v -> Natural -> BExp v
leInt v n = AVar v `BLe` ALit n

leVar :: v -> v -> BExp v
leVar v1 v2 = AVar v1 `BLe` AVar v2

replicateVar :: Int -> v -> [AExp v]
replicateVar n v = replicate n (AVar v)

intMult :: Int -> v -> AExp v
intMult = (foldr1 AAdd .) . replicateVar

-- | Non-empty statement sequence
stmts :: [Stmt v] -> Stmt v
stmts = foldr1 SSeq
