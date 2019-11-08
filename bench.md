``` {.haskell}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Online.Averages
import Online.Medians
import Options.Generic
import Prelude
import Perf
import qualified Control.Foldl as L
import Perf.Analysis
import Readme.Lhs
import Data.Foldable
import Control.Monad (void)
import Data.Maybe
import qualified Data.Text as Text

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

sumInt :: [Int] -> Int -> Int
sumInt xs n = foldl' (+) 0 (take n xs)

sumDouble :: [Double] -> Int -> Double
sumDouble xs n = foldl' (+) 0 (take n xs)

sumPoly :: (Num b) => [b] -> Int -> b
sumPoly xs n = foldl' (+) 0 (take n xs)

sumSum :: [Double] -> Int -> Double
sumSum xs n = L.fold L.sum (take n xs)

sumInt' :: Int -> Int
sumInt' x = foldl' (+) zero [0 .. x]

sumDouble' :: Double -> Double
sumDouble' x = foldl' (+) 0 [1 .. x]

sumPoly' :: (Enum b, Num b) => b -> b
sumPoly' x = foldl' (+) 0 [1 .. x]

avTestMain :: [Double] -> Int -> Double
avTestMain xs n = L.fold Main.av (take n xs)

av :: (Fractional a) => L.Fold a a
av = L.Fold step begin extract
  where
    begin = (0,0)
    step (s, c) a = (s + a, c + 1)
    extract (s, c) = s / c
{-# INLINABLE av #-}

maTest :: [Double] -> Int -> Double
maTest xs n = L.fold (ma 0.99) (take n xs)

fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 [1 .. x]

fPoly :: (Enum b, Num b, Additive b) => b -> b
fPoly x = foldl' (+) 0 [1 .. x]

main :: IO ()
main = do
  o :: Opts <- getRecord "online performance benchmarking"
  let n = fromMaybe 100 (runs o)
  let a = fromMaybe 1000 (sumTo o)
  let !a' = fromIntegral a :: Double
  let !xs = [zero .. a]
  let !xs' = fromIntegral <$> xs :: [Double]
  _ <- warmup 100

  (rSumInt', _) <- ticks n sumInt' a
  (rSumDouble', _) <- ticks n sumDouble' a'
  (rSumPoly', _) <- ticks n sumPoly' a'
  (rSumInt, _) <- ticks n (sumInt xs) a
  (rSumDouble, _) <- ticks n (sumDouble xs') a
  (rSumPoly, _) <- ticks n (sumPoly xs') a
  (rSumSum, _) <- ticks n (sumSum xs') a
  (rAvTestMain, _) <- ticks n (avTestMain xs') a
  (rMaTest, _) <- ticks n (maTest xs') a
  (rStdTest, _) <- ticks n (\x -> L.fold (std 0.99) $ take x xs') a
  (rMaL1Test, _) <- ticks n (\x -> L.fold (maL1 0 0.01 0.99) $ take x xs') a
  (rabsmaL1Test, _) <- ticks n (\x -> L.fold (absmaL1 0 0.01 0.99) $ take x xs') a

  void $ runOutput
    ("online-bench/bench.lhs", LHS)
    ("bench.md", GitHubMarkdown) $

    output "results" $ Native $
      [ plain (Text.pack $ "runs: " <> show n <> " summing to: " <> show a)
      ] <>
      [formatRuns 3 2
       [ ("sumInt'", rSumInt')
       , ("sumDouble'", rSumDouble')
       , ("sumPoly'", rSumPoly')
       , ("sumInt", rSumInt)
       , ("sumDouble", rSumDouble)
       , ("sumPoly", rSumPoly)
      , ("rSumSum", rSumSum)
      , ("rAvTestMain", rAvTestMain)
      , ("rMaTest", rMaTest)
      , ("rStdTest", rStdTest)
      , ("rMaL1Test", rMaL1Test)
      , ("rabsmaL1Test", rabsmaL1Test)
       ]
      ]
```

results
=======

runs: 100 summing to: 1000

| run          |   first|  second|   third|  average|  median|
|:-------------|-------:|-------:|-------:|--------:|-------:|
| sumInt'      |  7.16e3|  2.30e3|  6.06e3|   2.37e3|  2.28e3|
| sumDouble'   |  7.00e5|  1.83e5|  1.94e5|   8.16e5|  3.49e4|
| sumPoly'     |  3.12e4|  3.05e4|  3.09e4|   6.84e4|  3.18e4|
| sumInt       |  1.69e4|  1.18e4|  1.17e4|   1.17e4|  1.17e4|
| sumDouble    |  2.74e4|  1.18e4|  1.17e4|   1.23e4|  1.22e4|
| sumPoly      |  1.23e4|  1.23e4|  1.23e4|   1.23e4|  1.22e4|
| rSumSum      |  1.24e4|  1.23e4|  1.24e4|   1.23e4|  1.22e4|
| rAvTestMain  |  1.29e4|  1.24e4|  1.24e4|   1.39e4|  1.25e4|
| rMaTest      |  1.91e4|  1.22e4|  1.23e4|   1.26e4|  1.24e4|
| rStdTest     |  3.25e5|  9.10e5|  1.49e5|   2.08e5|  1.11e5|
| rMaL1Test    |  8.24e4|  7.98e4|  3.30e5|   1.31e5|  7.95e4|
| rabsmaL1Test |  6.00e4|  5.96e4|  7.11e4|   1.32e5|  7.06e4|