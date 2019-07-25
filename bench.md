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
import NumHask.Prelude hiding ((%), fromIntegral)
import Protolude (fromIntegral)
import Perf hiding (zero, Additive)
import qualified Control.Foldl as L
import Perf.Analysis
import Readme.Lhs

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

sumInt :: [Int] -> Int -> Int
sumInt xs n = foldl' (+) zero (take n xs)

sumDouble :: [Double] -> Int -> Double
sumDouble xs n = foldl' (+) zero (take n xs)

sumPoly :: (Additive b) => [b] -> Int -> b
sumPoly xs n = foldl' (+) zero (take n xs)

sumSum :: [Double] -> Int -> Double
sumSum xs n = L.fold L.sum (take n xs)

sumInt' :: Int -> Int
sumInt' x = foldl' (+) zero [zero .. x]

sumDouble' :: Double -> Double
sumDouble' x = foldl' (+) zero [one .. x]

sumPoly' :: (Enum b, Multiplicative b, Additive b) => b -> b
sumPoly' x = foldl' (+) zero [one .. x]

avTestMain :: [Double] -> Int -> Double
avTestMain xs n = L.fold Main.av (take n xs)

av :: (Field a) => L.Fold a a
av = L.Fold step begin extract
  where
    begin = (zero, zero)
    step (s, c) a = (s + a, c + one)
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
      [ plain ("runs: " <> show n <> " summing to: " <> show a)
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
  pure ()
```

results
=======

runs: 100 summing to: 1000

| run          |   first|  second|   third|  average|  median|
|:-------------|-------:|-------:|-------:|--------:|-------:|
| sumInt'      |  2.77e3|  1.61e3|  1.58e3|   1.60e3|  1.60e3|
| sumDouble'   |  1.38e6|  1.98e5|  1.80e5|   1.65e6|  3.31e4|
| sumPoly'     |  3.02e4|  2.98e4|  2.97e4|   4.88e4|  2.99e4|
| sumInt       |  1.68e4|  1.19e4|  1.17e4|   1.18e4|  1.17e4|
| sumDouble    |  2.62e4|  1.18e4|  1.16e4|   1.17e4|  1.16e4|
| sumPoly      |  1.17e4|  1.16e4|  1.16e4|   1.16e4|  1.16e4|
| rSumSum      |  1.16e4|  1.16e4|  1.16e4|   1.16e4|  1.16e4|
| rAvTestMain  |  1.24e4|  1.18e4|  1.18e4|   1.18e4|  1.18e4|
| rMaTest      |  1.25e4|  1.20e4|  1.20e4|   1.20e4|  1.20e4|
| rStdTest     |  2.08e5|  6.18e5|  1.11e5|   1.84e5|  1.11e5|
| rMaL1Test    |  8.27e4|  7.95e4|  3.30e5|   1.33e5|  7.92e4|
| rabsmaL1Test |  5.99e4|  5.90e4|  5.88e4|   1.04e5|  5.93e4|