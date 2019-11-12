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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

fPoly :: (Enum b, Num b) => b -> b
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
    ("other/readme_.md", GitHubMarkdown)
    ("readme.md", GitHubMarkdown) $

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


