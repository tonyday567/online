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
import Formatting
-- import qualified Data.Text as Text
import qualified Control.Foldl as L
import Data.Scientific
import Perf.Analysis

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts


-- | format a real float as a Scientific with a label and precision
formatDouble :: (RealFloat a) => Text -> Int -> a -> Text
formatDouble label p x =
        sformat
          ((right 24 ' ' %. stext) %
           (left 8 ' ' %. prec p)) label (fromFloatDigits x)

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

tick_Test :: FilePath -> IO ()
tick_Test f = do
  onetick <- tick_
  ticks' <- replicateM 10 tick_
  manyticks <- replicateM 1000000 tick_
  let avticks = average manyticks
  let tick999 = percentile 0.999 manyticks
  let tick99999 = percentile 0.99999 manyticks
  let tick99 = percentile 0.99 manyticks
  let tick40 = percentile 0.4 manyticks
  writeFile f $
    code
      [ "one tick_: " <> show onetick
      , "next 10: " <> show ticks'
      , formatDouble "average over 1m: " 2 avticks
      , formatDouble "99.999% perc: " 2 tick99999
      , formatDouble "99.9% perc: " 2 tick999
      , formatDouble "99th perc:  " 2 tick99
      , formatDouble "40th perc:  " 2 tick40
      ]

fMono :: Int -> Int
fMono x = foldl' (+) 0 [1 .. x]

fLambda :: Int -> Int
fLambda = \x -> foldl' (+) 0 [1 .. x]

fPoly :: (Enum b, Num b, Additive b) => b -> b
fPoly x = foldl' (+) 0 [1 .. x]

tickTest :: FilePath -> Int -> IO ()
tickTest f a' = do
  (t, resultPrime) <- tick fMono a'
  print resultPrime
  (t2,_) <- tick fMono a'
  writeFile f $
    code
      [ "sum to " <> show a'
      , "first measure: " <> show t <> " cycles"
      , "second measure: " <> show t2 <> " cycles"
      ]


ticksTest :: FilePath -> Int -> Int -> IO ()
ticksTest f a' n = do
  -- | various versions of tick
  (rpure, _) <- ticks n fMono a'
  (rpurePoly, _) <- ticks n fPoly a'
  (rpureLambda, _) <- ticks n fLambda a'
  (rio, _) <- ticksIO n (pure $ fMono a')
  (rioPoly, _) <- ticksIO n (pure $ fPoly a')
  (rioLambda, _) <- ticksIO n (pure $ fLambda a')

  writeFile f $
    code [ "sum to " <> show a' <> " n = " <> show n
         , formatRunHeader
         , formatRun "ticks" 2 rpure
         , formatRun "ticks lambda" 2 rpureLambda
         , formatRun "ticks (poly)" 2 rpurePoly
         , formatRun "ticks io" 2 rio
         , formatRun "io lambda" 2 rioLambda
         , formatRun "io poly" 2 rioPoly
         ]

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

  writeFile "other/perf.md" $
    code
      [ "sum to " <> sformat commas a
      , formatRunHeader
      , formatRun "rSumInt'" 2 rSumInt'
      , formatRun "rSumDouble'" 2 rSumDouble'
      , formatRun "rSumPoly'" 2 rSumPoly'
      , formatRun "rSumInt" 2 rSumInt
      , formatRun "rSumDouble" 2 rSumDouble
      , formatRun "rSumPoly" 2 rSumPoly
      , formatRun "rSumSum" 2 rSumSum
      , formatRun "rAvTestMain" 2 rAvTestMain
      , formatRun "rMaTest" 2 rMaTest
      , formatRun "rStdTest" 2 rStdTest
      , formatRun "rMaL1Test" 2 rMaL1Test
      , formatRun "rabsmaL1Test" 2 rabsmaL1Test
      ]

  tick_Test "other/tick_.md"
  tickTest "other/tick.md" a
  ticksTest "other/ticks.md" a n
  pure ()
