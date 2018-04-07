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
import qualified Data.Text as Text
import qualified Control.Foldl as L
import Data.Scientific
import Data.TDigest

data Opts = Opts
  { runs :: Maybe Int -- <?> "number of runs"
  , sumTo :: Maybe Int -- <?> "sum to this number"
  } deriving (Generic, Show)

instance ParseRecord Opts

-- | compute deciles
--
-- > c5 <- decile 5 <$> ticks n f a
--
deciles :: (Functor f, Foldable f) => Int -> f Cycle -> [Double]
deciles n xs =
  (\x -> fromMaybe 0 $ quantile x (tdigest (fromIntegral <$> xs) :: TDigest 25)) <$>
  ((/ fromIntegral n) . fromIntegral <$> [0 .. n]) :: [Double]

-- | compute a percentile
--
-- > c <- percentile 0.4 . fst <$> ticks n f a
--
percentile :: (Functor f, Foldable f) => Double -> f Cycle -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest (fromIntegral <$> xs) :: TDigest 25)

expt' :: Int -> Format r (Scientific -> r)
expt' x = scifmt Exponent (Just x)

code :: [Text] -> Text
code cs = "\n```\n" <> Text.intercalate "\n" cs <> "\n```\n"

sumInt :: [Int] -> Int -> Int
sumInt xs n = foldl' (+) zero (take n xs)

sumDouble :: [Double] -> Int -> Double
sumDouble xs n = foldl' (+) zero (take n xs)

sumPoly :: (Enum b, MultiplicativeUnital b, Additive b) => [b] -> Int -> b
sumPoly xs n = foldl' (+) zero (take n xs)

sumSum :: [Double] -> Int -> Double
sumSum xs n = L.fold L.sum (take n xs)

sumInt' :: Int -> Int
sumInt' x = foldl' (+) zero [zero .. x]

sumDouble' :: Double -> Double
sumDouble' x = foldl' (+) zero [one .. x]

sumPoly' :: (Enum b, MultiplicativeUnital b, Additive b) => b -> b
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

formatRun :: [Cycle] -> Text -> Text
formatRun cs label =
    sformat
          ((right 24 ' ' %. stext) % stext %
           (left 9 ' ' %. expt' 3) % " cycles")
          label
          (Text.intercalate " " $ sformat (left 7 ' ' %. expt' 3) <$>
           (\x -> scientific (fromIntegral x) 0) <$> take 5 cs)
          (fromFloatDigits $ percentile 0.4 cs)

formatRunHeader :: Text
formatRunHeader =
        sformat
          ((right 24 ' ' %. stext) %
           (left 7 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext) %
           (left 8 ' ' %. stext))
          "run"
          "first"
          "2nd"
          "3rd"
          "4th"
          "5th"
          "40th %"

run :: Functor f => Text -> f ([Cycle], b) -> f Text
run label t = (`formatRun` label) . fst <$> t

tick_Test :: FilePath -> IO ()
tick_Test f = do
  onetick <- tick_
  ticks' <- replicateM 10 tick_
  manyticks <- replicateM 1000000 tick_
  let avticks = average manyticks
  let qticks = deciles 10 manyticks
  let tick999 = percentile 0.999 manyticks
  let tick99999 = percentile 0.99999 manyticks
  let tick99 = percentile 0.99 manyticks
  let tick40 = percentile 0.4 manyticks
  writeFile f $
    code
      [ "one tick_: " <> show onetick <> " cycles"
      , "next 10: " <> show ticks'
      , "average over 1m: " <> sformat (fixed 2) avticks <> " cycles"
      , "99.999% perc: " <> sformat commas (floor tick99999 :: Integer)
      , "99.9% perc: " <> sformat (fixed 2) tick999
      , "99th perc:  " <> sformat (fixed 2) tick99
      , "40th perc:  " <> sformat (fixed 2) tick40
      , "[min, 10th, 20th, .. 90th, max]:"
      , mconcat (sformat (" " % expt' 4) . fromFloatDigits <$> qticks)
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
  rpure <- run "ticks" $ ticks n fMono a'
  rpurePoly <- run "ticks (poly)" $ ticks n fPoly a'
  rpureLambda <- run "ticks (lambda)" $ ticks n fLambda a'
  rio <- run "ticksIO" $ ticksIO n (pure $ fMono a')
  rioPoly <- run "ticksIO (poly)" $ ticksIO n (pure $ fPoly a')
  rioLambda <- run "ticksIO (lambda)" $ ticksIO n (pure $ fLambda a')

  writeFile f $
    code [ "sum to " <> show a' <> " n = " <> show n
         , formatRunHeader
         , rpure
         , rpureLambda
         , rpurePoly
         , rio
         , rioLambda
         , rioPoly
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

  rSumInt' <- run "sumInt [0..]" $ ticks n sumInt' a
  rSumDouble' <- run "sumDouble [0..]" $ ticks n sumDouble' a'
  rSumPoly' <- run "sumPoly [0..]" $ ticks n sumPoly' a'
  rSumInt <- run "sum Int" $ ticks n (sumInt xs) a
  rSumDouble <- run "sum Double" $ ticks n (sumDouble xs') a
  rSumPoly <- run "sum Poly" $ ticks n (sumPoly xs') a
  rSumSum <- run "fold sum" $ ticks n (sumSum xs') a
  rAvTestMain <- run "fold av" $ ticks n (avTestMain xs') a
  rMaTest <- run "fold ma" $ ticks n (maTest xs') a
  rStdTest <- run "fold std" $ ticks n (\x -> L.fold (std 0.99) $ take x xs') a

  rMaL1Test <- run "fold maL1" $ ticks n (\x -> L.fold (maL1 0 0.01 0.99) $ take x xs') a
  rabsmaL1Test <- run "fold absmaL1" $ ticks n (\x -> L.fold (absmaL1 0 0.01 0.99) $ take x xs') a

  writeFile "other/perf.md" $
    code
      [ "sum to " <> sformat commas a
      , formatRunHeader
      , rSumInt'
      , rSumDouble'
      , rSumPoly'
      , rSumInt
      , rSumDouble
      , rSumPoly
      , rSumSum
      , rAvTestMain
      , rMaTest
      , rStdTest
      , rMaL1Test
      , rabsmaL1Test
      ]

  tick_Test "other/tick_.md"
  tickTest "other/tick.md" a
  ticksTest "other/ticks.md" a n
  pure ()
