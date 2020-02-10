{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | online statistics based on a moving average
module Online.AveragesB
  ( foldB,
    maB,
    absmaB,
    sqmaB,
    stdB,
    std'
  ) where

import Prelude
import Numeric.Backprop as B
import qualified Prelude.Backprop as PB
import qualified Control.Foldl as L
import Control.Foldl (Fold(..))

foldB' :: (Backprop a, Backprop b, Reifies s W, Fractional a, Fractional b) => (BVar s a -> BVar s b) -> BVar s b -> BVar s [a] -> BVar s b
foldB' f r xs = divide (PB.foldl' (step' f r) (T2 0 0) xs) where
  step' f r (T2 s c) a = uncurry T2 ((r*) $ s + f a, (r*) $ c + 1)  
  divide (T2 s c) = s / c


online :: (Reifies s W, Fractional b) => (BVar s a -> BVar s b) -> (BVar s b -> BVar s b) -> Fold (BVar s a) (BVar s b)
online f g = Fold step begin extract
  where
    begin = (0, 0)
    step (s, c) a = (g $ s + f a, g $ c + 1)
    extract (s, c) = s / c

ma' :: (Reifies s W, Fractional b) => BVar s b -> Fold (BVar s b) (BVar s b)
ma' r = online id (*r)

sqma' :: (Reifies s W, Fractional b) => BVar s b -> Fold (BVar s b) (BVar s b)
sqma' r = online (\x -> x * x) (*r)

std' r = (\s ss -> sqrt (ss - s ** 2)) <$> ma' r <*> sqma' r

-- coerceVar

foldB :: (Reifies s W) => (BVar s Double -> BVar s Double) -> BVar s Double -> BVar s [Double] -> BVar s Double
foldB f r xs = divide (PB.foldl' (step' f r) (T2 0 0) xs) where
  step' f r (T2 s c) a = uncurry T2 ((r*) $ s + f a, (r*) $ c + 1)  
  divide (T2 s c) = s / c

stdB :: Reifies s W => BVar s Double -> BVar s [Double] -> BVar s Double
stdB r xs = (\ss s -> sqrt (ss - s ** 2)) (foldB id r xs) (foldB (\x -> x * x) r xs)

-- stdB' :: Reifies s W => BVar s Double -> BVar s [Double] -> BVar s Double
-- stdB' r xs = (\(T2 ss s) -> sqrt (ss - s ** 2)) (foldB' (\x -> (T2 x (x*x))) (T2 r r) xs)

maB :: Reifies s W => BVar s Double -> BVar s [Double] -> BVar s Double
maB r = foldB id r

absmaB :: Reifies s W => BVar s Double -> BVar s [Double] -> BVar s Double
absmaB r = foldB abs r

sqmaB :: Reifies s W => BVar s Double -> BVar s [Double] -> BVar s Double
sqmaB r = foldB (\x -> x * x) r

