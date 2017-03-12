{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Online.Histogram where

import Protolude
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import Linear hiding (identity)
import Data.List
import Formatting

-- a histogram
data Histogram = Histogram
   { _cuts   :: [Double] -- bucket boundaries
   , _values :: Map.Map Int Double -- bucket counts
   } deriving (Show, Eq)

freq' :: Map.Map Int Double -> Map.Map Int Double
freq' m = Map.map (* recip (Protolude.sum m)) m

freq :: Histogram -> Histogram
freq (Histogram c v) = Histogram c (freq' v)

count :: L.Fold Int (Map Int Double)
count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity

countW :: L.Fold (Int,Double) (Map Int Double)
countW = L.Fold (\x (a,w) -> Map.insertWith (+) a w x) Map.empty identity

countBool :: L.Fold Bool Int
countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity

histMap :: (Functor f, Functor g, Ord a, Foldable f, Foldable g) =>
    f a -> g a -> Map Int Double
histMap cuts xs = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> xs

histMapW :: (Functor f, Functor g, Ord a, Foldable f, Foldable g) =>
    f a -> g (a,Double) -> Map Int Double
histMapW cuts xs = L.fold countW $
    (\x -> (L.fold countBool (fmap (fst x >) cuts),snd x)) <$> xs

range :: (Fractional t, Ord t, Foldable f) => f t -> (t, t)
range = L.fold (L.Fold step initial extract)
  where
    step Nothing x = Just (x,x)
    step (Just (min,max)) x =
      Just (min' x min, max' x max)
    max' x1 x2 = if x1 >= x2 then x1 else x2
    min' x1 x2 = if x1 <= x2 then x1 else x2
    initial = Nothing
    extract = fromMaybe (-0.5,0.5)

equalSpacedCuts :: Int -> [Double] -> [Double]
equalSpacedCuts n xs = cuts
  where
    (min, max) = range xs :: (Double,Double)
    span' = max - min
    step' = 10 ^^ (floor (logBase 10 (span'/fromIntegral n)) :: Int)
    err = fromIntegral n / span' * step'
    step :: Double
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first = step * fromIntegral (floor (min/step) :: Int)
    last = step * fromIntegral ((floor (max/step) + 1) :: Int)
    n' :: Int
    n' = fromInteger (round ((last - first)/step))
    cuts = (\x -> first+step*fromIntegral x) <$> [0..n']

rangeCuts :: Int -> Double -> Double -> [Double]
rangeCuts n start end =
    (\x -> start+((end-start)/fromIntegral n)*fromIntegral x) <$>
    [0..n]

fill :: [Double] -> [Double] -> Histogram
fill cuts xs = Histogram cuts (histMap cuts xs)

insertW :: Histogram -> Double -> Double -> Histogram
insertW (Histogram cuts vs) value weight = Histogram cuts (Map.unionWith (+) vs s)
    where
      s = histMapW cuts [(value,weight)]

insertWs :: Histogram -> [(Double, Double)] -> Histogram
insertWs (Histogram cuts vs) vws = Histogram cuts (Map.unionWith (+) vs s)
    where
      s = histMapW cuts vws

data DealOvers = IgnoreOvers | IncludeOvers Double

fromHist :: DealOvers -> Histogram -> [V4 Double]
fromHist o (Histogram cuts counts) = zipWith4 V4 x y z w'
  where
      y = repeat 0
      w = zipWith (/)
          ((\x -> Map.findWithDefault 0 x counts) <$> [f..l])
          (zipWith (-) z x)
      f = case o of
        IgnoreOvers -> 1
        IncludeOvers _ -> 0
      l = case o of
        IgnoreOvers -> length cuts - 1
        IncludeOvers _ -> length cuts
      w' = (/Protolude.sum w) <$> w
      x = case o of
        IgnoreOvers -> cuts
        IncludeOvers outw -> [Data.List.head cuts - outw] <> cuts <> [Data.List.last cuts + outw]
      z = drop 1 x

labelsFromCuts :: DealOvers -> [Double] -> [Text]
labelsFromCuts o cuts =
    case o of
      IgnoreOvers -> inside
      IncludeOvers _ -> [ "< " <> sformat (prec 2) (Data.List.head cuts)] <> inside <> [ "> " <> sformat (prec 2) (Data.List.last cuts)]
  where
    inside = sformat (prec 2) <$> zipWith (\l u -> (l+u)/2) cuts (drop 1 cuts)

hist :: [Double] -> Double -> L.Fold Double Histogram
hist cuts r =
    L.Fold
    (\(Histogram cuts counts) a ->
       Histogram cuts
       (Map.unionWith (+)
        (Map.map (*r) counts)
        (Map.singleton (L.fold countBool (fmap (a>) cuts)) 1)))
    (Histogram cuts mempty)
    identity

roundD :: Double -> Double -> Double
roundD grain x = if frac > 0.5 then whole + grain else whole
    where
      whole = floorDD
      frac = (x - floorDD) / grain
      floorDD = fromIntegral (floorD (x / grain)) * grain
      floorD x' | x' < 0     = double2Int x' - 1
                | otherwise = double2Int x'
{-# INLINE roundD #-}
