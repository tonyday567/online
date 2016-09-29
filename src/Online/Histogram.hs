{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Online.Histogram where

import Protolude
-- import           Control.Foldl (Fold(..))
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map

-- a histogram
data Histogram = Histogram
   { _cuts   :: [Double] -- bucket boundaries
   , _values :: Map.Map Int Double -- bucket counts
   } deriving (Show, Eq)


count :: L.Fold Int (Map Int Double)
count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity

countBool :: L.Fold Bool Int
countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity

histMap :: (Functor f, Functor g, Ord a, Foldable f, Foldable g) =>
    f a -> g a -> Map Int Double
histMap cuts xs = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> xs

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
    (min, max) = range xs
    span' = max - min
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first = step * fromIntegral (floor (min/step))
    last = step * fromIntegral (floor (max/step) + 1)
    n' = fromIntegral $ round ((last - first)/step)
    cuts = (\x -> first+step*fromIntegral x) <$> [0..n']

rangeCuts :: Int -> Double -> Double -> [Double]
rangeCuts n start end =
    (\x -> start+((end-start)/fromIntegral n)*fromIntegral x) <$>
    [0..n]

fill :: [Double] -> [Double] -> Histogram
fill cuts xs = Histogram cuts (histMap cuts xs)

toXY :: Histogram -> [(Double,Double)]
toXY (Histogram cuts counts) = zip x y
    where
      n = length cuts - 1
      y = (\x -> Map.findWithDefault 0 x counts) <$> [1..n]
      x = zipWith (+) cuts ((0.5*) <$> (zipWith (-) (drop 1 cuts) cuts))

hist :: [Double] -> Double -> L.Fold Double Histogram
hist cuts r =
    L.Fold
    (\(Histogram cuts counts) a ->
       Histogram cuts
       (Map.unionWith (+)
        (Map.map (*r) counts)
        (Map.singleton (L.fold countBool (fmap (a>) cuts)) (1-(1/r)))))
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
