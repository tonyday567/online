{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Online.Quantiles where

import qualified Control.Foldl as L
import qualified Data.Vector as V
import Data.Vector ((!),Vector)
import Control.Lens
import Data.List

data Quantiles = Quantiles
  { _qSize    :: Int
    -- ^ number of quantiles (_qSize-1 = number of buckets)
  , _qMarkers :: Vector Double
    -- ^ quantile points
  , _qCounts  :: Vector Double
    -- ^ cumulative frequency (> except for first quantile)
  } deriving (Show, Eq)

quantiles :: Int -> L.Fold Double [Double]
quantiles n = L.Fold stepP2 (Quantiles n V.empty V.empty) (\(Quantiles _ m _) -> V.toList m)

max :: Quantiles -> Double
max (Quantiles s m _) = m V.! (s-1)

min :: Quantiles -> Double
min (Quantiles _ m _) = m V.! 0

nth :: Int -> Quantiles -> Double
nth n (Quantiles _ m _) = m V.! n

stepP2 :: Quantiles -> Double -> Quantiles
stepP2 (Quantiles b ms cs) a
  | V.length ms < b = Quantiles b (V.fromList (sort $ cons a (V.toList ms))) (V.generate (fromIntegral $ V.length ms + 1) (\x -> fromIntegral x + 1.0))
  | otherwise =
  let
    (Quantiles _ ms'' cs'') = addNewData (Quantiles b ms cs) a
    csIdeal = V.generate b (\i -> 1 + ((V.last cs'')-1) * fromIntegral i/(fromIntegral b-1)) :: Vector Double
    d = V.zipWith (-) csIdeal cs''
    dsign = V.map signum d
    c' = V.zipWith (-) (V.tail cs'') (V.init cs'')
    isOut = V.zipWith3 (\d' nd1 nd0 -> d' >= 1 && nd1 > 1  || d' <= -1 && nd0 > 1) (V.init $ V.tail d) (V.tail c') c'
    p2' = p2 ms'' (cs'') dsign
    l2' = l2 ms'' (cs'') dsign
    ms24 = V.take (b-2) $ V.tail ms''
    okP2 = V.zipWith3 (\x x0 x1 -> x > x0 && x < x1) p2' ms'' (V.drop 2 ms'')
    ms24' = V.zipWith3 (\x y z-> if x then y else z) okP2 p2' l2'
    ms24'' = V.zipWith3 (\x y z-> if x then y else z) isOut ms24' ms24
    ms''' = V.take 1 ms'' V.++ ms24'' V.++ V.drop (b-1) ms''
    cs24 = V.zipWith3 (\x y z-> if x then y+z else y) isOut (V.init $ V.tail cs'') (V.map (fromIntegral . fromEnum) $ V.init $ V.tail dsign)
    cs''' = V.take 1 cs'' V.++ cs24 V.++ V.drop (b-1) cs''
  in
   Quantiles b ms''' cs'''

addNewData :: Quantiles -> Double -> Quantiles
addNewData (Quantiles b ms qs) a
    | a < ms!0 =
      Quantiles b (cons a (V.tail ms))
      (cons 1 (V.map (+1) $ V.tail qs))
    | a > V.last ms =
      Quantiles b
      (snoc (V.init ms) a)
      (qs V.// [(b-1, 1 + qs!(b-1))])
    | otherwise =
      Quantiles b
      ms
      (V.imap (\i q -> if ms!i > a then q+1 else q) qs)

p2 :: Vector Double -> Vector Double -> Vector Double -> Vector Double
p2 q n d = res
  where
    d'  = V.init $ V.tail d
    ndelta  = V.zipWith (-) (V.tail n) n
    qdelta  = V.zipWith (-) (V.tail q) q
    n2delta = V.zipWith (-) (V.drop 2 n) n
    dq0 = V.zipWith4 (\d'' nd' q' n' -> (nd' + d'') * q' / n') d (V.init ndelta) (V.tail qdelta) (V.tail ndelta)
    dq1 = V.zipWith4 (\d'' nd' q' n' -> (nd' - d'') * q' / n') d (V.tail ndelta) (V.init qdelta) (V.init ndelta)
    delta = V.zipWith3 (\n2' dq0' dq1' -> (dq0' + dq1') / n2') n2delta dq0 dq1
    res = V.zipWith3 (\q'' d'' delta' -> q'' + d'' * delta') (V.init $ V.tail q) d' delta

l2 :: Vector Double -> Vector Double -> Vector Double -> Vector Double
l2 q n d =
  V.zipWith6
  (\q'' d'' qd' nd' qd'' nd'' ->
    ((+) q''
     (if signum d'' == 1 then d'' * qd' / nd' else d'' * qd'' / nd'')))
  q' d' (V.tail qdelta) (V.tail ndelta) (V.init qdelta) (V.init ndelta)
  where
    q' = V.init (V.tail q)
    d' = V.init (V.tail d)
    ndelta  = V.zipWith (-) (V.tail n) n
    qdelta  = V.zipWith (-) (V.tail q) q

onlineQuantiles :: Int -> (a -> Double) -> Double -> L.Fold a [Double]
onlineQuantiles n f decay = L.Fold step begin extract
  where
    begin = Quantiles n V.empty V.empty
    extract (Quantiles _ m _) = V.toList m
    step x a = Quantiles s' (V.map (\x' -> decay*x'+(1-decay)*f a) m') c'
      where
        (Quantiles s' m' c') = stepP2 x (f a)


online' :: Int -> (a -> Double) -> Double -> L.Fold a [Double]
online' n f decay = L.Fold step begin extract
  where
    begin = Quantiles n V.empty V.empty
    extract (Quantiles _ m _) = V.toList m
    step x a =
        Quantiles
        s'
        (V.map (\x' -> decay*x'+(1-decay)*f a) m')
        (V.map (\x' -> decay*x') c')
      where
        (Quantiles s' m' c') = stepP2 x (f a)

