{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Online.Quantiles where

import Tower.Prelude hiding (zipWith, drop, take, (++), empty, toList, map, length)
import qualified Control.Foldl as L
import Data.TDigest
import Data.TDigest.Internal.Tree
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Heap as VHeap
import Data.List.NonEmpty (NonEmpty)

-- | non-online version
tQuantiles :: [Double] -> L.Fold Double [Double]
tQuantiles qs = L.Fold step begin done
  where
    step x a = insert a x
    begin = tdigest ([]::[Double]) :: TDigest 25
    done x = fromMaybe nan <$> (\q -> quantile q (compress x)) <$> qs

-- | non-online version
tHist :: L.Fold Double (Maybe (NonEmpty HistBin))
tHist = L.Fold step begin done
  where
    step x a = insert a x
    begin = tdigest ([]::[Double]) :: TDigest 25
    done x = histogram . compress $ x

data OnlineTDigest = OnlineTDigest { td :: TDigest 25, tdN :: Int, tdRate :: Double } deriving (Show)

emptyOnlineTDigest :: Double -> OnlineTDigest
emptyOnlineTDigest r = OnlineTDigest (emptyTDigest :: TDigest n) 0 r

-- | decaying quantiles based on the tdigest library
onlineQuantiles :: Double -> [Double] -> L.Fold Double [Double]
onlineQuantiles r qs = L.Fold step begin done
  where
    step x a = onlineInsert a x
    begin = emptyOnlineTDigest r
    done x = fromMaybe nan <$> (\q -> quantile q t) <$> qs
      where
        (OnlineTDigest t _ _) = onlineForceCompress x

onlineInsert' :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert' x (OnlineTDigest td n r) = OnlineTDigest (insertCentroid (x, r^^(-(n+1))) td) (n+1) r

onlineInsert :: Double -> OnlineTDigest -> OnlineTDigest
onlineInsert x otd = onlineCompress (onlineInsert' x otd)

onlineCompress :: OnlineTDigest -> OnlineTDigest
onlineCompress otd@(OnlineTDigest Nil _ _ ) = otd
onlineCompress otd@(OnlineTDigest t _ _)
    | Data.TDigest.Internal.Tree.size t > relMaxSize * compression && Data.TDigest.Internal.Tree.size t > absMaxSize
        = onlineForceCompress otd
    | otherwise
        = otd
  where
    compression = fromInteger 25

onlineForceCompress :: OnlineTDigest -> OnlineTDigest
onlineForceCompress otd@(OnlineTDigest Nil _ _ ) = otd
onlineForceCompress (OnlineTDigest t n r) = OnlineTDigest t' 0 r
  where
    t' = Tower.Prelude.foldl' (flip insertCentroid) emptyTDigest $
         fmap (\(m,w) -> (m, w*(r^^n))) $ fmap fst $ VU.toList centroids
    -- Centroids are shuffled based on space
    centroids :: VU.Vector (Centroid, Double)
    centroids = runST $ do
        v <- toMVector t
        -- sort by cumulative weight
        VHeap.sortBy (comparing snd) v
        f <- VU.unsafeFreeze v
        pure f

onlineDigitize :: Double -> [Double] -> L.Fold Double Int
onlineDigitize r qs = L.Fold step begin done
  where
    step (x,l) a = (onlineInsert a x, l)
    begin = (emptyOnlineTDigest r, nan)
    done (x,l) = bucket' qs' l
      where
        qs' = fromMaybe nan <$> (\q -> quantile q t) <$> qs
        (OnlineTDigest t _ _) = onlineForceCompress x
        bucket' xs l' = L.fold L.sum $ (\x' -> if x'>l' then 0 else 1) <$> xs

-- | decaying histogram based on the tdigest library
onlineHist :: Double -> L.Fold Double (Maybe (NonEmpty HistBin))
onlineHist r = L.Fold step begin done
  where
    step x a = onlineInsert a x
    begin = emptyOnlineTDigest r
    done x = histogram . compress $ t
      where
        (OnlineTDigest t _ _) = onlineForceCompress x
