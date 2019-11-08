module Online.Medians
  ( -- * convert a statistic to an online median stat equivalent to L1
    Medianer(..)
  , onlineL1
  , onlineL1'

    -- * online statistics
  , maL1
  , absmaL1
  , covL1
  , corrL1
  , betaL1
  , alphaL1
  , autocorrL1
  ) where

import qualified Control.Foldl as L
import Control.Foldl (Fold(..))
import Prelude

-- | A rough Median.
-- The average absolute value of the stat is used to callibrate estimate drift towards the median
data Medianer a b = Medianer
  { medAbsSum :: a
  , medCount :: b
  , medianEst :: a
  }

-- | onlineL1' takes a function and turns it into a `Control.Foldl.Fold` where the step is an incremental update of an (isomorphic) median statistic.
onlineL1' ::
     (Ord b, Fractional b) => b -> b -> (a -> b) -> (b -> b) -> Fold a (b, b)
onlineL1' i d f g = Fold step begin extract
  where
    begin = Medianer 0 0 0
    step (Medianer s c m) a =
      Medianer
        (g $ s + abs (f a))
        (g $ c + 1)
        ((1 - d) * (m + s' * i * s / c') + d * f a)
      where
        c' =
          if c == 0
            then 1
            else c
        s'
          | f a > m = 1
          | f a < m = -1
          | otherwise = 0
    extract (Medianer s c m) = (s / c, m)
{-# INLINABLE onlineL1' #-}

-- | onlineL1 takes a function and turns it into a `Control.Foldl.Fold` where the step is an incremental update of an (isomorphic) median statistic.
onlineL1 :: (Ord b, Fractional b) => b -> b -> (a -> b) -> (b -> b) -> Fold a b
onlineL1 i d f g = snd <$> onlineL1' i d f g
{-# INLINABLE onlineL1 #-}

-- $setup
--
-- >>> :set -XNoImplicitPrelude
-- >>> import NumHask.Prelude
-- >>> import qualified Control.Foldl as L
-- >>> let n = 100
-- >>> let inc = 0.1
-- >>> let d = 0
-- >>> let r = 0.9

-- | moving median
-- >>> L.fold (maL1 inc d r) [1..n]
-- 93.92822312742108
--
maL1 :: (Ord a, Fractional a) => a -> a -> a -> Fold a a
maL1 i d r = onlineL1 i d id (* r)
{-# INLINABLE maL1 #-}

-- | moving absolute deviation
absmaL1 :: (Ord a, Fractional a) => a -> a -> a -> Fold a a
absmaL1 i d r = fst <$> onlineL1' i d id (* r)
{-# INLINABLE absmaL1 #-}

-- | covariance of a tuple
covL1 :: (Ord a, Fractional a) => a -> a -> a -> Fold (a, a) a
covL1 i d r =
  (\xy xbar ybar -> xy - xbar * ybar) <$> onlineL1 i d (uncurry (*)) (* r) <*>
  onlineL1 i d fst (* r) <*>
  onlineL1 i d snd (* r)
{-# INLINABLE covL1 #-}

-- | correlation of a tuple
corrL1 :: (Ord a, Floating a) => a -> a -> a -> Fold (a, a) a
corrL1 i d r =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> covL1 i d r <*>
  L.premap fst (absmaL1 i d r) <*>
  L.premap snd (absmaL1 i d r)
{-# INLINABLE corrL1 #-}

-- | the beta in a simple linear regression of a tuple
betaL1 :: (Ord a, Floating a) => a -> a -> a -> Fold (a, a) a
betaL1 i d r =
  (\xy x' y' x2 -> (xy - x' * y') / (x2 - x' * x')) <$>
  L.premap (uncurry (*)) (maL1 i d r) <*>
  L.premap fst (maL1 i d r) <*>
  L.premap snd (maL1 i d r) <*>
  L.premap (\(x, _) -> x * x) (maL1 i d r)
{-# INLINABLE betaL1 #-}

-- | the alpha in a simple linear regression of `snd` on `fst`
alphaL1 :: (Ord a, Floating a) => a -> a -> a -> Fold (a, a) a
alphaL1 i d r =
  (\y b x -> y - b * x) <$> L.premap fst (maL1 i d r) <*> betaL1 i d r <*>
  L.premap snd (maL1 i d r)
{-# INLINABLE alphaL1 #-}

autocorrL1 :: (Floating a, RealFloat a) => a -> a -> a -> a -> Fold a a
autocorrL1 i d maR corrR =
  case maL1 i d maR of
    (Fold maStep maBegin maDone) ->
      case corrL1 i d corrR of
        (Fold corrStep corrBegin corrDone) ->
          let begin = (maBegin, corrBegin)
              step (maAcc, corrAcc) a =
                ( maStep maAcc a
                , if isNaN (maDone maAcc)
                    then corrAcc
                    else corrStep corrAcc (maDone maAcc, a))
              done = corrDone . snd
          in Fold step begin done
{-# INLINABLE autocorrL1 #-}
