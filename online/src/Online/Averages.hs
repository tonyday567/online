{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | online statistics based on a moving average
module Online.Averages
  ( Averager
  , online
    -- * online statistics
  , ma
  , absma
  , sqma
  , std
  , cov
  , corr
  , corrGauss
  , beta
  , alpha
  , autocorr
  , mconst
  ) where

import qualified Control.Foldl as L
import Control.Foldl (Fold(..))
import NumHask.Prelude

-- | Most common statistics are averages.
newtype Averager a b = Averager
  { _averager :: (a, b)
  }

instance (Semigroup a, Semigroup b) => Semigroup (Averager a b) where
  (Averager (s, c)) <> (Averager (s', c')) =
    Averager (s <> s', c <> c')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Averager a b) where
  mempty = Averager (mempty, mempty)
  mappend = (<>)

-- | online takes a function and turns it into a `Control.Foldl.Fold` where the step is an incremental update of the (isomorphic) statistic.
online :: (Field b) => (a -> b) -> (b -> b) -> Fold a b
online f g = Fold step begin extract
  where
    begin = Averager (zero, zero)
    step (Averager (s, c)) a = Averager (g $ s + f a, g $ c + one)
    extract (Averager (s, c)) = s / c
{-# INLINABLE online #-}

-- $setup
--
-- >>> :set -XNoImplicitPrelude
-- >>> import NumHask.Prelude
-- >>> import qualified Control.Foldl as L
-- >>> let n = 100
-- >>> let r = 0.9

-- | moving average with a decay rate
-- 
-- so 'ma 1' is the simple average (no decay in the statistic), and 'ma 0.00001' is the last value (insta-decay)
--
-- >>> L.fold (ma 1) [0..100]
-- 50.0
--
-- >>> L.fold (ma 1e-12) [0..100] ≈ 100
-- True
--
-- >>> L.fold (ma 0.9) [0..100]
-- 91.00241448887785
--
ma :: (Field a) => a -> Fold a a
ma r = online identity (* r)
{-# INLINABLE ma #-}

-- | absolute average
absma :: (Field a, Signed a) => a -> Fold a a
absma r = online abs (* r)
{-# INLINABLE absma #-}

-- | average square
sqma :: (Field a) => a -> Fold a a
sqma r = online (\x -> x * x) (* r)
{-# INLINABLE sqma #-}

-- | standard deviation
--
-- The formulae for standard deviation, expressed in online terminology, highlights how this statistic is composed of averages:
--
-- > (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r
--
-- The average deviation of the numbers 1..1000 is about 1 / sqrt 12 * 1000 (see <<https://en.wikipedia.org/wiki/Uniform_distribution_(continuous)#Standard_uniform wiki>>)
--
-- >>> L.fold (std 1) [0..1000]
-- 288.9636655359978
--
-- The average deviation with a decay of 0.99
--
-- >>> L.fold (std 0.99) [0..1000]
-- 99.28328803164005
std :: (ExpField a) => a -> Fold a a
std r = (\s ss -> sqrt (ss - s ** (one+one))) <$> ma r <*> sqma r
{-# INLINABLE std #-}

-- | the covariance of a tuple
-- given an underlying central tendency fold
cov :: (Field a) => Fold a a -> Fold (a, a) a
cov m =
  (\xy x' y' -> xy - x' * y') <$> L.premap (uncurry (*)) m <*> L.premap fst m <*>
  L.premap snd m
{-# INLINABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
corrGauss :: (ExpField a) => a -> Fold (a, a) a
corrGauss r =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov (ma r) <*>
  L.premap fst (std r) <*>
  L.premap snd (std r)
{-# INLINABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
corr :: (Field a) => Fold a a -> Fold a a -> Fold (a, a) a
corr central deviation =
  (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov central <*>
  L.premap fst deviation <*>
  L.premap snd deviation
{-# INLINABLE corr #-}

-- | the beta in a simple linear regression of a tuple
-- given an underlying central tendency fold
beta :: (Field a) => Fold a a -> Fold (a, a) a
beta m =
  (\xy x' y' x2 -> (xy - x' * y') / (x2 - x' * x')) <$> L.premap (uncurry (*)) m <*>
  L.premap fst m <*>
  L.premap snd m <*>
  L.premap (\(x, _) -> x * x) m
{-# INLINABLE beta #-}

-- | the alpha of a tuple
alpha :: (Field a) => Fold a a -> Fold (a, a) a
alpha m = (\y b x -> y - b * x) <$> L.premap fst m <*> beta m <*> L.premap snd m
{-# INLINABLE alpha #-}

{-| autocorrelation is a slippery concept.  This method starts with the concept that there is an underlying random error process (e), and autocorrelation is a process on top of that ie for a one-step correlation relationship.

value@t = e@t + k * e@t-1

where k is the autocorrelation.

There are thus two online rates needed: one for the average being considered to be the dependent variable, and one for the online of the correlation calculation between the most recent value and the moving average. For example,

> L.fold (autocorr zero one)

would estimate the one-step autocorrelation relationship of the previous value and the current value over the entire sample set.

-}
autocorr :: (BoundedField a) => Fold a a -> Fold (a, a) a -> Fold a a
autocorr central corrf =
  case central of
    (Fold mStep mBegin mDone) ->
      case corrf of
        (Fold dStep dBegin dDone) ->
          let begin = (mBegin, dBegin)
              step (mAcc, dAcc) a =
                ( mStep mAcc a
                , if isNaN (mDone mAcc)
                    then dAcc
                    else dStep dAcc (mDone mAcc, a))
              done = dDone . snd
          in Fold step begin done
{-# INLINABLE autocorr #-}

-- | a constant fold
mconst :: a -> L.Fold a a
mconst a = L.Fold (\() _ -> ()) () (const a)
