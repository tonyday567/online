{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Online.Stats (
    -- * convert a statistic to online
    Averager,
    online,

    -- * common statistics
    av,

    -- * online statistics
    ma,
    absma,
    sqma,
    std,
    cov,
    corr,
    corrGauss,
    beta,
    alpha,
    autocorr
  ) where

import Protolude
import qualified Control.Foldl as L
import Control.Foldl (Fold(..))

-- | Most common statistics are averages.
newtype Averager a b = Averager { _averager :: (a, b)}

instance (Monoid a, Monoid b) => Monoid (Averager a b) where
    mempty = Averager (mempty, mempty)
    mappend (Averager (s,c)) (Averager (s',c')) = Averager (mappend s s', mappend c c')

-- | online takes a function and turns it into a `Control.Foldl.Fold` where the step is an incremental update of the (isomorphic) statistic.
online :: (Fractional b) => (a -> b) -> (b -> b) -> Fold a b
online f g = Fold step begin extract
  where
  begin = Averager (0, 0)
  step (Averager (s,c)) a = Averager (g $ s+f a,g $ c+1)
  extract (Averager (s,c)) = s/c

-- | average
av :: (Fractional a) => Fold a a
av = Fold step begin extract
  where
  begin = Averager (0, 0)
  step (Averager (s,c)) a = Averager (s+a,c+1)
  extract (Averager (s,c)) = s/c
{-# INLINABLE av #-}

-- | moving average
ma :: (Fractional a) => a -> Fold a a
ma r = online identity (* r)
{-# INLINABLE ma #-}

-- | absolute average
absma :: (Fractional a) => a -> Fold a a
absma r = online abs (* r)
{-# INLINABLE absma #-}

-- | average square
sqma :: (Fractional a) => a -> Fold a a
sqma r = online (\x -> x*x) (* r)
{-# INLINABLE sqma #-}

-- | standard deviation
std :: (Floating a) => a -> Fold a a
std r = (\s ss -> sqrt (ss - s**2)) <$> ma r <*> sqma r
{-# INLINABLE std #-}

-- | the covariance of a tuple
-- given an underlying central tendency fold
cov :: (Floating a) => Fold a a -> Fold (a,a) a
cov m =
    (\xy x' y' -> xy - x' * y') <$>
    L.premap (uncurry (*)) m <*>
    L.premap fst m <*>
    L.premap snd m
{-# INLINABLE cov #-}

-- | correlation of a tuple, specialised to Guassian
corrGauss :: (Floating a) => a -> Fold (a,a) a
corrGauss r = (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov (ma r) <*> L.premap fst (std r) <*> L.premap snd (std r)
{-# INLINABLE corrGauss #-}

-- | a generalised version of correlation of a tuple
corr :: (Floating a) => Fold a a -> Fold a a -> Fold (a,a) a
corr central deviation =
    (\cov' stdx stdy -> cov' / (stdx * stdy)) <$>
    cov central <*>
    L.premap fst deviation <*>
    L.premap snd deviation
{-# INLINABLE corr #-}

-- | the beta in a simple linear regression of a tuple
-- given an underlying central tendency fold
beta :: (Floating a) => Fold a a -> Fold (a,a) a
beta m =
    (\xy x' y' x2 -> (xy - x'*y')/(x2 - x'*x')) <$>
    L.premap (uncurry (*)) m <*>
    L.premap fst m <*>
    L.premap snd m <*>
    L.premap (\(x,_) -> x*x) m
{-# INLINABLE beta #-}

-- | the alpha of a tuple
alpha :: (Floating a) => Fold a a -> Fold (a,a) a
alpha m =
    (\y b x -> y - b * x) <$>
    L.premap fst m <*> beta m <*> L.premap snd m
{-# INLINABLE alpha #-}

{-| autocorrelation is a slippery concept.  This method starts with the concept that there is an underlying random error process (e), and autocorrelation is a process on top of that ie for a one-step correlation relationship.

value@t = e@t + k * e@t-1

where k is the autocorrelation.

There are thus two online rates needed: one for the average being considered to be the dependent variable, and one for the online of the correlation calculation between the most recent value and the moving average. For example,

>>> L.fold (autocorr 0 1)

would estimate the one-step autocorrelation relationship of the previous value and the current value over the entire sample set. 

-}
autocorr :: (Floating a, RealFloat a) => Fold a a -> Fold (a,a) a -> Fold a a
autocorr central corrf =
    case central of
        (Fold mStep mBegin mDone) ->
            case corrf of
                (Fold dStep dBegin dDone) ->
                    let begin = (mBegin, dBegin)
                        step (mAcc,dAcc) a = (mStep mAcc a,
                            if isNaN (mDone mAcc)
                            then dAcc
                            else dStep dAcc (mDone mAcc, a))
                        done = dDone . snd in
                    Fold step begin done
