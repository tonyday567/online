{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Online.Stats (
    -- * convert a statistic to online
    Averager,
    online,

    -- * common statistics
    ma,
    absma,
    sqma,
    std,
    cov,
    corr,
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
  step (Averager (s,c)) a = Averager ((g $ s+f a),(g $ c+1))
  extract (Averager (s,c)) = s/c

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

-- | covariance of a tuple
cov :: (Fractional a) => a -> Fold (a,a) a
cov r = (\xy xbar ybar -> xy - xbar * ybar) <$> online (uncurry (*)) (*r) <*> online fst (*r) <*> online snd (*r)
{-# INLINABLE cov #-}

-- | correlation of a tuple
corr :: (Floating a) => a -> Fold (a,a) a
corr r = (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov r <*> L.premap fst (std r) <*> L.premap snd (std r)
{-# INLINABLE corr #-}

-- | the beta in a simple linear regression of a tuple
beta :: (Floating a) => a -> Fold (a,a) a
beta r = (/) <$> cov r <*> L.premap snd (std r)
{-# INLINABLE beta #-}

-- | the alpha in a simple linear regression of `snd` on `fst`
alpha :: (Floating a) => a -> Fold (a,a) a
alpha r = (\y b x -> y - b * x) <$> L.premap fst (ma r) <*> beta r <*> L.premap snd (ma r)
{-# INLINABLE alpha #-}

{-| autocorrelation is a slippery concept.  This method starts with the concept that there is an underlying random error process (e), and autocorrelation is a process on top of that ie for a one-step correlation relationship.

value@t = e@t + k * e@t-1

where k is the autocorrelation.

There are thus two online rates needed: one for the average being considered to be the dependent variable, and one for the online of the correlation calculation between the most recent value and the moving average. For example,

>>> L.fold (autocorr 0 1)

would estimate the one-step autocorrelation relationship of the previous value and the current value over the entire sample set. 

-}

autocorr :: (Floating a, RealFloat a) => a -> a -> Fold a a
autocorr maR corrR = 
    case ma maR of
        (Fold maStep maBegin maDone) ->
            case corr corrR of
                (Fold corrStep corrBegin corrDone) ->
                    let begin = (maBegin, corrBegin)
                        step (maAcc,corrAcc) a = (maStep maAcc a,
                            if isNaN (maDone maAcc)
                            then corrAcc
                            else corrStep corrAcc (maDone maAcc, a)) 
                        done = corrDone . snd in
                    Fold step begin done
