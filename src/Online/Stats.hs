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
newtype Averager = Averager { _averager :: (Double,Double)}

instance Monoid Averager where
    mempty = Averager (0,0)
    mappend (Averager (s,c)) (Averager (s',c')) = Averager (s+s',c+c')

-- | online takes a function and turns it into a `Control.Foldl.Fold` where the step is an incremental update of the (isomorphic) statistic.
online :: (a -> Double) -> (Double -> Double) -> Fold a Double
online f g = Fold step begin extract
  where
  begin = Averager (0,0)
  step (Averager (s,c)) a = Averager ((g $ s+f a),(g $ c+1))
  extract (Averager (s,c)) = s/c

-- | moving average
ma :: Double -> Fold Double Double
ma r = online identity (*r)
{-# INLINABLE ma #-}

-- | absolute average
absma :: Double -> Fold Double Double
absma r = online abs (*r)
{-# INLINABLE absma #-}

-- | average square
sqma :: Double -> Fold Double Double
sqma r = online (\x -> x*x) (*r)
{-# INLINABLE sqma #-}

-- | standard deviation
std :: Double -> Fold Double Double
std r = (\s ss -> sqrt (ss - s**2)) <$> ma r <*> sqma r
{-# INLINABLE std #-}

-- | covariance
cov :: Double -> Fold (Double,Double) Double
cov r = (\xy xbar ybar -> xy - xbar * ybar) <$> online (uncurry (*)) (*r) <*> online fst (*r) <*> online snd (*r)
{-# INLINABLE cov #-}

-- | correlation
corr :: Double -> Fold (Double,Double) Double
corr r = (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov r <*> L.premap fst (std r) <*> L.premap snd (std r)
{-# INLINABLE corr #-}

-- | the beta in a simple linear regression of `snd` on `fst`
beta :: Double -> Fold (Double,Double) Double
beta r = (/) <$> cov r <*> L.premap snd (std r)
{-# INLINABLE beta #-}

-- | the alpha in a simple linear regression of `snd` on `fst`
alpha :: Double -> Fold (Double,Double) Double
alpha r = (\y b x -> y - b * x) <$> L.premap fst (ma r) <*> beta r <*> L.premap snd (ma r)
{-# INLINABLE alpha #-}

{-| autocorrelation is a slippery concept.  This method starts with the concept that there is an underlying random error process (e), and autocorrelation is a process on top of that ie for a one-step correlation relationship.

value@t = e@t + k * e@t-1

where k is the autocorrelation.

There are thus two online rates needed: one for the average being considered to be the dependent variable, and one for the online of the correlation calculation between the most recent value and the moving average. 

>>> L.fold (autoCorr 0 1)

Would estimate the one-step autocorrelation relationship of the previous value and the current value over the entire sample set. 

-}
autocorr :: Double -> Double -> Fold Double Double
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
