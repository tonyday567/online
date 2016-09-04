
<meta charset="utf-8">
<link rel="stylesheet" href="other/lhs.css">
 
online
===

Exploring the design space of online algorithms, charting, statistics and haskell.

[![Build Status](https://travis-ci.org/tonyday567/online.png)](https://travis-ci.org/tonyday567/online)



Machine learning in haskell is a .  With python, you are taken to a data nerd's wonderland of neat api's and gold-plated avenues of process and analysis. Haskell takes you straight to about here:

> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DeriveGeneric #-}
> import Protolude hiding ((%))
> import Control.Monad.Primitive (unsafeInlineIO)

online library
---

online is my term for turning a statistic into an incremental update of current state.

If we start with the concept of an average:

    av xs = (sum xs/length xs)
    -- av [0..10] == 5.0

As we traverse a sequence (has an ordering from first to last, often associated with a dimension like time), a local version of average is a useful state to describe for upstream processing.  What has been the average recently.  A moving average weights the elements seen so far within a window.  A moving average of the last three elements, when 6 have been seen looks like:

    0 0 0 1 1 1 _

In the next step, the weights now look like:

    0 0 0 0 1 1 1

This incremental update for a new data point - how has the recent moving average changed - looks like this:

    0 0 0 -1 0 0 1


An interpretation is that the the ma `learns` the newest and `forgets` the value at `t-3`.

But there are other weighting schemes.  The baseline narrative of a moving average update - what have the values averaged lately - tends towards selecting a weighting scheme where:

- very old values are almost forgotten ie not be influencing the current moving average much at all.
- newer values should hold more weight than older ones; weights are monotonicaly decreasing from latest to oldest value.

One solution is to arrange the weights of the incremental update like:

    [... (-0.1 * 0.9^2) (-0.1 * 0.9^1) (-0.1) 1]

And, taking the difference between the moving average before and after an update step gives this computation:

    \(summer, counter) a -> (summer * 0.9 + a, counter * 0.9 + 1)


`online` reifies this pattern into the foldl library api.

    av xs = L.fold (online identity (*0.9)) xs
    -- av [0..10] == 6.030559401413827
    -- av [0..100] == 91.00241448887785

This provides a moving average narrative that provides an intuitive representation of how big recent numbers have been at the end of a [0..100] data stream: 91ish rather than 50 when comparing the lifetime average.

online exposes:

- a decay function governing the rate at which the statistic decays.
- a stat function, that is the statistic to be computed.

decay
---

The decay function `(*r)` can be widely interpreted:

- a decay function equal to identity provides lifetime statistical calculations ie no forgetting.
- a decay function of `const 0` (or `(*0)`) provides the latest value ie always forget.
- in physical systems, an exponential-weighted moving average where the center-of-mass is x would be isomorphic to a decay function of (*(1-1/x)).
- where the data represents time series, the center-of-mass is often referred to as duration.  The duration of (*0.99) is 100.
- in bayesian methods, a learning rate of x is often equivalent to a statistic being decayed or forgotten by (*(1-x))

stat
---

stat is a premap function that is the statistic of interest:

    av = online id id

is a classical average over the whole sample.  A decay rate of zero means no forgetting.

    sqav = online (*2) id

is a lifetime squared average. And this:

    std = (\s ss -> sqrt (ss - s**2)) <$> av <*> sqav

is the standard deviation.

We get applicative syntax via the foldl library.

This:

    ma x = online identity (*x)

is then an exponentially-weighted moving average, with a decay rate of 0.9 per step. 

    std x x' = (\s ss -> sqrt (ss - s**2)) <$> online id (*x) <*> online (*2) (*x')

Very similar to the boiler-plate grade school version of standard deviation, but here the mean is conditional; itself a weighted average calculation, and with potentially a different decay function.

A correlation fold of a tuple is quite intuitive:

    cov r = (\xy xbar ybar -> xy - xbar * ybar) <$> online (uncurry (*)) r <*> online fst r <*> online snd r
    corr r = (\cov' stdx stdy -> cov' / (stdx * stdy)) <$> cov r <*> L.premap fst (std r) <*> L.premap snd (std r)


> import Online
> import qualified Control.Foldl as L


cassava
---

csv data arrives as a bytestring, gets decoded as a Vector, and decoding errors arrive as strings, so there's a fair bit of messiness working with Text Lists.


> import Data.Csv
> import GHC.Base (String)
> import Data.Text (pack, unpack)
> import Data.Text.Encoding (encodeUtf8Builder)
> import Data.ByteString.Builder (toLazyByteString)
> import Data.Vector (Vector)


pretty printing
---

> import Formatting
> import Chart

main
---

> main :: IO ()
> main = do

The test data is the daily S&P500 price index.

Time and money both tend towards being geometric, so a natural transformation is to look at the differences in log price as the main unit of analysis.  This is also the log(1+return).

To abstract a bit, I'm going to name them ys.  The xs in the data is time, but I'm choosing to forget this data piece and just retain the ordering information.  So xs can be thought of as [0..] in most cases.

This also makes the ys additive so that sum(ys) is always and meaningfully log(1+return) over the range being summed.

The first 2k ys:

![](other/elems.svg)

>     toFileChart "other/elems.svg" size
>         (barRange
>          (barChart . chartAxes . element 0 . axisTickStyle .~ TickNone $ def)
>          (zip [0..] (take 2000 ys)))
>     toFileChart
>         "other/asum.svg" size
>         (lineXY
>          (lineChart . chartAxes .~
>           fmap (axisTickStyle .~ TickNone)
>            (def ^. chartAxes) $ def)
>          (zip [0..] (L.scan L.sum ys)))


Accumulated sum of ys aka `L.scan L.sum ys`:

![](other/asum.svg)

statistics are folded using different decay functions:

- 'lifetime': id giving stats over the entire foldable
- 'year' : decay function of (*(1-1/250)) giving stats with a duration of about a year
- 'month': decay function of (*(1-1/250)) giving stats with a duration of about a month

```include
other/basic_stats.md
```

>     toFileMd "other/basic_stats.md" $
>         table ["stat","lifetime","year","month"]
>         (transpose
>          [ labels
>          , formatStats 250 $ L.fold (statsF 1) ys
>          , formatStats 250 $ L.fold (statsF (1-1/250)) ys
>          , formatStats 250 $ L.fold (statsF (1-1/20)) ys
>          ])


A histogram of all elements, outliers truncated.

![](other/hist.svg)

>     let h = toXY $ fill (rangeCuts 100 (-0.02) 0.02) ys
>     toFileChart "other/hist.svg" size $
>       barRange
>       (barChart . chartAxes .~
>        [axisTickStyle .~ TickLabels
>         (formatToString (prec 2) <$> rangeCuts 4 (-0.02) 0.02)
>         $ def] $ def)
>       h


quantiles
---

One problem with a histogram is the necessity of a prior about binning range and size.  An online approach - enforcing a single step through the data starting from scratch - tends to push these two-pass problems to the surface.

A similar statistic is a quantile computation, where bin ranges are allowed to vary, with bin edges converging to quantiles (or percentiles or whatever).  The decay method is to shrink the cuts towards the latest value.

```include
other/quantiles.md
```

>     toFileMd "other/quantiles.md" $
>         "\n    [min, 10th, 20th, .. 90th, max]:" <>
>         mconcat (sformat (" " % prec 3) <$> toList
>                  (L.fold (quantiles' 11) ys)) <>
>         "\n    online [min, 10th, 20th, .. 90th, max] with decay rate = 0.996 (one year)" <>
>         mconcat (sformat (" " % prec 3) <$> toList
>                  (L.fold (quantiles 11 identity 0.996) ys))


digitize
---

A related computation is to output the quantile of each value:


```include
other/digitize.md
```

>     toFileMd "other/digitize.md" $
>         "\n    first 100 values digitized into quantiles:" <>
>         mconcat ((sformat (" " % prec 3) <$>)
>                  (take 100 $ L.scan (digitize 5 identity 0.996) ys))

basic stats fold
---

> labels :: [Text]
> labels =
>     ["mean",
>      "sd",
>      "abs mean",
>      "pa mean",
>      "pa sd"
>     ]
>
> data Stats =
>     Stats {
>     statMa :: Double,
>     statStd :: Double,
>     statAbsma:: Double}
>
> statsF :: Double -> L.Fold Double Stats
> statsF rate = Stats <$> ma rate <*> std rate <*> absma rate
>
> formatStats :: Double -> Stats -> [Text]
> formatStats pa stats =
>     [ sformat (prec 3) (statMa stats)
>     , sformat (prec 3) (statStd stats)
>     , sformat (prec 3) (statAbsma stats)
>     , sformat (prec 3) (pa * statMa stats)
>     , sformat (prec 3) (sqrt pa * statStd stats)
>     ]

 
data munge
---

data is from [yahoo](https://www.quandl.com/data/YAHOO/INDEX_GSPC-S-P-500-Index) and consists of the following fields:

    Date,Open,High,Low,Close,Volume,Adjusted Close

Stats are soley on adjusted close.


> data YahooRep = YahooRep
>   { date :: ByteString
>   , open :: ByteString
>   , high :: ByteString
>   , low :: ByteString
>   , close :: !ByteString
>   , volume :: ByteString
>   , adjustedClose :: Double
>   } deriving Generic
>
> instance FromRecord YahooRep


The base unit for analysis (which I've called ys to abstract) is log(1+return).  Returns are geometric by nature, and this premap removes the effect before we get to distributions.


> ys :: [Double]
> ys = fmap (\x -> log (1+x)) $ ret $ reverse $ unsafeInlineIO $ do
>     bs <- readFile "other/YAHOO-INDEX_GSPC.csv"
>     let rawdata =
>             decode HasHeader (toLazyByteString $ encodeUtf8Builder bs)
>             :: Either String (Vector YahooRep)
>     case rawdata of
>         (Left e) -> panic $ pack e
>         (Right xs) -> pure $ adjustedClose <$> toList xs
>
> ret :: [Double] -> [Double]
> ret [] = []
> ret [_] = []
> ret xs = L.fold diff' xs
>     where
>         diff' = L.Fold step ([], Nothing) fst
>         step x a = case snd x of
>             Nothing -> ([], Just a)
>             Just a' -> ((a-a')/a':fst x, Just a)


markdown and chart combinators
---

> size :: (Double,Double)
> size = (400,400)
>
> toFileChart :: Text -> (Double,Double) -> ChartSvg Double -> IO ()
> toFileChart fileName shape chart = do
>     toFile (unpack fileName) shape chart
>
>
> toFileMd :: FilePath -> Text -> IO ()
> toFileMd = writeFile
>
>
> table :: [Text] -> [[Text]] -> Text
> table headers ts =
>     mconcat $ intersperse "\n"
>         ([wrap headers] <>
>          [wrap (replicate (length headers) "---")] <>
>          (wrap <$> ts))
>       where
>         wrap xs = mconcat $ ["|"] <> intersperse "|" xs <> ["|"]



To compile via pandoc

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o readme.html --filter pandoc-include

