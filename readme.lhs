<meta charset="utf-8">
<link rel="stylesheet" href="other/lhs.css">

[online](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/online.png)](https://travis-ci.org/tonyday567/online)
===

tl;dr

online turns a statistic (a summary or fold of data) into an online algorithm.

derivation
===

Imagine a data stream, like an ordered indexed container or a time-series of measurements. An exponential moving average can be calculated as a repeated iteration over a stream of xs:

$$ ema_t = ema_{t-1} * 0.9 + x_t * 0.1$$

The 0.1 is akin to the learning rate in machine learning, or 0.9 can be thought of as a decaying or a rate of forgetting.  An exponential moving average learns about what the value of x has been lately, where lately is, on average, about 1/0.1 = 10 x's ago.  All very neat.

The first bit of neat is speed.  There's 2 times and a plus.  The next is space: an ema is representing the recent xs in a size as big as a single x.  Compare that with a simple moving average where you have to keep the history of the last n xs around to keep up (just try it).

It's so neat, it's probably a viable monoidal category all by itself.

online
===

Haskell allows us to abstract the compound ideas in an ema and create polymorphic routines over a wide variety of statistics, so that they all retain these properties of speed, space and rigour.

    av xs = L.fold (online identity (.* 0.9)) xs
    -- av [0..10] == 6.030559401413827
    -- av [0..100] == 91.00241448887785

`online identity (.* 0.9)` is how you express an ema with a decay rate of 0.9.

Here's an average of recent values for the grey line, for r=0.9 and r=0.99.

![](other/av.svg)

online works for any statistic.  Here's the construction of standard deviation using applicative style:

    std :: Double -> L.Fold Double Double
    std r = (\s ss -> sqrt (ss - s**2)) <$> ma r <*> sqma r
      where
        ma r = online identity (.*r)
        sqma r = online (**2) (.*r)

And the results over our fake data:

![](other/std.svg)

daily stock market data
---

Time to explore online using the most data-mined time-series in history; the S&P500 return since 1958. Here's the accumulation of all those daily random variates:

![](other/asum.svg)

And here's the histogram of daily log returns (grey background), and the most recent histogram onlined with a rate of 0.99:

![](other/hist.svg)

Recent returns have been higher and less volatile than the long history.  Roll on bull market.

momentum
===

Starting with a hypothesis that the current conditional mean is related to historical average return, we can construct a linear map as so:

$$ r_t = beta * av_o + alpha + e $$
$$ e = r_t - beta * av_o - alpha $$

Without this structure, each daily value can be seen as a surprise, so that $e=r_t$.

We can make the results of the regression an online fold as well:

![](other/cmean.svg)

The (online) alpha regression estimate through time is orange and beta is blue.  A typical pattern for regression - terms of opposite sign pretending to make sense of noise.  This is not the get-rich droid you are looking for.

But let's say it was.  Let's look at the histograms of return, and the residual error term if we include the conditional mean relationship:

![](other/cmeane.svg)

If there is an effect of recent returns on current return stochastics, it's small, but it does move the residual stochastics more towards a more symetrical distribution.

fat tails
---

We can do similar things for magnitude measures.

$$r_t**2 = beta * r2_o + alpha$$
$$r_t = (sqrt r_t**2) * e
$$e = r_t / sqrt (beta * r2_o + alpha)

![](other/csqma.svg)


and where to from here ...
====

- alpha and beta are independent calculations - wrong!
- calculate stats of e (residual error) that we might be interested in.
- draw a normal distribution on top of e
- add in code and explanations in the above sections




Code
===

> import Protolude hiding ((%))
> import Control.Monad.Primitive (unsafeInlineIO)
> import Online
> import Chart.Unit
> import Chart.Types
> import Data.Default
> import Control.Lens
> import qualified Control.Foldl as L
> import Linear hiding (identity)
> import Data.List
>

cassava
---

csv data arrives as a bytestring, gets decoded as a Vector, and decoding errors arrive as strings, so there's a fair bit of messiness working with Text Lists.

> import Data.Csv
> import GHC.Base (String)
> import Data.Text (pack)
> import Data.Text.Encoding (encodeUtf8Builder)
> import Data.ByteString.Builder (toLazyByteString)
> import Data.Vector (Vector)

pretty printing
---

> import Formatting

chart-unit
---

> import Chart.Unit
> import Chart.Types

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
>   , close :: ByteString
>   , volume :: ByteString
>   , adjustedClose :: !Double
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

main
===

main constructs output into charts and markdown fragments which are stitched together in this file using pandoc.

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o readme.html --filter pandoc-include

Think ipython notebook style without the fancy.

> main :: IO ()
> main = do
>     fileSvg "other/elems.svg" (300,300)
>         (rect'
>          (chartAxes . element 0 . axisTickStyle .~ TickNone $ def)
>          [def]
>          ([zipWith4 V4 [0..] (replicate 2000 0) [1..] (take 2000 ys)]))
>     fileSvg
>         "other/asum.svg" (300,300)
>         (line def [(LineConfig 0.002 (Color 0.33 0.33 0.33 0.4))]
>          ([zipWith V2 [0..] (L.scan L.sum ys)])
>          )
>     let fake = ([0..100] <> replicate 101 100 :: [Double])
>     fileSvg "other/av.svg" (300,300) $
>         line def
>         [ LineConfig 0.005 (Color 0.88 0.33 0.12 1)
>         , LineConfig 0.005 (Color 0.12 0.33 0.83 1)
>         , LineConfig 0.002 (Color 0.33 0.33 0.33 1)
>         ]
>         [ zipWith V2 [0..] (drop 1 $ L.scan (online identity (* 0.9)) fake)
>         , zipWith V2 [0..] (drop 1 $ L.scan (online identity (* 0.99)) fake)
>         , zipWith V2 [0..] fake
>         ]
>     fileSvg "other/std.svg" (300,300) $
>         line def
>         [ LineConfig 0.005 (Color 0.88 0.33 0.12 1)
>         , LineConfig 0.005 (Color 0.12 0.33 0.83 1)
>         , LineConfig 0.002 (Color 0.33 0.33 0.33 1)
>         ]
>         [ zipWith V2 [0..] (drop 1 $ L.scan (std 0.9) fake)
>         , zipWith V2 [0..] (drop 1 $ L.scan (std 0.99) fake)
>         , zipWith V2 [0..] fake
>         ]
>     fileSvg "other/cmean.svg" (300,300) $
>         line def
>         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
>         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
>         ]
>         [ zipWith V2 [0..] $
>           take 5000 $ drop 100 $ drop 2 $
>           (L.scan (beta 0.99)) $ drop 1 $
>           zip ys (L.scan (ma 0.9975) ys)
>         , zipWith V2 [0..] $
>           take 5000 $ drop 100 $ drop 2 $
>           (L.scan (alpha 0.99)) $ drop 1 $
>           zip ys (L.scan (ma 0.9975) ys)
>         ]
>     fileSvg "other/cmeane.svg" (300,300) $
>         rect' def
>         [def, RectConfig 0 (Color 0.88 0.33 0.12 0) (Color 0.33 0.33 0.12 0.3)]
>         [ toV4 $ L.fold (hist (rangeCuts 6 (-0.03) 0.03) 1) $
>           take 5000 $ drop 100 $
>           (L.scan ((\r b o a -> r - b * o - a) <$>
>              L.premap fst (ma 0.00001) <*>
>              beta 0.99 <*>
>              L.premap snd (ma 0.00001) <*>
>              alpha 0.99)) $
>            drop 400 $ zip ys (L.scan (ma 0.9975) ys)
>          , toV4 $ L.fold (hist (rangeCuts 6 (-0.03) 0.03) 1) $
>            take 5000 $ drop 100 $
>            ys
>          ]
>
>     fileSvg "other/csqma.svg" (300,300) $
>         line def
>         [ LineConfig 0.002 (Color 0.88 0.33 0.12 1)
>         , LineConfig 0.002 (Color 0.12 0.33 0.83 1)
>         ]
>         (fmap (zipWith V2 [0..]) <$> (\x -> [fst <$> x, snd <$> x]) $
>          take 12000 $ drop 12000 $ drop 2 $
>          ( L.scan ((,) <$> (alpha 0.99) <*> beta 0.99)) $
>            drop 100 $ zip ((**2)<$> ys) (L.scan (sqma 0.9975) ys))
>
>

basic stats
---

online mean and std at a 0.99 decay rate:

![](other/moments.svg)

>     let st = drop 1 $ L.scan ((,) <$> (ma 0.9) <*> (std 0.99)) ys
>     fileSvg "other/moments.svg" (300,300) $ (line def [(LineConfig 0.002 (Color 0.33 0.33 0.33 0.4)), (LineConfig 0.002 (Color 0.88 0.33 0.12 0.4))] $
>         [ zipWith V2 [0..] (fst <$> st)
>         , zipWith V2 [0..] (snd <$> st)
>         ])

scan of 1000 recent ma 0.99 and std 0.99, in basis points, rendered as a scatter chart.

![](other/scatter.svg)

>     fileSvg "other/scatter.svg" (500,500) $
>         scatter def [def] $ [drop (length ys - 1000) $
>         fmap (10000*) <$> L.scan (V2 <$> (ma 0.99) <*> (std 0.99)) ys]


A histogram with r=0.99 with lifetime stats as the grey background

![](other/hist.svg)

>     let cuts = (rangeCuts 5 (-0.02) 0.02)
>     let h = toV4 $ freq $ L.fold (hist cuts 0.99) ys
>     let h' = toV4 $ freq $ L.fold (hist cuts 1) ys
>     fileSvg "other/hist.svg" (300,300) $
>       rect'
>       (chartAxes .~ [def] $ def)
>       [def, rectBorderColor .~ Color 0 0 0 0
>       $ rectColor .~ Color 0.333 0.333 0.333 0.1
>       $ def]
>       [h, h']

quantiles
---

One problem with a histogram is the necessity of a prior about binning range and size.  An online approach - enforcing a single step through the data starting from scratch - tends to push these two-pass problems to the surface.

A similar statistic is a quantile computation, where bin ranges are allowed to vary, with bin edges converging to quantiles (or percentiles or whatever).  The decay method is to shrink the cuts towards the latest value.

```include
other/quantiles.md
```

>     writeFile "other/quantiles.md" $
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

>     writeFile "other/digitize.md" $
>         "\n    first 100 values digitized into quantiles:" <>
>         mconcat ((sformat (" " % prec 3) <$>)
>                  (take 100 $ L.scan (digitize 5 identity 0.996) ys))
>
>     filePng "other/scratchpad.png" (400,400) $ line def [def]
>         [zipWith V2 [0..] (L.scan L.sum ys), zipWith V2 [0..] ((2*)<$>(L.scan L.sum ys))]

workflow
---

    stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --filter pandoc-include

