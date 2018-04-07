[online](https://github.com/tonyday567/online)
==============================================

[![Build
Status](https://travis-ci.org/tonyday567/online.svg)](https://travis-ci.org/tonyday567/online)
[![Hackage](https://img.shields.io/hackage/v/online.svg)](https://hackage.haskell.org/package/online)
[![lts](https://www.stackage.org/package/online/badge/lts)](http://stackage.org/lts/package/online)
[![nightly](https://www.stackage.org/package/online/badge/nightly)](http://stackage.org/nightly/package/online)

online turns a statistic (in haskell this can usually be thought of as a
fold of a foldable) into an online algorithm.

motivation
==========

Imagine a data stream, like an ordered indexed container or a
time-series of measurements. An exponential moving average can be
calculated as a repeated iteration over a stream of xs:

$$ ema_t = ema_{t-1} * 0.9 + x_t * 0.1 $$

The 0.1 is akin to the learning rate in machine learning, or 0.9 can be
thought of as a decaying or a rate of forgetting. An exponential moving
average learns about what the value of x has been lately, where lately
is, on average, about 1/0.1 = 10 x's ago. All very neat.

The first bit of neat is speed. There's 2 times and a plus. The next is
space: an ema is representing the recent xs in a size as big as a single
x. Compare that with a simple moving average where you have to keep the
history of the last n xs around to keep up (just try it).

It's so neat, it's probably a viable monoidal category all by itself.

online
======

Haskell allows us to abstract the compound ideas in an ema and create
polymorphic routines over a wide variety of statistics, so that they all
retain these properties of speed, space and rigour.

    av xs = L.fold (online identity (.* 0.9)) xs
    -- av [0..10] == 6.030559401413827
    -- av [0..100] == 91.00241448887785

`online identity (.* 0.9)` is how you express an ema with a decay rate
of 0.9.

online works for any statistic. Here's the construction of standard
deviation using applicative style:

    std :: Double -> L.Fold Double Double
    std r = (\s ss -> sqrt (ss - s**2)) <$> ma r <*> sqma r
      where
        ma r = online identity (.*r)
        sqma r = online (**2) (.*r)

[perf](https://hackage.haskell.org/package/perf)
================================================

1 cycle = 0.4 nanoseconds.

    sum to 1,000
    run                       first     2nd     3rd     4th     5th  40th %
    sumInt [0..]            6.064e3 1.746e3 1.560e3 1.540e3 1.626e3  1.544e3 cycles
    sumDouble [0..]         7.835e5 3.032e5 3.104e5 2.837e5 3.051e5  8.957e4 cycles
    sumPoly [0..]           1.139e5 7.660e4 7.638e4 7.648e4 7.636e4  7.674e4 cycles
    sum Int                 1.601e4 1.186e4 1.167e4 1.158e4 1.176e4  1.168e4 cycles
    sum Double              2.756e4 1.189e4 1.158e4 1.163e4 1.158e4  1.159e4 cycles
    sum Poly                1.170e4 1.172e4 1.163e4 1.166e4 1.158e4  1.164e4 cycles
    fold sum                1.177e4 1.174e4 1.178e4 1.185e4 1.177e4  1.175e4 cycles
    fold av                 2.935e4 1.190e4 1.181e4 1.182e4 1.177e4  1.181e4 cycles
    fold ma                 1.289e4 1.202e4 1.205e4 1.203e4 1.206e4  1.201e4 cycles
    fold std                2.052e5 1.210e5 7.236e5 1.364e5 1.348e5  1.324e5 cycles
    fold maL1               8.218e4 1.192e5 1.052e5 2.567e5 1.168e5  7.856e4 cycles
    fold absmaL1            3.405e5 5.966e4 5.976e4 6.025e4 5.939e4  5.975e4 cycles

recipe
======

    stack build --test --exec "$(stack path --local-install-root)/bin/online-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md other/readme_.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
