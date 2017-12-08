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
    sumInt [0..]             8.69e3  1.59e3  1.54e3  1.60e3  1.64e3 1.57e3 cycles
    sumDouble [0..]          3.00e5  2.82e5  3.27e5  2.99e5  2.61e5 9.38e4 cycles
    sumPoly [0..]            9.34e4  1.05e5  1.09e5  1.18e5  1.64e5 7.81e4 cycles
    sum Int                  1.67e4  1.17e4  1.16e4  1.17e4  1.16e4 1.17e4 cycles
    sum Double               2.63e4  1.18e4  1.17e4  1.16e4  1.16e4 1.16e4 cycles
    sum Poly                 1.28e4  1.18e4  1.17e4  1.17e4  1.18e4 1.17e4 cycles
    fold sum                 1.23e4  1.19e4  1.18e4  1.17e4  1.18e4 1.18e4 cycles
    fold av                  1.23e4  1.18e4  1.18e4  1.18e4  1.18e4 1.18e4 cycles
    fold ma                  1.24e4  1.19e4  1.20e4  1.19e4  1.19e4 1.19e4 cycles
    fold std                 2.04e5  1.15e5  6.14e5  1.16e5  1.16e5 1.12e5 cycles
    fold maL1                9.11e4  8.32e4  1.27e5  3.34e5  1.04e5 1.02e5 cycles
    fold absmaL1             6.82e4  6.71e4  6.64e4  6.66e4  2.97e5 6.65e4 cycles

recipe
======

    stack build --test --exec "$(stack path --local-install-root)/bin/online-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md other/readme_.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
