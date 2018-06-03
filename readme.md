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

performance benchmark
=====================

1 cycle = 0.4 nanoseconds.

    sum to 1,000
    run                        first      2nd      3rd   median      av.
    rSumInt'                  9.72e3   1.68e3   1.56e3   1.63e3   1.71e3
    rSumDouble'               1.41e6   2.94e5   3.04e5   9.19e4   1.84e5
    rSumPoly'                 9.17e4   9.15e4   9.13e4   7.90e4   1.10e5
    rSumInt                   1.58e4   1.18e4   1.17e4   1.17e4   1.17e4
    rSumDouble                2.62e4   1.18e4   1.16e4   1.16e4   1.18e4
    rSumPoly                  1.17e4   1.17e4   1.16e4   1.16e4   1.16e4
    rSumSum                   1.16e4   1.16e4   1.17e4   1.16e4   1.16e4
    rAvTestMain               2.85e4   1.19e4   1.19e4   1.19e4   1.20e4
    rMaTest                   1.26e4   1.20e4   1.20e4   1.20e4   1.27e4
    rStdTest                  2.14e5   1.26e5   8.05e5   1.16e5   2.03e5
    rMaL1Test                 1.73e5   8.30e4   1.14e5   7.61e4   1.21e5
    rabsmaL1Test              3.34e5   5.89e4   5.93e4   5.93e4   1.06e5

recipe
======

    stack build --test --exec "$(stack path --local-install-root)/bin/online-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax"
