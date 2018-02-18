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
    sumInt [0..]            1.008e4 1.658e3 1.560e3 1.540e3 1.612e31.618e3 cycles
    sumDouble [0..]         4.790e5 5.350e5 3.570e5 3.189e5 3.737e59.083e4 cycles
    sumPoly [0..]           9.256e4 9.012e4 9.000e4 8.985e4 8.979e47.757e4 cycles
    sum Int                 1.662e4 1.182e4 1.169e4 1.170e4 1.175e41.163e4 cycles
    sum Double              2.700e4 1.172e4 1.166e4 1.161e4 1.163e41.163e4 cycles
    sum Poly                1.232e4 1.178e4 1.174e4 1.178e4 1.178e41.191e4 cycles
    fold sum                1.229e4 1.176e4 1.181e4 1.175e4 1.181e41.176e4 cycles
    fold av                 1.241e4 1.187e4 1.180e4 1.184e4 1.183e41.179e4 cycles
    fold ma                 1.285e4 1.196e4 1.188e4 1.497e4 1.344e41.291e4 cycles
    fold std                7.486e5 4.159e5 1.235e6 1.811e5 1.431e51.124e5 cycles
    fold maL1               9.295e4 8.391e4 1.106e5 3.845e5 8.382e48.319e4 cycles
    fold absmaL1            6.897e4 6.648e4 6.660e4 6.699e4 3.197e56.656e4 cycles

recipe
======

    stack build --test --exec "$(stack path --local-install-root)/bin/online-bench" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/header.md other/readme_.md other/footer.md -t html -o index.html --filter pandoc-include --mathjax" --exec "$(stack path --local-bin)/pandoc -f markdown -i other/readme_.md -t markdown -o readme.md --filter pandoc-include --mathjax" --file-watch
