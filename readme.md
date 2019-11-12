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

$$ ema\_t = ema\_{t-1} \* 0.9 + x\_t \* 0.1 $$

The 0.1 is akin to the learning rate in machine learning, or 0.9 can be
thought of as a decaying or a rate of forgetting. An exponential moving
average learns about what the value of x has been lately, where lately
is, on average, about 1/0.1 = 10 x's ago. All very neat.

The first bit of neat is speed. There's 2 times and a plus. The next is
space: an ema is representing the recent xs in a size as big as a single

x\. Compare that with a simple moving average where you have to keep the
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

runs: 100 summing to: 1000

| run          |   first|  second|   third|  average|  median|
|:-------------|-------:|-------:|-------:|--------:|-------:|
| sumInt'      |  6.97e3|  2.37e3|  2.28e3|   2.33e3|  2.28e3|
| sumDouble'   |  1.11e6|  1.70e5|  1.70e5|   1.63e6|  3.30e4|
| sumPoly'     |  3.02e4|  2.96e4|  2.97e4|   4.87e4|  2.99e4|
| sumInt       |  1.66e4|  1.19e4|  1.17e4|   1.18e4|  1.17e4|
| sumDouble    |  2.58e4|  1.20e4|  1.20e4|   1.22e4|  1.22e4|
| sumPoly      |  1.23e4|  1.22e4|  1.22e4|   1.19e4|  1.20e4|
| rSumSum      |  1.23e4|  1.23e4|  1.23e4|   1.22e4|  1.22e4|
| rAvTestMain  |  1.27e4|  1.23e4|  1.22e4|   1.22e4|  1.22e4|
| rMaTest      |  1.27e4|  1.23e4|  1.23e4|   1.30e4|  1.23e4|
| rStdTest     |  1.97e5|  9.66e5|  1.92e5|   1.87e5|  1.10e5|
| rMaL1Test    |  8.28e4|  7.97e4|  4.26e5|   1.37e5|  7.97e4|
| rabsmaL1Test |  6.00e4|  5.96e4|  5.95e4|   1.09e5|  5.96e4|

recipe
======

    stack build --test --exec "$(stack path --local-install-root)/bin/online-bench"