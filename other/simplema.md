
motivation: a simple moving average
---

To start somewhere, let's deconstruct the calculation of a simple moving average. Here's an average:

    av xs = (sum xs/length xs)
    -- av [0..10] == 5.0

Imagine that there is a data stream, arriving in an sequence (an ordering from first to last).  As the data values arrive, a state variable is updated which is the moving average of the last 3 values (ma3).  To calculate ma3, the values so far would be multiplied by the following weights, summed, and divided by 3.

When 6 values have been consumed so far, the ma3 is:

    sum(xs * [0 0 0 1 1 1])/3

The next value streams in, and the ma3 calculation weights now looks like this:

    0 0 0 0 1 1 1

The difference between these weights and the previous weights is:

    w = [0 0 0 -1 0 0 1]

So that the ma3 can be calculated as $ma3_t = ma3_{t-1} + \sum (w * xs)/3$


a better moving average
---

There are other weighting schemes, and a better one than the simple moving average is the exponential moving average, where the weights look like this:

    0.1 * [... (0.9^2) (0.9^1) (0.9^0) _]

    0.1 * [... (0.9^3) (0.9^2) (0.9^1) 1]

So that the difference is:

    0.1 * [... (-0.1 * 0.9^2) (-0.1 * 0.9^1) (-0.1 * 0.9^0) 1]

And, as it happens, $ema_{t} = 0.9 * ema_{t-1} + 0.1 * x_t$

This state variable (or statistic) is quite compact (one state variable being the current ma) compared with the simple moving average where the last n values need to be remembered in order to drop the nth oldest each update.  This also makes it blaxingly fast compared to the simple average update.

More generally, the baseline narrative of an online moving average - what have the values averaged lately - tends towards preferring the exponential version as:

- very old values are almost forgotten ie not be influencing the current moving average much at all, compared with the simple moving average where past values go from having no effect to a lot at an arbitrary point in the history.
- newer values should hold more weight than older ones; weights are monotonicaly decreasing from latest to oldest value.

