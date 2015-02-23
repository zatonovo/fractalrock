FractalRock
===========
Asset price simulation using fractals and other approaches. 

Simulating stock market prices and returns can be accomplished using a number
of techniques. Most commonly, geometric brownian motion (aka a random walk) is  used to simulate stock prices. Using this technique results in a normal
distribution of price returns. As an alternative technique, it is possible to
generate price series using fractals. The advantage is that price returns
tend to have volatility that clusters, similar to actual returns.

The basic principle driving fractal generation of time series is that data is
generated iteratively based on increasing levels of resolution. The initial
series is defined by a so-called initiator pattern and then generators are
used to replace each segment of the initial pattern. Regular, repeatable
patterns can be produced by using the same seed and generators. By using a set  of generators, non-repeatable time series can be produced. This technique is
the basis of the fractal time series process in this package.

Usage
-----
As random generators, most of the functions in this package follow the R
convention of using `r` as a prefix to denote a random number generator.
The available generators include:

+ rprices
+ rintraday

Stochastic processes
--------------------
All price generators use a stochastic process to generate the sequence of
random numbers.

### Fractal time series
Mandelbrot not only pioneered fractals but also used them to investigate
volatility and risk in financial markets.

### Geometric brownian motion
GBM is another name for a random walk.

```R
mygbm <- function(x) gbm(x, 40, .03/1440)
```


### Ornstein-Uhlenbeck
An OU process is typically used to simulate interest rates.

Daily price simulation
----------------------
Any process can be used to generate prices. The rprices function attaches
dates to the random sequence so it can be analyzed as an asset price series.

```R
mygbm <- function(x) gbm(x, 40, .03/1440)
ps <- rprices(mygbm, obs=100)
```

Intraday price simulation
-------------------------
Generating intraday prices involves specifying a stochastic process along
with some parameters indicating how many points to generating and how to
map the points in the series to time. The `trading_hours` function knows
about exchange holidays and trading hours to give a realistic price series.

```R
th <- function(x) trading_hours(x,'cme')
seed <- rintraday(mygbm, 60, th)

> head(seed)
              close
2015-02-23 40.00000
2015-02-24 39.95202
2015-02-25 40.03372
2015-02-26 40.13978
2015-02-27 40.17472
2015-03-02 40.04889
```

An end date can be specified instead of the number of observations.
These definitions are typed via `lambda.r`, so it's not necessary to
name either of these parameters, although for clarity you may choose
to do so.
```R
seed <- rintraday(mygbm, as.Date('2015-03-05'), th)
> range(index(seed))
[1] "2015-02-23" "2015-03-05"
```

To specify an explicit date range, use this signature:
```R
seed <- rintraday(mygbm, start='2015-01-01', end='2015-03-01', th)
> range(index(seed))
[1] "2015-01-01" "2015-02-27"
```


### Correlated random numbers

FractalRock now supports generating correlated random numbers. This usage
involves simulating a seed series and then generating a set of correlated
series.

First we create the trading hours generator, followed by a call to 
`rintraday` to generate the seed series.

```R
th <- function(x) trading_hours(x,'cme')
seed <- rintraday(mygbm, obs=60, th)
```
Now we specify the correlation matrix.
```R
cmat <- matrix(c(1,0,0, .8,1,0, .6,.4,1), ncol=3)

> cmat
     [,1] [,2] [,3]
[1,]    1  0.8  0.6
[2,]    0  1.0  0.4
[3,]    0  0.0  1.0
```

Finally, we generate the correlated price series.

```R
z <- rintraday(seed, cmat)

> cor(z)
          [,1]      [,2]      [,3]
[1,] 1.0000000 0.8128455 0.6432900
[2,] 0.8128455 1.0000000 0.3788305
[3,] 0.6432900 0.3788305 1.0000000
```

Complete OHLC bars can be generated for correlated series as well. The
OHLC option specifies the standard deviation for the HLC series.
Instead of an `xts` object, a list of `xts` objects are returned,
one for each time series.

```R
th <- function(x) trading_hours(x,'cme')
seed <- rintraday(mygbm, obs=60, th)
cmat <- matrix(c(1,0,0, .8,1,0, .6,.4,1), ncol=3)
z <- rintraday(seed, cmat, ohlc=1, volume=100)

> head(z[[2]])
              close     open      low     high volume
2015-02-23 40.03911 40.18967 39.11649 40.34437    111
2015-02-24 39.94562 40.03911 39.18681 40.17525     94
2015-02-25 40.05002 39.94562 38.61717 40.39037    103
2015-02-26 40.10920 40.05002 39.41671 41.46493    109
2015-02-27 40.19329 40.10920 39.83513 40.58183     58
2015-03-02 40.04908 40.19329 39.72885 41.37340    128
```


Iterative function systems
--------------------------

Further Work
------------
At a later date, implementation of the [modified] rescaled range statistic
will be included to provide more analytical insight into the time series data
produced by this package.


History
-------
This package started off as a way to explore generating time series
using fractals. It has since evolved to encompass other forms of
asset price simulation.
