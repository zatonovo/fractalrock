\name{fractalrock-package}
\alias{fractalrock-package}
\alias{fractalrock}
\docType{package}
\title{
  A package for generating time series data using fractals
}
\description{
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

  At a later date, implementation of the [modified] rescaled range statistic
  will be included to provide more analytical insight into the time series data
  produced by this package.
}
\details{
\tabular{ll}{
Package: \tab fractalrock\cr
Type: \tab Package\cr
Version: \tab {version}\cr
Date: \tab {date}\cr
License: \tab GPL-3\cr
}
To generate a set of asset prices, the function \code{\link{getPortfolioPrices}}is the most direct way to accomplish this. An xts object will be returned with
one time series per 'asset' provided. In addition, the dates will be coerced
to fit a given business day calendar based on timeDate.

Investigation into fractals via this package is best accomplished by calling the
underlying \code{\link{fractal}} function. This function produces raw values
useful for analysis of the fractal generation process.
}
\author{
Brian Lee Yung Rowe <r@zatonovo.com>
}
\references{
  M. Frame, B. Mandelbrot, N. Neger. Fractal Geometry. 2009.
  http://classes.yale.edu/fractals/
}
\keyword{ package }
\keyword{ ts }
\keyword{ math }

