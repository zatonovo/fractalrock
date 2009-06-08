\name{fractal}
\alias{fractal}
\alias{fractal.uniform}
\alias{fractal.random}
\alias{next.seeds}
\title{ Create time series based on fractal generators }
\description{
  The fractal function generates a time series of points using basic principles
  of fractal patterns. Fractal generation can be used to simulate a time series
  of asset prices, which has been shown to better reflect the distribution of
  returns than using a Gaussian random walk. Any number of points can be
  generated based on specifying the total count or by running over a number of
  epochs. The range of the data is defined by the given seed for the generation
  plus the available patterns.
}
\usage{
fractal(seeds, patterns, count = NULL, epochs = NULL, ..., type = "uniform")

fractal.uniform(seed, patterns, count = NULL, epochs = NULL, origin = '1970-01-01', date.fun = as.Date, only = NULL)

fractal.random(seed, patterns, count)

next.seeds(old.seed, new.seed, pattern, idx, epoch)
}
\arguments{
  \item{seeds}{ A list of seed patterns to use for generating the time series }
  \item{seed}{ The seed pattern to use for generating the time series }
  \item{patterns}{ A single pattern or list of patterns that get randomly selected for each segment being replaced }
  \item{count}{ The total number of points to create. Either count or epochs must be provided. Specifying count indirectly sets the number of epochs to run and truncates the data appropriately to get the specified number of points. }
  \item{epochs}{ The total number of epochs to run. Either count or epochs must be populated. Using epochs is good for experimentation to visualize what happens at every stage of the generation. }
  \item{origin}{ The starting date for the generated time series }
  \item{date.fun}{ The function to use to parse dates and/or times }
  \item{only}{ Only use the nth pattern instead of randomly choosing from patterns }
  \item{...}{ Additional arguments to pass to underlying function }
  \item{type}{ The type of generation to perform. Uniform descends each level in a uniform manner (meaning all segments get replaced) whereas the random generation will randomly select segments to replace during each epoch. }
  \item{old.seed}{(Internal) Previous seed used to generate pattern}
  \item{new.seed}{(Internal) Next seed used to generate pattern}
  \item{pattern}{(Internal) Available patterns to use}
  \item{idx}{(Internal) Index of current iteration}
  \item{epoch}{(Internal) Current epoch}
}
\value{
  An xts object containing a time series of values representing asset prices
}
\references{
  M. Frame, B. Mandelbrot, N. Neger. Fractal Geometry. 2009.
  http://classes.yale.edu/fractals/
}
\author{ Brian Lee Yung Rowe }
\examples{
data(generators)
series <- fractal(sampleInitiators, sampleGenerators, count=10)

# View the results of a single iteration using the second pattern
series <- fractal(sampleInitiators, sampleGenerators, epochs=1, only=2)
}
\keyword{ ts }
\keyword{ math }

