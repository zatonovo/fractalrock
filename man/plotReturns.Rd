\name{plotReturns}
\alias{plotReturns}
\title{ Plot asset prices and returns for fractal analysis }
\description{
  This is a convenience function for studying the generated time series by the
  fragtalrock package. Given a time series of prices, plotReturns will plot
  both the original time series of prices and the returns series. This is a
  useful visual aid in determining the utility of the simulated time series.
}
\usage{
plotReturns(series, ...)
}
\arguments{
  \item{series}{ A time series }
  \item{\dots}{ Additional arguments to pass to plot }
}
%\details{
%}
\value{
  Invisibly returns the original series
}
\author{ Brian Lee Yung Rowe }
\examples{
  data(sampleInitiators)
  data(sampleGenerators)
  ps <- fragtal(sampleInitiator, sampleGenerators, epochs=3)
  plotReturns(ps)
}
\keyword{ ts }
