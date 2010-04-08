\name{process}
\alias{ou.process}
\title{ Generate a time series based on stochastic processes }
\description{
  A collection of functions to produce time series using stochastic processes.
}
\usage{
ou.process(theta, mu = 0, sigma = 1, initial=mu, end = Sys.Date(), start = NULL, obs = NULL)
}
\arguments{
  \item{theta}{ Rate of dissipation }
  \item{mu}{ Mean }
  \item{sigma}{ Volatility }
  \item{initial}{ Initial value }
  \item{end}{ The end date }
  \item{start}{ The starting date }
  \item{obs}{ Number of observations to produce }
}
\details{
  The 'ou.process' function generates a mean-reverting time series according to
  the Ornstein-Uhlenbeck process.
}
\value{
  An xts object containing a time series of values representing asset prices
  whose evolution is defined by the given process.
}
\references{
http://en.wikipedia.org/wiki/Ornstein\%E2\%80\%93Uhlenbeck_process
}
\author{ Brian Lee Yung Rowe }
\examples{
data(generators)
series <- ou.process(1, 1.2, 0.3, obs=50)
}
\keyword{ ts }
\keyword{ math }

