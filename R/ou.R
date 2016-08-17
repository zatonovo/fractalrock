# Deprecated
ou.process <- function(theta, mu=0, sigma=1, initial=mu,
  end=Sys.Date(), start=NULL, obs=NULL)
{
  dates <- getTradingDates(end, start, obs)

  if (is.null(obs)) obs <- length(dates)
  ts <- 1:obs
  series <- initial * exp(-theta *ts) + mu * (1 - exp(-theta * ts))
  noise <- rnorm(obs, sd=sqrt(sigma^2 / (2*theta) * (1-exp(-2*theta*ts)) ) )
  xts(series + noise, order.by=dates)
}

#' Ornstein-Uhlenbeck process
#'
#' @name ou
#' @param n Number of samples to generate
#' @param s Initial value
#' @param lambda The shock, in annualized terms
#' @param mu The mean-reverting value
#' @param sigma The annualized standard deviation of the noise term
ou(n, s=0.04, lambda=3, mu=0.01, sigma=0.03) %as% {
  ts <- 1:n
  series <- s * exp(-lambda/252 * ts) + mu * (1 - exp(-lambda/252 * ts))
  noise <- rnorm(n, sd=sqrt(sigma^2 * (1-exp(-lambda/252)) / (2*lambda)))
  series + noise
}

