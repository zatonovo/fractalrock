# References
#   http://en.wikipedia.org/wiki/Uhlenbeck-Ornstein_process
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

ou(n, s=0.04, lambda=3, mu=0.01, sigma=0.03) %as% {
  ou(n, s, lambda, mu, sigma, s)
}

ou(0, s, lambda, mu=0.01, sigma=0.03, acc) %as% acc

ou(n, s, lambda, mu=0.01, sigma=0.03, acc) %as% {
  s1 <- s * exp(-lambda/252) + mu * (1 - exp(-lambda/252))
    + sigma * sqrt((1 - exp(-lambda/252)) / (2 * lambda)) * rnorm(1)
  ou(n-1, s1, lambda, mu, sigma, c(acc, s1))
}
