
#' Geometric Brownian motion
#'
#' @name gbm
#' @param n Number of samples to generate
#' @param s0 Initial value
#' @param mu The mean-reverting value
#' @param sigma The annualized standard deviation of the noise term
gbm(n, s0=10, mu=0.01, sigma=0.03) %as% {
  cumprod(c(s0, exp((mu - sigma^2/2) / 252 + sigma*rnorm(n-1) / sqrt(252))))
}

#' Wiener process
#'
#' @name wiener
#' @param n Number of samples to generate
#' @param s Initial value
#' @param mu The mean-reverting value
#' @param sigma The annualized standard deviation of the noise term
wiener(n, s=10, mu=0.01, sigma=0.03) %as% {
  wiener(n, s, mu, sigma, s)
}

wiener(0, s, mu=0.01, sigma=0.03, acc) %as% acc

wiener(n, s, mu=0.01, sigma=0.03, acc) %as% {
  #s1 <- s * mu / 252 + sigma * rnorm(1) / sqrt(252)
  s1 <- s * (1 + mu / 252) + sigma * rnorm(1) / sqrt(252)
  wiener(n-1, s1, mu, sigma, c(acc, s1))
}




