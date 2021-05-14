
#' Geometric Brownian motion
#'
#' @name gbm
#' @param n Number of samples to generate
#' @param s0 Initial value
#' @param mu The mean-reverting value
#' @param sigma The annualized standard deviation of the noise term
gbm(n, s0, mu, sigma, steps=252, mod_fn=NULL) %:=% {
  xs <- c(s0, exp((mu - sigma^2/2) / steps + sigma*rnorm(n-1) / sqrt(steps)))
  if (!is.null(mod_fn)) { xs <- mod_fn(xs) }
  cumprod(xs)
}

#' Wiener process
#'
#' @name wiener
#' @param n Number of samples to generate
#' @param s0 Initial value
#' @param mu The mean-reverting value
#' @param sigma The annualized standard deviation of the noise term
#' @param steps Number of steps in "annualized" period.
wiener(n, s0, mu, sigma, steps=252, mod_fn=NULL) %:=% {
  xs <- c(s0, (1 + mu / steps) + sigma * rnorm(n-1) / sqrt(steps))
  if (!is.null(mod_fn)) { xs <- mod_fn(xs) }
  cumprod(xs)
}
