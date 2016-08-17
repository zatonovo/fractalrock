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

