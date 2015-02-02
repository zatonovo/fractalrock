# Generate time series data based on different generators
# Author: Brian Lee Yung Rowe

DAILY <- 60 * 24


#' Compatible with POSIXct
#' @return Vector of seconds used to offset a date
intraday_ticks <- function(period) {
  period * 60 * (0:(DAILY/period))
}

#' @param period The date period, defaulting to 1 day (1440 minutes).
#' Units are in minutes, so values less than 1440 will result in 
#' intraday time steps being added.
#' @examples
#' getTradingDates('2009-02-24',obs=10)
getTradingDates <- function(...) {
  flog.info("This function is deprecated. Use trading_dates instead.")
  trading_dates(...)
}

trading_dates <- function(end, start=NULL, obs=NULL, calendar=holidayNYSE, period=1440)
{
  if (is.null(obs) & is.null(start)) stop("Either obs or start must be set")

  end <- as.Date(end)
  if (!is.null(start))
  {
    start <- as.Date(start)
    dates <- timeSequence(from=start, to=end)
    dates <- dates[isBizday(dates, holidays=calendar())]
  }
  else
  {
    # This is to get enough dates to account for holidays and weekends
    shimmed <- ceiling(obs * 1.45)
    start <- as.Date(end) - shimmed + 1
    dates <- timeSequence(from=start, length.out=shimmed)
    dates <- dates[isBizday(dates, holidays=calendar())]
    if (length(dates) < obs)
    {
      # It turns out that there are a lot of holidays so add a few more days
      gap <- (2 + obs - length(dates)) * 2
      start.1 <- as.Date(dates[1] - shimmed)
      dates.1 <- timeSequence(from=start, length.out=gap)
      dates <- c(dates.1[isBizday(dates.1, holidays=calendar())], dates)
    }

    inf <- anylength(dates) - obs + 1
    sup <- anylength(dates)
    dates <- dates[inf:sup]
  }
  ds <- as.Date(dates)
  if (period == 1440) return(ds)
  
  ts <- lapply(ds, function(d) as.POSIXct(d) + intraday_ticks(period))
  unique(do.call(c, ts))
}

# Generate n price series using the specified method
# Example
# getPortfolioPrices('TWM', '2009-02-24',obs=10, seed=seed, patterns=pats)
getPortfolioPrices <- function(...) {
  flog.info("This function is deprecated. Use portfolio_prices instead.")
  portfolio_prices(...)
}

portfolio_prices <- function(symbols, obs=NULL, end=Sys.Date(), start=NULL,
  calendar=holidayNYSE, seeds=NULL, patterns=NULL, ..., type='uniform')
{
  dates <- trading_dates(end, start, obs, calendar)
  if (is.null(seeds))
  {
    data(generators)
    seeds = sample(sampleInitiators, anylength(symbols), TRUE)
  }
  if (is.null(patterns))
  {
    data(generators)
    patterns = sample(sampleGenerators, anylength(symbols), TRUE)
  }

  # Loop over symbols names
  set <- NULL
  for (symbol in symbols)
  {
    # Call fractal, and cbind
    if (! exists('count') & ! exists('epochs'))
      ts <- fractal(seeds, patterns, ..., count=obs, type=type)
    else
      ts <- fractal(seeds, patterns, ..., type=type)

    #ts <- ts[(nrow(ts)-length(dates)+1):nrow(ts),]
    ts <- tail(ts, anylength(dates))
    index(ts) <- dates
    set <- cbind(set, ts)
  }
  anynames(set) <- symbols
  return(set)
}


plotReturns <- function(series, ...) {
  flog.info("This function is deprecated. Use plot_returns instead.")
  plot_returns(series, ...)
}

plot_returns <- function(series, ...)
{
  o.par <- par(mfrow=c(2,1), mar=c(3.1, 2.1, 2.1, 1.1))
  plot(series, type='l', main='Prices',...)
  plot(Delt(series), main='Returns')
  par(o.par)
  invisible(series)
}



#' Create correlated intraday prices
#'
#' @examples
#' myou <- function(x) ou(x, 120, 3/24)
#' seed <- rintraday(myou, obs=60,period=1)
#' cmat <- matrix(c(1,0,0, .8,1,0, .6,.4,1), ncol=3)
#' y <- rintraday(seed, cmat)
rintraday(series, rho) %::% xts : matrix : xts
rintraday(series, rho) %as% {
  if (all(rho[lower.tri(rho)] == 0))
    rho[lower.tri(rho)] <- rho[upper.tri(rho)]

  cd <- chol(nearPD(rho)$mat)
  u <- mean(series)
  s <- sd(series)
  num.cols <- ncol(rho) - 1
  r <- cbind((series - u)/s, 
    matrix(rnorm(nrow(series) * num.cols), ncol=num.cols))
  rc <- r %*% cd
  out <- as.xts(u + rc * s, order.by=index(series))
  colnames(out) <- colnames(rho)
  out
}

#' myou <- function(x) ou(x, 120, 3/24)
#' seed <- rintraday(myou, obs=60,period=1)
#' cmat <- matrix(c(1,0,0, .8,1,0, .6,.4,1), ncol=3)
#' y <- rintraday(seed, cmat, ohlc=1, volume=100)
rintraday(series, rho, ohlc, volume) %::% xts : matrix : . : . : list
rintraday(series, rho, ohlc=FALSE, volume=FALSE) %as% {
  prices <- rintraday(series, rho)
  colnames(prices) <- rep('close',ncol(prices))
  out <- lapply(1:ncol(prices), function(x) prices[,x])

  if (ohlc) out <- lapply(out, function(x) .add_ohlc(x, ohlc))
  if (volume) out <- lapply(out, function(x) .add_volume(x, volume))
  out
}

#' Simulate intraday prices
#'
#' @examples
#' myou <- function(x) ou(x, 40, 3/24, 45, .03/1440)
#' x <- rintraday(myou, obs=60,period=1)
rintraday(process, end=Sys.Date(), ohlc=FALSE, volume=FALSE, ...) %as% {
  dates <- trading_dates(end=end, ...)
  n <- length(dates)
  prices <- as.xts(process(n), order.by=dates)
  #rownames(prices) <- format(dates)
  colnames(prices) <- 'close'

  if (ohlc) prices <- .add_ohlc(prices, ohlc)
  if (volume) prices <- .add_volume(prices, volume)
  prices
}


.add_ohlc <- function(series, sd) {
  n <- nrow(series)
  open <- as.numeric(series$close[1]) + rnorm(1) # This is totally a kludge
  series$open <- c(open,series$close[1:(n-1)])
  series$low <- pmin(series$open,series$close) - abs(rnorm(n, sd=sd))
  series$high <- pmax(series$open,series$close) + abs(rnorm(n, sd=sd))
  series
}

.add_volume <- function(series, mean, sd=mean/4) {
  n <- nrow(series)
  series$volume <- as.integer(abs(rnorm(n, mean=mean, sd=sd)))
  series
}


#' Simulate prices
#'
#' @examples
#' rprices(c("A","B","C"), 10, function(n) rnorm(n))
#' rprices(c("A","B","C"), 10, function(n) gbm(n, runif(1,10,200)))
#' ps <- rprices('CLM14', 3, function(x) ou(x, 45, 3/24), period=60)
rprices(symbols, obs, process, end=Sys.Date(), start=NULL, calendar=holidayNYSE) %as% {
  dates <- trading_dates(end, start, obs, calendar)
  n <- length(dates)
  prices <- sapply(symbols, function(x) process(n))
  rownames(prices) <- format(dates)
  anynames(prices) <- symbols
  as.xts(prices)
}

# beta_a = cov(r_a, r_m) / var(r_m)
# cov(r_a, r_m) = beta_a * var(r_m)
# cor(r_a, r_m) = cov(r_a, r_m) / (sd(r_a) * sd(r_m))
#' Generate a random vector based on a time period and betas
rprices(beta, var=252) %as% {
  n <- 1 + length(beta)
  m <- matrix(rep(0,n^2), nrow=n)
  m[1,] <- c(1, beta)
  m[,1] <- c(1, beta)
  diag(m) <- 1
  cor.mat <- denoise(m, RandomMatrixDenoiser())
  cd <- chol(cor.mat)
  z <- matrix(rnorm(length(m)), nrow=nrow(m))
  v <- t(cd) %*% z
}

# m <- rprices(c(.5,.8,.3,-.3,1.4))
# md <- denoise(m, RandomMatrixDenoiser())
# cd <- chol(md)
# z <- matrix(rnorm(length(m)), nrow=nrow(m))
# v <- t(cd) %*% z
