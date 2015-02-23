# Generate time series data based on different generators
# Author: Brian Lee Yung Rowe

#' @examples
#' mygbm <- function(x) gbm(x, 40, .03/1440)
#' ps <- rprices(mygbm, obs=100)
rprices(process, start, ohlc, volume, ...) %::% Function:.:logical:logical:...:xts
rprices(process, start=Sys.Date(), ohlc=FALSE, volume=FALSE, ...) %as% {
  dates <- trading_dates(start=start, ...)
  n <- length(dates)
  prices <- as.xts(process(n), order.by=dates)
  #rownames(prices) <- format(dates)
  colnames(prices) <- 'close'

  if (ohlc) prices <- .add_ohlc(prices, ohlc)
  if (volume) prices <- .add_volume(prices, volume)
  prices
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


#' @param period The date period, defaulting to 1 day (1440 minutes).
#' Units are in minutes, so values less than 1440 will result in 
#' intraday time steps being added.
#' @examples
#' getTradingDates('2009-02-24',obs=10)
getTradingDates <- function(...) {
  flog.info("This function is deprecated. Use trading_dates instead.")
  trading_dates(...)
}


trading_dates(start, end, calendar=holidayNYSE) %::% a:a:Function:Date
trading_dates(start, end, calendar=holidayNYSE) %as% {
  start <- as.Date(start)
  end <- as.Date(end)
  dates <- timeSequence(from=start, to=end)
  dates <- dates[isBizday(dates, holidays=calendar(unique(year(dates))))]
  as.Date(dates)
}

trading_dates(start, obs, calendar=holidayNYSE) %::% a:numeric:Function:Date
trading_dates(start, obs, calendar=holidayNYSE) %as% {
  start <- as.Date(start)
  # This is to get enough dates to account for holidays and weekends
  shimmed <- ceiling(obs * 2)
  dates <- timeSequence(from=start, length.out=shimmed)
  dates <- as.Date(dates[isBizday(dates, holidays=calendar(unique(year(dates))))])
  dates <- dates[dates >= start]
  dates <- dates[1:obs]
}

trading_dates(start, obs, period, hours.fn) %::% a:numeric:numeric:Function:POSIXt
trading_dates(start, obs, period=1, hours.fn) %as% {
  dates <- trading_dates(start, obs, hours.fn)
  hours <- hours.fn(dates)
  ts <- lapply(dates, 
    function(d) as.POSIXct(d) + intraday_ticks(period, hours[d]))
  unique(do.call(c, ts))
}

# th <- function(x) trading_hours(x,'cme')
# trading_dates('2014-06-30','2014-01-01',5, th)
trading_dates(start, end, period, hours.fn) %::% a:a:numeric:Function:POSIXt
trading_dates(start, end, period=1, hours.fn) %as% {
  dates <- trading_dates(start, end, hours.fn)
  hours <- hours.fn(dates)
  ts <- lapply(dates, 
    function(d) as.POSIXct(d) + intraday_ticks(period, hours[d]))
  unique(do.call(c, ts))
}


# OBSOLETE
.trading_dates <- function(end, start=NULL, obs=NULL, calendar=holidayNYSE, period=1440)
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

