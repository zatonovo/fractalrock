# Generate time series data based on different generators
# Author: Brian Lee Yung Rowe

# Example
# getTradingDates('2009-02-24',obs=10)
getTradingDates <- function(end, start=NULL, obs=NULL, calendar=holidayNYSE)
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
  return(as.Date(dates))
}

# Generate n price series using the specified method
# Example
# getPortfolioPrices('TWM', '2009-02-24',obs=10, seed=seed, patterns=pats)
getPortfolioPrices <- function(symbols, obs=NULL, end=Sys.Date(), start=NULL,
  calendar=holidayNYSE, seeds=NULL, patterns=NULL, ..., type='uniform')
{
  dates <- getTradingDates(end, start, obs, calendar)
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

# Code to make data
#seed.1 <- matrix(c(13460,10, 13507,20, 13566,18, 13600,24), ncol=2, byrow=TRUE)
#seed.2 <- matrix(c(13460,24, 13488,28, 13600,27), ncol=2, byrow=TRUE)
#seed.3 <- matrix(c(13460,48, 13521,45, 13600,52), ncol=2, byrow=TRUE)
#seed.4 <- matrix(c(13460,18, 13517,20, 13528,19, 13600,22), ncol=2, byrow=TRUE)
#seed.5 <- matrix(c(13460,52, 13499,67, 13520,65, 13600,62), ncol=2, byrow=TRUE)
#seed.6 <- matrix(c(13460,98, 13474,89, 13588,99, 13600,95), ncol=2, byrow=TRUE)
#seed.7 <- matrix(c(13460,85, 13556,81, 13600,83), ncol=2, byrow=TRUE)
#seed.8 <- matrix(c(13460,22, 13522,21, 13534,23, 13549,25, 13600,24), ncol=2, byrow=TRUE)
#seed.9 <- matrix(c(13460,65, 13512,68, 13538,65, 13600,69), ncol=2, byrow=TRUE)
#sampleInitiators <- list(seed.1=seed.1, seed.2=seed.2, 
#  seed.3=seed.3, seed.4=seed.4, seed.5=seed.5, seed.6=seed.6,
#  seed.7=seed.7, seed.8=seed.8, seed.9=seed.9)

#pattern.1 <- matrix(c(0, 0.2, 0.4, 0.1, 0.7,-0.3, 1,0.2), ncol=2, byrow=TRUE)
##pattern.2 <- matrix(c(0,-0.1, 0.3, 0.2, 0.6,-0.2, 1,0.4), ncol=2, byrow=TRUE)
#pattern.3 <- matrix(c(0,-0.3, 0.4,-0.2, 0.7, 0.3, 1,0.2), ncol=2, byrow=TRUE)
#pattern.4 <- matrix(c(0, 0.7, 0.2, 0.4, 0.5, 0.4, 1,0.1), ncol=2, byrow=TRUE)
#pattern.5 <- matrix(c(0, 0.1, 0.2, 0.3, 0.4, 0.3, 1,0.1), ncol=2, byrow=TRUE)
#pattern.6 <- matrix(c(0,-0.2, 0.1, 0.2, 0.5, 0.2, 1,0.1), ncol=2, byrow=TRUE)
#pattern.7 <- matrix(c(0, 0.3, 0.4,-0.1, 0.6, 0.4, 1,0.2), ncol=2, byrow=TRUE)
#pattern.8 <- matrix(c(0, 0.0, 0.3, 0.0, 0.7,-0.2, 1,0.1), ncol=2, byrow=TRUE)
#pattern.9 <- matrix(c(0, 0.4, 0.2, 0.2, 0.6, 0.1, 1,0.3), ncol=2, byrow=TRUE)
#sampleGenerators <- list(pattern.1=pattern.1, pattern.2=pattern.2,
#  pattern.3=pattern.3, pattern.4=pattern.4, pattern.5=pattern.5,
#  pattern.6=pattern.6, pattern.7=pattern.7, pattern.8=pattern.8,
#  pattern.9=pattern.9)
#save(sampleInitiators, sampleGenerators, file='fractalrock/data/generators.RData')

#ps <- fractal(seed, pats, 4)
#ps <- fractal(seed, pats, 4, only=4)
#plotReturns(ps)

# Simulate order sign
# (Incomplete)
#microInitiators <- list(
#  seed.1=matrix(c(600.0, 1, 602.4,-1, 603.4,-1,  610.0,-1), ncol=2,byrow=TRUE),
#  seed.2=matrix(c(600.0,-1, 601.2,-1, 602.8, 1,  610.0, 1), ncol=2,byrow=TRUE),
#  seed.3=matrix(c(600.0, 1, 603.1, 1, 604.2,-1,  610.0, 1), ncol=2,byrow=TRUE)
#)
#microGenerators <- list(
##  pattern.1=matrix(c(0, 1, 0.22, 1, 0.61,-1,  1,-1), ncol=2,byrow=TRUE),
##  pattern.2=matrix(c(0,-1, 0.40, 1, 0.63, 1,  1, 1), ncol=2,byrow=TRUE),
##  pattern.3=matrix(c(0,-1, 0.55,-1, 0.83,-1,  1, 1), ncol=2,byrow=TRUE),
##  pattern.4=matrix(c(0,-1, 0.55,-1, 0.83,-1,  1, 1), ncol=2,byrow=TRUE)
#)
#ps <- getPortfolioPrices('IBM',10, seeds=microInitiators, patterns=microGenerators, date.fun=as.POSIXct)

# Unused
#.fracret <- function(assets=10, epochs=3)
#{
#  fn <- function(x)
#  {
#    series <- fractal(seed, pats, epochs=epochs)
#    series <- Delt(series)
#    series <- series[! is.na(series) & ! is.infinite(series)]
#    series <- series - mean(series)
#  }
#  rets <- do.call(cbind, lapply(1:assets, fn))
#  names(rets) <- 1:assets
#  rets
#}

#.interpolate.mat <- function(x, by.col)
#{
#  
#}



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
