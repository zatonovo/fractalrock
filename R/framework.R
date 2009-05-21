# Generate time series data based on different generators
# Author: Brian Lee Yung Rowe
#require(futile)

# Example
# getTradingDates('2009-02-24',obs=10)
getTradingDates <- function(end, start=NULL, obs=NULL, calendar=holidayNYSE)
{
  require(futile)
  require(timeDate, quietly=TRUE)
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
  require(futile)
  # Get dates
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
    # Call fragtal, and cbind
    if (! exists('count') & ! exists('epochs'))
      ts <- fragtal(seeds, patterns, ..., count=obs, type=type)
    else
      ts <- fragtal(seeds, patterns, ..., type=type)

    #ts <- ts[(nrow(ts)-length(dates)+1):nrow(ts),]
    ts <- tail(ts, anylength(dates))
    index(ts) <- dates
    set <- cbind(set, ts)
  }
  anynames(set) <- symbols
  return(set)
}

# count - total number of points to generate (will truncate to achieve exact
# number
# epochs - number of iterations to run. Note that the count grows quickly.
# Example
# Get 10 points
# ps <- fragtal(seed, pats, 10)
# Get 3 epochs
# ps <- fragtal(seed, pats, epochs=3)
fragtal <- function(seeds, patterns, count=NULL, epochs=NULL, ..., type='uniform')
{
  require(xts, quietly=TRUE)
  if ('list' %in% class(seeds)) seed <- sample(seeds, 1)
  else seed <- seeds

  do.call(paste('fragtal.',type, sep=''), list(seed,patterns,count,epochs,...))
}

# ps <- fragtal.uniform(seed,pats, 2)
# ps <- fragtal.uniform(seed,pattern.3, 1); plot(ps, type='l')
# origin - The date at which to start time series. Default is the beginning of
# the unix epoch, but any valid date is allowed.
# only - Only use the given index in pattern
# Example
# Get 10 points
# ps <- fragtal.uniform(seed, pats, 10)
# Get 3 epochs
# ps <- fragtal.uniform(seed, pats, epochs=3)
fragtal.uniform <- function(seed, patterns, count=NULL, epochs=NULL,
  origin='1970-01-01', only=NULL)
{
  require(futile)
  if (! 'list' %in% class(patterns))
  {
    patterns <- list(pattern.1=patterns)
  }

  # Calculate count based on size of seed and patterns
  if (! is.null(epochs))
  {
    use.count <- FALSE
    count <- (length(seed) - 1) * (length(patterns[[1]])-1)^epochs
  }
  else if (! is.null(count))
  {
    use.count <- TRUE
    seed.legs <- nrow(seed)-1
    pattern.legs <- nrow(patterns[[1]])-1
    epochs <- floor(log(count / seed.legs, base=pattern.legs)) + 1
    #cat("seed.legs:",seed.legs,"; pattern.legs:",pattern.legs,"\n")
    if (logLevel() > 0) cat("Set epochs to",epochs,"\n")
  }

  for (dummy in 1:epochs)
  {
    rows <- nrow(seed)
    next.seed <- seed
    for (idx in 2:rows)
    {
      if (! is.null(only)) pattern <- patterns[[only]]
      else pattern <- patterns[[sample(length(patterns),1)]]

      x.delta <- seed[idx,1] - seed[idx-1,1]
      y.delta <- seed[idx,2] - seed[idx-1,2]
      scale <- c(x.delta, y.delta)
      start <- c(seed[idx-1,1], seed[idx-1,2])

      if (logLevel() > 1)
      {
        cat("[",dummy,".",idx,"]",sep=''); cat(" scale:",scale,"\n")
        cat("[",dummy,".",idx,"]",sep=''); cat(" start:",start,"\n")
      }
      segment <- pattern * 
        matrix(rep(scale, nrow(pattern)), ncol=2, byrow=TRUE) +
        matrix(rep(start, nrow(pattern)), ncol=2, byrow=TRUE)
      next.seed <- rbind(next.seed[! (next.seed[,1] %in% segment[,1]), ], segment)
      seed <- rbind(seed[! (seed[,1] %in% segment[,1]), ],
        segment[(segment[,1] %in% seed[,1]), ])
      seed <- seed[order(seed[,1]),]
      if (logLevel() > 1)
      { cat("[",dummy,".",idx,"]",sep=''); cat(" segment:",segment,"\n") }
    }
    seed <- next.seed[order(next.seed[,1]),]
  }
  seed <- unique(seed)
  #seed[,1] <- as.Date(seed[,1], origin=origin)
  seed <- xts(seed[,2], order.by=as.Date(seed[,1], origin=origin))
  if (use.count) seed <- tail(seed, count)
  return(seed)
}

fragtal.random <- function(seed, patterns, count)
{
  if (! 'list' %in% class(patterns))
  {
    patterns <- list(pattern.1=patterns)
  }

  for (dummy in 1:count)
  {
    pattern <- patterns[[sample(length(patterns),1)]]
    idx <- sample((nrow(seed)-1), 1) + 1
    #cat("Got index",idx,"\n")

    x.delta <- seed[idx,1] - seed[idx-1,1]
    y.delta <- seed[idx,2] - seed[idx-1,2]
    scale <- c(x.delta, y.delta)
    start <- c(seed[idx-1,1], seed[idx-1,2])
    segment <- pattern * 
      matrix(rep(scale, nrow(pattern)), ncol=2, byrow=TRUE) +
      matrix(rep(start, nrow(pattern)), ncol=2, byrow=TRUE)
    if (idx <= 2)
    {
      next.seed <- rbind(segment, seed[idx:nrow(seed),])
    }
    else if (idx == nrow(seed))
    {
      next.seed <- rbind(seed[1:(idx-2),], segment)
    }
    else
    {
      next.seed <- rbind(seed[1:(idx-2),], segment, seed[(idx+1):nrow(seed),])
    }
    seed <- next.seed[order(next.seed[,1]),]
  }
  return(unique(seed))
}


plotReturns <- function(series, ...)
{
  require(quantmod, quietly=TRUE)
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
#save(sampleInitiators, sampleGenerators, file='fragtalrock/data/generators.RData')

#ps <- fragtal(seed, pats, 4)
#ps <- fragtal(seed, pats, 4, only=4)
#plotReturns(ps)

