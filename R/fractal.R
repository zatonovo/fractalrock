rinitiator(xrange=c(0,1), yrange=c(0,100)) %as% {
  segments <- round(runif(1,1,3))
  ymin <- round(runif(1, yrange[1], yrange[2]))
  len <- round(runif(1,3,10))
  ymax <- min(ymin + len, yrange[2])

  mid <- runif(segments, xrange[1], xrange[2])
  x <- c(xrange[1], mid, xrange[2])
  x <- x[order(x)]
  y <- round(runif(segments+2, ymin, ymax))
  cbind(x,y)
}

rgenerator(xrange=c(0,1), yrange=c(-1,1)) %as% {
  segments <- round(runif(1,1,2))

  mid <- round(runif(segments, xrange[1], xrange[2]),digits=1)
  x <- c(xrange[1], mid, xrange[2])
  x <- x[order(x)]
  y <- round(runif(segments+2, yrange[1], yrange[2]), digits=1)
  cbind(x,y)
}

rfractal(n, initiator=rinitiator, generator=rgenerator) %as% {
  next.seed <- this.seed <- initiator()
  row.fn <- function(epoch, idx, max, this.seed, next.seed) {
    if (idx >= max) return(next.seed)
    flog.trace("[%s.%s] Generating segment", epoch,idx)

    pattern <- generator()
    iteration <- next.seeds(this.seed, next.seed, pattern, idx, epoch)
    this.seed <- iteration$this.seed
    next.seed <- iteration$next.seed
    row.fn(epoch, idx+1, max, this.seed, next.seed)
  }
  epoch.fn <- function(epoch, this.seed, next.seed) {
    if (nrow(this.seed) >= n) return(this.seed)
    flog.trace("[%s] Starting new epoch with %s rows", epoch,nrow(this.seed))

    next.seed <- row.fn(epoch, 2, nrow(this.seed), this.seed, next.seed)
    this.seed <- next.seed[order(next.seed[,1]),]

    epoch.fn(epoch + 1, this.seed, next.seed)
  }
  out <- epoch.fn(1, this.seed, next.seed)
  out <- unique(out)
  out[1:n,]
}


next.seeds <- function(old.seed, new.seed, pattern, idx, epoch)
{
  flog.trace("[%s.%s] old.seed:", epoch,idx)
  flog.trace("", old.seed, capture=TRUE)
  x.delta <- old.seed[idx,1] - old.seed[idx-1,1]
  y.delta <- old.seed[idx,2] - old.seed[idx-1,2]
  scale <- c(x.delta, y.delta)
  start <- c(old.seed[idx-1,1], old.seed[idx-1,2])

  flog.debug("[%s.%s] scale: %s,%s",epoch,idx, scale[1],scale[2])
  flog.debug("[%s.%s] start: %s,%s",epoch,idx, start[1],start[2])

  segment <- pattern * 
    matrix(rep(scale, nrow(pattern)), ncol=2, byrow=TRUE) +
    matrix(rep(start, nrow(pattern)), ncol=2, byrow=TRUE)

  # Create new seed by adding the new segment
  new.seed <- rbind(new.seed[! (new.seed[,1] %in% segment[,1]), ], segment)
  flog.debug('nrow(old.seed) = %s', nrow(old.seed))
  old.seed <- rbind(old.seed[! (old.seed[,1] %in% segment[,1]), ],
    segment[(segment[,1] %in% old.seed[,1]), ])
  old.seed <- old.seed[order(old.seed[,1]),]
  flog.debug('nrow(old.seed) = %s', nrow(old.seed))
  flog.debug("segment:",segment, capture=TRUE)

  return(list(this.seed=old.seed, next.seed=new.seed))
}


# count - total number of points to generate (will truncate to achieve exact
# number
# epochs - number of iterations to run. Note that the count grows quickly.
# Example
# Get 10 points
# ps <- fractal(seed, pats, 10)
# ps <- fractal(sample(sampleInitiators,1), sampleGenerators, 10)
# Get 3 epochs
# ps <- fractal(seed, pats, epochs=3)
fractal <- function(seeds, patterns, count=NULL, epochs=NULL, ..., type='uniform')
{
  flog.warn("This function is deprecated. Use rfractal instead.")

  if ('list' %in% class(seeds)) seed <- sample(seeds, 1)[[1]]
  else seed <- seeds

  do.call(paste('fractal.',type, sep=''), list(seed,patterns,count,epochs,...))
}

# ps <- fractal.uniform(seed,pats, 2)
# ps <- fractal.uniform(seed,pattern.3, 1); plot(ps, type='l')
# origin - The date at which to start time series. Default is the beginning of
# the unix epoch, but any valid date is allowed.
# date.fun - The function to use to create dates. This defaults to as.Date, but
#   for intraday work, it's possible to use as.POSIXct
# only - Only use the given index in pattern
# Example
# Get 10 points
# ps <- fractal.uniform(seed, pats, 10)
# Get 3 epochs
# ps <- fractal.uniform(seed, pats, epochs=3)
fractal.uniform <- function(seed, patterns, count=NULL, epochs=NULL,
  origin='1970-01-01', date.fun=as.Date, only=NULL)
{
  flog.warn("This function is deprecated. Use rfractal instead.")

  if (! 'list' %in% class(patterns)) patterns <- list(pattern.1=patterns)

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
    flog.debug("Set epochs to %s",epochs)
  }
  if (is.na(epochs) | is.null(epochs)) stop("Unable to calculate epochs")

  for (dummy in 1:epochs)
  {
    rows <- nrow(seed)
    next.seed <- seed
    for (idx in 2:rows)
    {
      if (! is.null(only)) pattern <- patterns[[only]]
      else pattern <- patterns[[sample(length(patterns),1)]]

      iteration <- next.seeds(seed, next.seed, pattern, idx, dummy)
      seed <- iteration$this.seed
      next.seed <- iteration$next.seed
    }
    seed <- next.seed[order(next.seed[,1]),]
  }
  # TODO: This may cause problems related to binary order sign
  seed <- unique(seed)
  #seed[,1] <- as.Date(seed[,1], origin=origin)
  seed <- xts(seed[,2], order.by=date.fun(seed[,1], origin=origin))
  if (use.count) seed <- tail(seed, count)
  return(seed)
}


fractal.random <- function(seed, patterns, count=NULL, epochs=NULL, 
  origin='1970-01-01', date.fun=as.Date, only=NULL)
{
  flog.warn("This function is deprecated. Use rfractal instead.")

  if (! 'list' %in% class(patterns)) patterns <- list(pattern.1=patterns)

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
    flog.debug("Set epochs to %s",epochs)
  }
  if (is.na(epochs) | is.null(epochs)) stop("Unable to calculate epochs")

  for (dummy in 1:epochs)
  {
    pattern <- patterns[[sample(length(patterns),1)]]
    idx <- sample((nrow(seed)-1), 1) + 1

    x.delta <- seed[idx,1] - seed[idx-1,1]
    y.delta <- seed[idx,2] - seed[idx-1,2]
    scale <- c(x.delta, y.delta)
    start <- c(seed[idx-1,1], seed[idx-1,2])
    segment <- pattern * 
      matrix(rep(scale, nrow(pattern)), ncol=2, byrow=TRUE) +
      matrix(rep(start, nrow(pattern)), ncol=2, byrow=TRUE)
    if (idx <= 2)
    {
      next.seed <- rbind(segment, seed[(idx+1):nrow(seed),])
    }
    else if (idx == nrow(seed))
    {
      # Last row
      next.seed <- rbind(seed[1:(idx-2),], segment)
    }
    else
    {
      next.seed <- rbind(seed[1:(idx-2),], segment, seed[(idx+1):nrow(seed),])
    }
    seed <- next.seed[order(next.seed[,1]),]
  }
  # TODO: This may cause problems related to binary order sign
  seed <- (unique(seed))
  seed <- xts(seed[,2], order.by=date.fun(seed[,1], origin=origin))
  if (use.count) seed <- tail(seed, count)
  return(seed)
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
