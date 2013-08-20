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

next.seeds <- function(old.seed, new.seed, pattern, idx, epoch)
{
  x.delta <- old.seed[idx,1] - old.seed[idx-1,1]
  y.delta <- old.seed[idx,2] - old.seed[idx-1,2]
  scale <- c(x.delta, y.delta)
  start <- c(old.seed[idx-1,1], old.seed[idx-1,2])

  flog.debug("[%s.%s] scale: %s",epoch,idx, scale)
  flog.debug("[%s.%s] epoch: %s",epoch,idx, start)

  segment <- pattern * 
    matrix(rep(scale, nrow(pattern)), ncol=2, byrow=TRUE) +
    matrix(rep(start, nrow(pattern)), ncol=2, byrow=TRUE)

  # Create new seed by adding the new segment
  new.seed <- rbind(new.seed[! (new.seed[,1] %in% segment[,1]), ], segment)
  old.seed <- rbind(old.seed[! (old.seed[,1] %in% segment[,1]), ],
    segment[(segment[,1] %in% old.seed[,1]), ])
  old.seed <- old.seed[order(old.seed[,1]),]
  flog.debug("[%s.%s] segment: %s",epoch,idx,segment)

  return(list(this.seed=old.seed, next.seed=new.seed))
}

fractal.random <- function(seed, patterns, count=NULL, epochs=NULL, 
  origin='1970-01-01', date.fun=as.Date, only=NULL)
{
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


