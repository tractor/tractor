pvec <- function(v, FUN, ..., mc.set.seed=TRUE, mc.silent=FALSE, mc.cores=getOption("cores"), mc.cleanup=TRUE) {
  if (!is.vector(v)) stop("v must be a vector")
  
  env <- parent.frame()
  cores <- mc.cores
  if (is.null(cores)) cores <- volatile$detectedCores
  cores <- as.integer(cores)

  n <- length(v)
  l <- if (n < cores) as.list(v) else {
    # compute the scheduling, making it as fair as possible
    il <- as.integer(n / cores)
    xc <- n - il * cores
    sl <- rep(il, cores)
    if (xc) sl[1:xc] <- il + 1
    si <- cumsum(c(1L, sl))
    se <- si + c(sl, 0L) - 1L
    lapply(1:cores, function(ix) v[si[ix]:se[ix]])
  }
  jobs <- NULL
  cleanup <- function() {
    ## kill children if cleanup is requested
    if (length(jobs) && mc.cleanup) {
      ## first take care of uncollected children
      collect(children(jobs), FALSE)
      kill(children(jobs), if (is.integer(mc.cleanup)) mc.cleanup else SIGTERM)
      collect(children(jobs))
    }
    if (length(jobs)) {
      ## just in case there are zombies
      collect(children(jobs), FALSE)
    }
  }
  on.exit(cleanup())
  FUN <- match.fun(FUN)
  jobs <- lapply(seq(cores), function(i) parallel(FUN(l[[i]], ...), name=i, mc.set.seed=mc.set.seed, silent=mc.silent))
  res <- collect(jobs)
  names(res) <- NULL
  res <- do.call(c, res)
  if (length(res) != n) warning("some results may be missing, folded or caused an error")
  res
}
