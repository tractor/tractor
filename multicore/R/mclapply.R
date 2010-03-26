mclapply <- function(X, FUN, ..., mc.preschedule=TRUE, mc.set.seed=TRUE, mc.silent=FALSE, mc.cores=getOption("cores")) {
  env <- parent.frame()
  cores <- mc.cores
  if (is.null(cores)) cores <- volatile$detectedCores
  cores <- as.integer(cores)

  if (!mc.preschedule) { # sequential (non-scheduled)
    FUN <- match.fun(FUN)
    if (length(X) <= cores) { # all-out, we can use one-shot parallel
      jobs <- lapply(seq(X), function(i) parallel(FUN(X[[i]], ...), name=names(X)[i], mc.set.seed=mc.set.seed, silent=mc.silent))
      res <- collect(jobs)
      if (length(res) == length(X)) names(res) <- names(X)
      return(res)
    } else { # more complicated, we have to wait for jobs selectively
      sx <- seq(X)
      res <- lapply(sx, function(x) NULL)
      names(res) <- names(X)
      ent <- rep(FALSE, length(X))  # values entered (scheduled)
      fin <- rep(FALSE, length(X))  # values finished
      jobid <- 1:cores
      jobs <- lapply(jobid,  function(i) parallel(FUN(X[[i]], ...), mc.set.seed=mc.set.seed, silent=mc.silent))
      jobsp <- processID(jobs)
      ent[jobid] <- TRUE
      while (!all(fin)) {
        s <- selectChildren(jobs, 0.5)
	if (is.null(s)) break  # no children -> no hope
        if (is.integer(s)) for (ch in s) {
          ji <- which(jobsp == ch)[1]
          ci <- jobid[ji]
          r <- readChild(ch)
          if (is.raw(r))  res[[ci]] <- unserialize(r)
	  else {
	    fin[ci] <- TRUE
#	    cat("fin: "); print(fin)
#	    cat("res: "); print(unlist(lapply(res, is.null)))
	    if (!all(ent)) { # still something to do, spawn a new job
	      nexti <- which(!ent)[1]
	      jobid[ji] <- nexti
	      jobs[[ji]] <- parallel(FUN(X[[nexti]], ...), mc.set.seed=mc.set.seed, silent=mc.silent)
	      jobsp[ji] <- processID(jobs[[ji]])
	      ent[nexti] <- TRUE
            }
          }
        }
      }
      return(res)
    }
  }
  if (length(X) < cores) cores <- length(X)
  if (cores < 2) return(lapply(X, FUN, ...))
  sindex <- lapply(1:cores, function(i) seq(i,length(X), by=cores))
  schedule <- lapply(1:cores, function(i) X[seq(i,length(X), by=cores)])
  ch <- list()
  res <- lapply(seq(X), function(x) NULL)
  names(res) <- names(X)
  cp <- rep(0L, cores)
  fin <- rep(FALSE, cores)
  dr <- rep(FALSE, cores)
  inner.do <- function(core) {
    S <- schedule[[core]]
    f <- fork()
    if (inherits(f, "masterProcess")) { # child process
      on.exit(exit(1,structure("fatal error in wrapper code",class="try-error")))
      if (isTRUE(mc.set.seed)) set.seed(Sys.getpid())
      if (isTRUE(mc.silent)) closeStdout()
      sendMaster(try(lapply(S, FUN, ...), silent=TRUE))
      exit(0)
    }
    ch[[core]] <<- f
    cp[core] <<- f$pid
    NULL
  }
  lapply(1:cores, inner.do)
  ac <- cp[cp > 0]
  while (!all(fin)) {
    s <- selectChildren(ac, 1)
    if (is.null(s)) break # no children -> no hope we get anything
    if (is.integer(s)) for (ch in s) { 
      a <- readChild(ch)
      if (is.integer(a)) {
        core <- which(cp == a)
        fin[core] <- TRUE
      } else if (is.raw(a)) {
        core <- which(cp == attr(a, "pid"))
        res[[core]] <- unserialize(a)
        dr[core] <- TRUE
      }
    }
  }
#  str(res)
  ores <- list()
  for (i in 1:cores) ores[sindex[[i]]] <- res[[i]]
  if (length(names(X)) == length(ores)) names(ores) <- names(X)
  ores
}

#mcapply(1:4, function(i) i+1)
