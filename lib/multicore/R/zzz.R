# this envoronment holda any volatile variables we may want to keep inside the package
volatile <- new.env(TRUE, emptyenv())

# detect the number of [virtual] CPUs (cores)
detectCores <- function(all.tests = FALSE) {
  # feel free to add tests - those are the only ones I could test [SU]
  systems <- list(darwin  = "/usr/sbin/sysctl -n hw.ncpu 2>/dev/null",
                  linux   = "grep processor /proc/cpuinfo 2>/dev/null|wc -l",
		  irix    = c("hinv |grep Processors|sed 's: .*::'", "hinv|grep '^Processor '|wc -l"),
		  solaris = "/usr/sbin/psrinfo -v|grep 'Status of.*processor'|wc -l")
  for (i in seq(systems))
    if(all.tests || length(grep(paste("^", names(systems)[i], sep=''), R.version$os)))
      for (cmd in systems[i]) {
        a <- gsub("^ +","",system(cmd, TRUE)[1])
        if (length(grep("^[1-9]", a))) return(as.integer(a))
      }
  NA
}

.onLoad <- function(libname, pkgname) {
  cores <- detectCores()
  volatile$detectedCoresSuccess <- !is.na(cores)
  if (is.na(cores)) cores <- 8L # a fallback expecting higher-end desktop ...
  volatile$detectedCores <- cores
  TRUE
}
