##
##
## Copyright (c) 2009-2011, Brandon Whitcher and Volker Schmid
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer. 
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
## 
## $Id: convert_anlz.R 332 2010-01-29 16:54:07Z bjw34032 $
##

convert.bitpix.anlz <- function(bitpix=NULL) {
  anlz.bitpix <- list("NONE" = 0,
                      "UNKNOWN" = 0,
                      "BINARY" = 1,
                      "UNSIGNED_CHAR" = 8,
                      "SIGNED_SHORT" = 16,
                      "SIGNED_INT" = 32,
                      "FLOAT" = 32,
                      "COMPLEX" = 64,
                      "DOUBLE" = 64,
                      "RGB" = 24,
                      "ALL" = 0)
  if (is.null(bitpix)) {
    anlz.bitpix
  } else {
    names(which(anlz.bitpix == bitpix))
  }
}

convert.datatype.anlz <- function(datatype.code=NULL) {
  anlz.datatype <- list("NONE" = 0,
                        "UNKNOWN" = 0,
                        "BINARY" = 1,
                        "UNSIGNED_CHAR" = 2,
                        "SIGNED_SHORT" = 4,
                        "SIGNED_INT" = 8,
                        "FLOAT" = 16,
                        "COMPLEX" = 32,
                        "DOUBLE" = 64,
                        "RGB" = 128,
                        "ALL" = 255)
  if (is.null(datatype.code)) {
    anlz.datatype
  } else {
    names(which(anlz.datatype == as.numeric(datatype.code)))
  }
}

convert.orient.anlz <- function(orientation) {
  switch(as.character(orientation),
         "0" = "transverse unflipped",
         "1" = "coronal unflipped",
         "2" = "sagittal unflipped",
         "3" = "transverse flipped",
         "4" = "coronal flipped",
         "5" = "sagittal flipped",
         "unknown")
}

############################################################################
## as.anlz()
############################################################################

as.anlz <- function(from, value=NULL, verbose=FALSE) {
  integertype <- function(from) {
    integer.ranges <- list("SIGNED_SHORT" = c(0, 2^15-1),
                           "SIGNED_INT" = c(0, 2^31-1))
    fromRange <- range(from)
    for (i in 1:length(integer.ranges)) {
      if (fromRange[1] >= min(integer.ranges[[i]]) &&
	  fromRange[2] <= max(integer.ranges[[i]])) {
	return(names(integer.ranges)[i])
      }
    }
    warning("Range too large to be kept as integer, forcing float")
    floattype(from)
  }
  
  floattype <- function(from) {
    return("FLOAT")
  }

  if (is.null(value)) {
    aim <- anlz()
  } else {
    aim <- value
  }

  if (is.array(from)) {
    ## Determine a sensible datatype
    dataClass <- class(from[1])
    datatypeString <- switch(dataClass,
                             logical = integertype(from),
                             integer = integertype(from),
                             numeric = floattype(from),
                             stop("Can't transform data in from: ",
                                  class(from[1])))
    aim@"data_type" <- datatypeString
    aim@"datatype" <- convert.datatype.anlz()[[datatypeString]]
    aim@"bitpix" <- convert.bitpix.anlz()[[datatypeString]]
    aim@"cal_min" <- min(from, na.rm=TRUE)
    aim@"cal_max" <- max(from, na.rm=TRUE)
    aim@"dim_" <- c(length(dim(from)), dim(from))
    if (length(aim@"dim_") < 8) {
      aim@"dim_" <- c(aim@"dim_", rep(1, 8 - length(aim@"dim_")))
    }
    aim@.Data <- from
  } else {
    if (is.list(from)) {
      aim <- lapply(from, function(x) as.anlz(x, value))
    } else {
      if (verbose) {
        warning("Cannot convert class =", class(from), "to anlz object")
      }
      aim <- from
    }
  }
  return(aim)
}


