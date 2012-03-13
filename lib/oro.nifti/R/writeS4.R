##
##
## Copyright (c) 2009-2011 Brandon Whitcher and Volker Schmid
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
## $Id: writeS4.R 332 2010-01-29 16:54:07Z bjw34032 $
##

setGeneric("writeNIfTI", function(nim,  ...) standardGeneric("writeNIfTI"))

setMethod("writeNIfTI", signature(nim="nifti"), 
	  function(nim, filename, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
                   warn=-1) {
            .writeNIfTI(nim, filename, onefile, gzipped, verbose, warn)
          })
setMethod("writeNIfTI", signature(nim="anlz"), 
	  function(nim, filename, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
                   warn=-1) {
            .writeNIfTI(as(nim, "nifti"), filename, onefile, gzipped,
                        verbose, warn)
          })
setMethod("writeNIfTI", signature(nim="array"), 
	  function(nim, filename, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
                   warn=-1) {
            .writeNIfTI(as(nim, "nifti"), filename, onefile, gzipped,
                        verbose, warn)
          })

.writeNIfTI <- function(nim, filename, onefile=TRUE, gzipped=TRUE,
                        verbose=FALSE, warn=-1) {
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)
  ## Basic error checking
  validNIfTI <- getValidity(getClassDef("nifti"))
  if (is.character(vnim <- validNIfTI(nim))) {
    stop(vnim)
  }
  ## Write header file...
  if (gzipped) {
    fid <- gzfile(paste(filename, "nii.gz", sep="."), "wb")
  } else {
    fid <- file(paste(filename, "nii", sep="."), "wb")
  }

  extensions <- NULL
  if (is(nim, "niftiExtension")) {
    if (verbose) {
      cat("  niftiExtension detected!", fill=TRUE)
    }
    extensions <- nim@extensions
  }
  if (getOption("niftiAuditTrail") && is(nim, "niftiAuditTrail")) {
    if (verbose) {
      cat("  niftiAuditTrail detected!", fill=TRUE)
    }
    sec <- niftiAuditTrailToExtension(nim, getwd(), filename, match.call())
    extensions <- append(extensions, sec)
  }
  if (!is.null(extensions)) {
    ## update the vox_offset  FIXME twofile!
    totalesizes <- sum(unlist(lapply(extensions, function(x) x@"esize")))
    nim@"extender"[1] <- 1
    nim@"vox_offset" <- 352 + totalesizes
    if (verbose) {
      cat("  vox_offset =", nim@"vox_offset", fill=TRUE)
    }
  }

  writeBin(as.integer(nim@"sizeof_hdr"), fid, size=4)
  writeChar(nim@"data_type", fid, nchars=10, eos=NULL)
  writeChar(nim@"db_name", fid, nchars=18, eos=NULL)
  writeBin(as.integer(nim@"extents"), fid, size=4)
  writeBin(as.integer(nim@"session_error"), fid, size=2)
  writeChar(nim@"regular", fid, nchars=1, eos=NULL)
  writeBin(as.integer(nim@"dim_info"), fid, size=1)
  writeBin(as.integer(nim@"dim_"), fid, size=2)
  writeBin(nim@"intent_p1", fid, size=4)
  writeBin(nim@"intent_p2", fid, size=4)
  writeBin(nim@"intent_p3", fid, size=4)
  writeBin(as.integer(nim@"intent_code"), fid, size=2)
  writeBin(as.integer(nim@"datatype"), fid, size=2)
  writeBin(as.integer(nim@"bitpix"), fid, size=2)
  writeBin(as.integer(nim@"slice_start"), fid, size=2)
  writeBin(nim@"pixdim", fid, size=4)
  writeBin(nim@"vox_offset", fid, size=4) # default offset = 352
  writeBin(nim@"scl_slope", fid, size=4)
  writeBin(nim@"scl_inter", fid, size=4)
  writeBin(as.integer(nim@"slice_end"), fid, size=2)
  writeBin(as.integer(nim@"slice_code"), fid, size=1)
  writeBin(as.integer(nim@"xyzt_units"), fid, size=1)
  writeBin(nim@"cal_max", fid, size=4)
  writeBin(nim@"cal_min", fid, size=4)
  writeBin(nim@"slice_duration", fid, size=4)
  writeBin(nim@"toffset", fid, size=4)
  writeBin(as.integer(nim@"glmax"), fid, size=4)
  writeBin(as.integer(nim@"glmin"), fid, size=4)
  writeChar(nim@"descrip", fid, nchars=80, eos=NULL)
  writeChar(nim@"aux_file", fid, nchars=24, eos=NULL)
  writeBin(as.integer(nim@"qform_code"), fid, size=2)
  writeBin(as.integer(nim@"sform_code"), fid, size=2)
  writeBin(nim@"quatern_b", fid, size=4)
  writeBin(nim@"quatern_c", fid, size=4)
  writeBin(nim@"quatern_d", fid, size=4)
  writeBin(nim@"qoffset_x", fid, size=4)
  writeBin(nim@"qoffset_y", fid, size=4)
  writeBin(nim@"qoffset_z", fid, size=4)
  writeBin(nim@"srow_x", fid, size=4)
  writeBin(nim@"srow_y", fid, size=4)
  writeBin(nim@"srow_z", fid, size=4)
  writeChar(nim@"intent_name", fid, nchars=16, eos=NULL)
  writeChar(nim@"magic", fid, nchars=4, eos=NULL)
  writeBin(as.integer(nim@"extender"), fid, size=1)
  ## writeChar(as.character(nim@"extender"), fid, nchars=4, eos=NULL)
  ## Extensions?
  if (nim@"extender"[1] > 0 || nim@"vox_offset" > 352) {
    if (! is.null(extensions)) {
      if (verbose) {
        cat("  writing niftiExtension(s) at byte =", seek(fid), fill=TRUE)
      }
      lapply(extensions,
             function(x) {
               writeBin(as.integer(x@"esize"), fid, size=4)
               writeBin(as.integer(x@"ecode"), fid, size=4)
               ## Write out all the characters in the data section
	       writeChar(x@"edata", fid, nchars=nchar(x@"edata"), eos=NULL)
	       ## add margin to write \0 till 0 mod 16
	       margin <- (-(nchar(x@"edata", type="bytes") + 8) %% 16) 
	       if (margin > 0) {
		 writeBin(rep("", margin), fid, size=margin)
	       }
               invisible()
             })
    } else {
      stop("@extender set but", nim, "has no extensions")
    }
  }
  ## reorient?
  if (nim@"reoriented") {
    data <- as.vector(inverseReorient(nim))
  } else {
    data <- as.vector(nim@.Data)
  }
  ## Write image file...
  if (verbose) {
    cat("  writing data at byte =", seek(fid), fill=TRUE)
  }
  switch(as.character(nim@"datatype"),
         "2" = writeBin(as.integer(data), fid, size=nim@"bitpix"/8),
         "4" = writeBin(as.integer(data), fid, size=nim@"bitpix"/8),
         "8" = writeBin(as.integer(data), fid, size=nim@"bitpix"/8),
         "16" = writeBin(as.double(data), fid, size=nim@"bitpix"/8),
         "64" = writeBin(as.double(data), fid, size=nim@"bitpix"/8),
         "512" = writeBin(as.integer(data), fid, size=nim@"bitpix"/8)
         )
  close(fid)
  ## Warnings?
  options(warn=oldwarn)
  invisible()
}

############################################################################
############################################################################
############################################################################

setGeneric("writeANALYZE", function(aim,  ...) standardGeneric("writeANALYZE"))
setMethod("writeANALYZE", signature(aim="anlz"), 
	  function(aim, filename, gzipped=TRUE, verbose=FALSE, warn=-1) {
            .writeANALYZE(aim, filename, gzipped, verbose, warn)
          })
.writeANALYZE <- function(aim, filename, gzipped=TRUE, verbose=FALSE,
                          warn=-1) {
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)
  ## Basic error checking
  validANALYZE <- getValidity(getClassDef("anlz"))
  if (is.character(error <- validANALYZE(aim))) {
    cat(error, fill=TRUE)
    stop("-- aborting writeANALYZE --")
  }
  ## Write header file...
  if (gzipped) {
    fid <- gzfile(paste(filename, ".hdr.gz", sep=""), "wb")
  } else {
    fid <- file(paste(filename, ".hdr", sep=""), "wb")
  }
  ## header_key
  writeBin(as.integer(aim@"sizeof_hdr"), fid, size=4)  #  0 + 4
  writeChar(aim@"data_type", fid, nchars=10, eos=NULL) #  4 + 10
  writeChar(aim@"db_name", fid, nchars=18, eos=NULL)   #  14 + 18
  writeBin(as.integer(aim@"extents"), fid, size=4)     #  32 + 4
  writeBin(as.integer(aim@"session_error"), fid, size=2) # 36 + 2
  writeChar(aim@"regular", fid, nchars=1, eos=NULL)    # 38 + 1
  writeChar(aim@"hkey_un0", fid, nchars=1, eos=NULL)   # 39 + 1
  ##                                                   40 bytes
  ## image_dimension
  writeBin(as.integer(aim@"dim_"), fid, size=2)        # 0 + (2 x 8)
  writeChar(aim@"vox_units", fid, nchars=4, eos=NULL)  # 16 + 4
  writeChar(aim@"cal_units", fid, nchars=8, eos=NULL)  # 20 + 8
  writeBin(as.integer(aim@"unused1"), fid, size=2)     # 28 + 2
  writeBin(as.integer(aim@"datatype"), fid, size=2)    # 30 + 2
  writeBin(as.integer(aim@"bitpix"), fid, size=2)      # 32 + 2
  writeBin(as.integer(aim@"dim_un0"), fid, size=2)     # 34 + 2
  writeBin(aim@"pixdim", fid, size=4)                  # 36 + (4 x 8)
  writeBin(aim@"vox_offset", fid, size=4)              # 68 + 4
  writeBin(aim@"funused1", fid, size=4)                # 72 + 4
  writeBin(aim@"funused2", fid, size=4)                # 76 + 4
  writeBin(aim@"funused3", fid, size=4)                # 80 + 4
  writeBin(as.numeric(aim@"cal_max"), fid, size=4)     # 84 + 4
  writeBin(as.numeric(aim@"cal_min"), fid, size=4)     # 88 + 4
  writeBin(as.integer(aim@"compressed"), fid, size=4)  # 92 + 4
  writeBin(as.integer(aim@"verified"), fid, size=4)    # 96 + 4
  writeBin(as.integer(aim@"glmax"), fid, size=4)       # 100 + 4
  writeBin(as.integer(aim@"glmin"), fid, size=4)       # 104 + 4
  ##                                                   108 bytes
  ## data_history
  writeChar(aim@"descrip", fid, nchars=80, eos=NULL)   # 0 + 80
  writeChar(aim@"aux_file", fid, nchars=24, eos=NULL)  # 80 + 24
  writeChar(aim@"orient", fid, nchars=1, eos=NULL)     # 104 + 1
  writeBin(as.integer(aim@"origin"), fid, size=2)      # 105 + (2 x 5)
  writeChar(aim@"generated", fid, nchars=10, eos=NULL) # 115 + 10
  writeChar(aim@"scannum", fid, nchars=10, eos=NULL)   # 125 + 10
  writeChar(aim@"patient_id", fid, nchars=10, eos=NULL) # 135 + 10
  writeChar(aim@"exp_date", fid, nchars=10, eos=NULL)  # 145 + 10
  writeChar(aim@"exp_time", fid, nchars=10, eos=NULL)  # 155 + 10
  writeChar(aim@"hist_un0", fid, nchars=3, eos=NULL)   # 165 + 3
  writeBin(as.integer(aim@"views"), fid, size=4)       # 168 + 4
  writeBin(as.integer(aim@"vols_added"), fid, size=4)  # 172 + 4
  writeBin(as.integer(aim@"start_field"), fid, size=4) # 176 + 4
  writeBin(as.integer(aim@"field_skip"), fid, size=4)  # 180 + 4
  writeBin(as.integer(aim@"omax"), fid, size=4)        # 184 + 4
  writeBin(as.integer(aim@"omin"), fid, size=4)        # 188 + 4
  writeBin(as.integer(aim@"smax"), fid, size=4)        # 192 + 4
  writeBin(as.integer(aim@"smin"), fid, size=4)        # 196 + 4
  ##                                                   200 bytes
  close(fid)
  ## Write image file...
  if (gzipped) {
    fid <- gzfile(paste(filename, "img.gz", sep="."), "wb")
  } else {
    fid <- file(paste(filename, "img", sep="."), "wb")
  }
  dims <- 2:(1+aim@"dim_"[1])
  if (verbose) {
    cat("  dims =", aim@"dim_"[dims], fill=TRUE)
  }
  ## writeBin(as.vector(aim), fid, , size=aim@"bitpix"/8)
  data <- as.vector(aim@.Data)
  switch(as.character(aim@"datatype"),
         "1" = writeBin(as.integer(data), fid, size=aim@"bitpix"/8),
         "2" = writeBin(as.integer(data), fid, size=aim@"bitpix"/8),
         "4" = writeBin(as.integer(data), fid, size=aim@"bitpix"/8),
         "8" = writeBin(as.integer(data), fid, size=aim@"bitpix"/8),
         "16" = writeBin(as.numeric(data), fid, size=aim@"bitpix"/8),
         "64" = writeBin(as.double(data), fid, size=aim@"bitpix"/8))
  close(fid)
  ## Warnings?
  options(warn=oldwarn)
  invisible()
}
