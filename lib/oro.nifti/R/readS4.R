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
## $Id: readS4.R 332 2010-01-29 16:54:07Z bjw34032 $
##

## Sub-routines
.readCharWithEmbeddedNuls <- function(fid, n, to="UTF-8") {
  txt <- readBin(fid, "raw", n)
  iconv(rawToChar(txt[txt != as.raw(0)]), to=to)
}

##
##
##

readNIfTI <- function(fname, verbose=FALSE, warn=-1, reorient=TRUE,
                      call=NULL) {
  if (is.null(call)) {
    call <- match.call()
  }
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)

  if (verbose) {
    cat(paste("  fname =", fname), fill=TRUE)
  }
  ## Strip any extensions
  fname <- sub("\\.gz$", "", fname)
  fname <- sub("\\.nii$", "", fname)
  fname <- sub("\\.hdr$", "", fname)
  fname <- sub("\\.img$", "", fname)

  nii <- paste(fname, "nii", sep=".")
  niigz <- paste(fname, "nii.gz", sep=".")
  hdr <- paste(fname, "hdr", sep=".")
  hdrgz <- paste(fname, "hdr.gz", sep=".")
  img <- paste(fname, "img", sep=".")
  imggz <- paste(fname, "img.gz", sep=".")

  if (file.exists(niigz)) {
    ## If compressed file exists, then upload!
    if (verbose) {
      cat(paste("  files =", niigz), fill=TRUE)
    }
    nim <- .read.nifti.content(fname, gzipped=TRUE, verbose=verbose,
                               warn=warn, reorient=reorient, call=call)
  } else {
    if (file.exists(nii)) {
      ## If uncompressed file exists, then upload!
      if (verbose) {
        cat(paste("  files =", nii), fill=TRUE)
      }
      nim <- .read.nifti.content(fname, gzipped=FALSE, verbose=verbose,
                                 warn=warn, reorient=reorient, call=call)
    } else {
      if (file.exists(hdrgz) && file.exists(imggz)) {
        ## If compressed files exist, then upload!
        if (verbose) {
          cat(paste("  files =", hdrgz, "and", imggz), fill=TRUE)
        }
        nim <- .read.nifti.content(fname, onefile=FALSE, gzipped=TRUE,
                                   verbose=verbose, warn=warn,
                                   reorient=reorient, call=call)
      } else {
        ## If uncompressed files exist, then upload!
        if (file.exists(hdr) && file.exists(img)) {
          if (verbose) {
            cat(paste("  files =", hdr, "and", img), fill=TRUE)
          }
        nim <- .read.nifti.content(fname, onefile=FALSE, gzipped=FALSE,
                                   verbose=verbose, warn=warn,
                                   reorient=reorient, call=call)
        } else {
          stop("File(s) not found!")
        }
      }
    }
  }
  options(warn=oldwarn)
  return(nim)
}

############################################################################
############################################################################
############################################################################

.read.nifti.content <- function(fname, onefile=TRUE, gzipped=TRUE,
                                verbose=FALSE, warn=-1, reorient=FALSE,
                                call=NULL) {
  ## Open appropriate file
  if (gzipped) {
    suffix <- ifelse(onefile, "nii.gz", "hdr.gz")
    fname <- paste(fname, suffix, sep=".")
    fid <- gzfile(fname, "rb")
    if (verbose) {
      cat("  nii   =", fname, fill=TRUE)
    }
  } else {
    suffix <- ifelse(onefile, "nii", "hdr")
    fname <- paste(fname, suffix, sep=".")
    fid <- file(fname, "rb")
    if (verbose) {
      cat("  hdr   =", fname, fill=TRUE)
    }
  }
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)
  ## Test for endian properties
  endian <- .Platform$endian
  sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  if (sizeof.hdr != 348) {
    close(fid)
    endian <- "swap"
    if (gzipped) {
      fid <- gzfile(fname, "rb")
    } else {
      fid <- file(fname, "rb")
    }
    sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
    if (verbose) {
      cat("  ENDIAN = swap", fill=TRUE)
    }
  }
  ## Construct S4 object
  nim <- nifti()
  nim@"sizeof_hdr" <- sizeof.hdr
  nim@"data_type" <- .readCharWithEmbeddedNuls(fid, n=10)
  nim@"db_name" <- .readCharWithEmbeddedNuls(fid, n=18)
  nim@"extents" <- readBin(fid, integer(), size=4, endian=endian)
  nim@"session_error" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"regular" <- .readCharWithEmbeddedNuls(fid, n=1)
  nim@"dim_info" <- .readCharWithEmbeddedNuls(fid, n=1)
  nim@"dim_" <- readBin(fid, integer(), 8, size=2, endian=endian)
  nim@"intent_p1" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"intent_p2" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"intent_p3" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"intent_code" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"datatype" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"bitpix" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"slice_start" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"pixdim" <- readBin(fid, numeric(), 8, size=4, endian=endian)
  nim@"vox_offset" <- readBin(fid, numeric(), size=4, endian=endian)
  if (verbose) {
    cat("  vox_offset =", nim@"vox_offset", fill=TRUE)
  }
  nim@"scl_slope" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"scl_inter" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"slice_end" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"slice_code" <- readBin(fid, integer(), size=1, signed=FALSE,
                              endian=endian)
  nim@"xyzt_units" <- readBin(fid, integer(), size=1, signed=FALSE,
                              endian=endian)
  nim@"cal_max" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"cal_min" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"slice_duration" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"toffset" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"glmax" <- readBin(fid, integer(), size=4, endian=endian)
  nim@"glmin" <- readBin(fid, integer(), size=4, endian=endian)
  nim@"descrip" <- .readCharWithEmbeddedNuls(fid, n=80)
  nim@"aux_file" <- .readCharWithEmbeddedNuls(fid, n=24)
  nim@"qform_code" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"sform_code" <- readBin(fid, integer(), size=2, endian=endian)
  nim@"quatern_b" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"quatern_c" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"quatern_d" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"qoffset_x" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"qoffset_y" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"qoffset_z" <- readBin(fid, numeric(), size=4, endian=endian)
  nim@"srow_x" <- readBin(fid, numeric(), 4, size=4, endian=endian)
  nim@"srow_y" <- readBin(fid, numeric(), 4, size=4, endian=endian)
  nim@"srow_z" <- readBin(fid, numeric(), 4, size=4, endian=endian)
  nim@"intent_name" <- .readCharWithEmbeddedNuls(fid, n=16)
  nim@"magic" <- .readCharWithEmbeddedNuls(fid, n=4)
  ## To flag such a struct as being conformant to the NIFTI-1 spec,
  ## the last 4 bytes of the header must be either the C String "ni1"
  ## or "n+1"; in hexadecimal, the 4 bytes [6E 69 31 00] or [6E 2B 31
  ## 00] (in any future version of this format, the 1 will be upgraded
  ## to 2, etc.).  Normally, such a "magic number" or flag goes at the
  ## start of the file, but trying to avoid clobbering widely-used
  ## ANALYZE 7.5 fields led to putting this marker last.  However,
  ## recall that "the last shall be first" (Matthew 20:16).
  if (onefile) {  
    nim@"extender" <- readBin(fid, integer(), 4, size=1, signed=FALSE,
                              endian=endian)
    ## If extension[0] is nonzero, it indicates that extended header
    ## information is present in the bytes following the extension
    ## array.  In a .nii file, this extended header data is before the
    ## image data (and vox_offset must be set correctly to allow for
    ## this).  In a .hdr file, this extended data follows extension and
    ## proceeds (potentially) to the end of the file.
    ##
    if (nim@"extender"[1] > 0 || nim@"vox_offset" > 352) {
      if (verbose) {
        cat("  niftiExtension detected!", fill=TRUE)
      }
      if (!is(nim, "niftiExtension")) {
        nim <- as(nim, "niftiExtension")
      }
      while (seek(fid) < nim@"vox_offset") {
        if (verbose) {
          cat("  seek(fid) =", seek(fid), fill=TRUE)
        }
        nimextsec <- new("niftiExtensionSection")
        nimextsec@esize <- readBin(fid, integer(), size=4, endian=endian)
        nimextsec@ecode <- readBin(fid, integer(), size=4, endian=endian)
        nimextsec@edata <- .readCharWithEmbeddedNuls(fid, n=nimextsec@esize-8)
        nim@extensions <- append(nim@extensions, nimextsec)
      }
      if (seek(fid) > nim@"vox_offset") {
        stop("-- extension size (esize) has overshot voxel offset --")
      }
    }
  }

  if (verbose) {
    cat("  seek(fid) =", seek(fid), fill=TRUE)
  }
  n <- prod(nim@"dim_"[2:5])
  if (! onefile) {
    close(fid)
    fname <- sub("\\.hdr$", "\\.img", fname)
    if (gzipped) {
      fid <- gzfile(fname, "rb")
    } else {
      fid <- file(fname, "rb")
    }
    seek(fid, nim@"vox_offset") ## is this correct?
  }
  data <-
    switch(as.character(nim@"datatype"),
           "2" = readBin(fid, integer(), n, nim@"bitpix"/8, signed=FALSE,
             endian=endian),
           "4" = readBin(fid, integer(), n, nim@"bitpix"/8, endian=endian),
           "8" = readBin(fid, integer(), n, nim@"bitpix"/8, endian=endian),
           "16" = readBin(fid, double(), n, nim@"bitpix"/8, endian=endian),
           "64" = readBin(fid, double(), n, nim@"bitpix"/8, endian=endian),
           "512" = readBin(fid, integer(), n, nim@"bitpix"/8, endian=endian),
           stop(paste("Data type ", nim@"datatype", " unsupported in ",
                      fname, ".img", sep=""))
           )
  close(fid)
  ##
  ## THE SLOW BIT FOLLOWS
  ##
  ## 3D IMAGE (VOLUME) ORIENTATION AND LOCATION IN SPACE:
  ## There are 3 different methods by which continuous coordinates can
  ## attached to voxels.  The discussion below emphasizes 3D volumes,
  ## and the continuous coordinates are referred to as (x,y,z).  The
  ## voxel index coordinates (i.e., the array indexes) are referred to
  ## as (i,j,k), with valid ranges:
  ##   i = 0 .. dim[1]-1
  ##   j = 0 .. dim[2]-1  (if dim[0] >= 2)
  ##   k = 0 .. dim[3]-1  (if dim[0] >= 3)
  ## The (x,y,z) coordinates refer to the CENTER of a voxel.  In
  ## methods 2 and 3, the (x,y,z) axes refer to a subject-based
  ## coordinate system, with
  ##   +x = Right  +y = Anterior  +z = Superior.
  ## This is a right-handed coordinate system.  However, the exact
  ## direction these axes point with respect to the subject depends on
  ## qform_code (Method 2) and sform_code (Method 3).
  dims <- 2:(1+nim@"dim_"[1])
  if (reorient) {
    nim@.Data <- reorient(nim, data, verbose=verbose)
    nim@"reoriented" <- TRUE
  } else {
    nim@.Data <- array(data, nim@"dim_"[dims])
  }
  ## Warnings?
  options(warn=oldwarn)
  ## Check validity
  validNIfTI <- getValidity(getClassDef("nifti"))
  validNIfTI(nim)
  if (getOption("niftiAuditTrail")) {
    if (is.null(call)) {
      call <- match.call()
    }
    nim <- niftiExtensionToAuditTrail(nim, workingDirectory=getwd(),
                                      filename=fname, call=call)
  }
  return(nim)
}

############################################################################
############################################################################
############################################################################

readANALYZE <- function(fname, verbose=FALSE, warn=-1) {
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)
  if (verbose) {
    cat(paste("  fname =", fname), fill=TRUE)
  }
  ## Strip any extensions
  fname <- sub("\\.gz$", "", fname)
  fname <- sub("\\.hdr$", "", fname)
  fname <- sub("\\.img$", "", fname)
  if (! (file.exists(paste(fname, "hdr", sep=".")) &&
         file.exists(paste(fname, "img", sep="."))) &&
      ! (file.exists(paste(fname, "hdr.gz", sep=".")) &&
         file.exists(paste(fname, "img.gz", sep=".")))) {
    stop("File(s) not found!")
  }
  ## If uncompressed files exist, then upload!
  if (file.exists(paste(fname, "hdr", sep=".")) &&
      file.exists(paste(fname, "img", sep="."))) {      
    if (verbose) {
      cat(paste("  files = ", fname, ".{hdr,img}", sep=""), fill=TRUE)
    }
    aim <- .read.analyze.content(fname, gzipped=FALSE, verbose=verbose,
                                 warn=warn)
    options(warn=oldwarn)
    return(aim)
  }
  ## If compressed files exist, then upload!
  if (file.exists(paste(fname, "hdr.gz", sep=".")) &&
      file.exists(paste(fname, "img.gz", sep="."))) {      
    if (verbose) {
      cat(paste("  files = ", fname, ".{hdr.gz,img.gz}", sep=""), fill=TRUE)
    }
    aim <- .read.analyze.content(fname, gzipped=TRUE, verbose=verbose,
                                 warn=warn)
    options(warn=oldwarn)
    return(aim)
  }
  invisible()
}

############################################################################
############################################################################
############################################################################

.read.analyze.content <- function(fname, gzipped=TRUE, verbose=FALSE,
                                  warn=-1) {
  ## Open header file
  if (gzipped) {
    fname <- paste(fname, "hdr.gz", sep=".")
    fid <- gzfile(fname, "rb")
  } else {
    fname <- paste(fname, "hdr", sep=".")
    fid <- file(fname, "rb")
  }
  if (verbose) {
    cat("  hdr   =", fname, fill=TRUE)
  }
  ## Warnings?
  oldwarn <- getOption("warn")
  options(warn=warn)
  ## Test for endian properties
  endian <- .Platform$endian
  sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  if (sizeof.hdr != 348) {
    close(fid)
    endian <- "swap"
    if (gzipped) {
      fid <- gzfile(fname, "rb")
    } else {
      fid <- file(fname, "rb")
    }
    sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  }
  ## Construct S4 object
  aim <- new("anlz")
  aim@"sizeof_hdr" <- sizeof.hdr
  aim@"data_type" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"db_name" <- .readCharWithEmbeddedNuls(fid, n=18)
  aim@"extents" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"session_error" <- readBin(fid, integer(), size=2, endian=endian)
  aim@"regular" <- .readCharWithEmbeddedNuls(fid, n=1)
  aim@"hkey_un0" <- .readCharWithEmbeddedNuls(fid, n=1)
  aim@"dim_" <- readBin(fid, integer(), 8, size=2, endian=endian)
  aim@"vox_units" <- .readCharWithEmbeddedNuls(fid, n=4)
  aim@"cal_units" <- .readCharWithEmbeddedNuls(fid, n=8)
  aim@"unused1" <- readBin(fid, integer(), size=2, endian=endian)
  aim@"datatype" <- readBin(fid, integer(), size=2, endian=endian)
  aim@"bitpix" <- readBin(fid, integer(), size=2, endian=endian)
  aim@"dim_un0" <- readBin(fid, integer(), size=2, endian=endian)
  aim@"pixdim" <- readBin(fid, numeric(), 8, size=4, endian=endian)
  aim@"vox_offset" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"funused1" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"funused2" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"funused3" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"cal_max" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"cal_min" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"compressed" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"verified" <- readBin(fid, numeric(), size=4, endian=endian)
  aim@"glmax" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"glmin" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"descrip" <- .readCharWithEmbeddedNuls(fid, n=80)
  aim@"aux_file" <- .readCharWithEmbeddedNuls(fid, n=24)
  aim@"orient" <- .readCharWithEmbeddedNuls(fid, n=1)
  aim@"origin" <- readBin(fid, integer(), 5, size=2, endian=endian) # .readCharWithEmbeddedNuls(fid, 10)
  aim@"generated" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"scannum" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"patient_id" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"exp_date" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"exp_time" <- .readCharWithEmbeddedNuls(fid, 10)
  aim@"hist_un0" <- .readCharWithEmbeddedNuls(fid, n=3)
  aim@"views" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"vols_added" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"start_field" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"field_skip" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"omax" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"omin" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"smax" <- readBin(fid, integer(), size=4, endian=endian)
  aim@"smin" <- readBin(fid, integer(), size=4, endian=endian)
  close(fid)
  ## Open image file
  if (gzipped) {
    fname <- sub("\\.hdr\\.gz$", "\\.img\\.gz", fname) # paste(fname, ".img.gz", sep=".")
    fid <- gzfile(fname, "rb")
  } else {
    fname <- sub("\\.hdr$", "\\.img", fname) # paste(fname, "img", sep=".")
    fid <- file(fname, "rb")
  }
  if (verbose) {
    cat("  img   =", fname, fill=TRUE)
  }
  n <- prod(aim@"dim_"[2:5])
  data <- switch(as.character(aim@"datatype"),
                 "1" = readBin(fid, integer(), n, aim@"bitpix"/8,
                   signed=FALSE, endian=endian),
                 "2" = readBin(fid, integer(), n, aim@"bitpix"/8,
                   signed=FALSE, endian=endian),
                 "4" = readBin(fid, integer(), n, aim@"bitpix"/8,
                   endian=endian),
                 "8" = readBin(fid, integer(), n, aim@"bitpix"/8,
                   endian=endian),
                 "16" = readBin(fid, numeric(), n, aim@"bitpix"/8,
                   endian=endian),
                 "64" = readBin(fid, double(), n, aim@"bitpix"/8,
                   endian=endian),
                 stop(paste("Data type ", aim@"datatype", " (",
                            convert.datatype.anlz(aim@"datatype"), 
                            ") unsupported in", fname, sep="")))
  close(fid)
  dims <- 2:(1+aim@"dim_"[1])
  aim@.Data <- array(data, aim@"dim_"[dims])
  ## Warnings?
  options(warn=oldwarn)
  ## Check validity
  validANALYZE <- getValidity(getClassDef("anlz"))
  validANALYZE(aim)
  return(aim)
}
