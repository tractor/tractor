.NilPointer <- methods::new("externalptr")

#' Test for or create a nil pointer
#'
#' A "nil" external pointer is used throughout this package to indicate the
#' absence of a valid underlying C++ object, in contexts where an ordinary
#' `NULL` is not appropriate.
#'
#' @param object An object, or `NULL`.
#' @return If `object` is `NULL`, the nil pointer singleton is returned.
#'   Otherwise, a logical value indicating whether `object` is identical to
#'   the nil pointer.
#' @author Jon Clayden
#' @export
nilPointer <- function (object = NULL)
{
    if (is.null(object)) .NilPointer else identical(object, .NilPointer)
}

#' The DiffusionModel class
#'
#' This class represents a fitted diffusion model, encapsulating a pointer to
#' an underlying C++ object which is used by a [Tracker] to perform
#' streamline tractography. Objects of this class are created by
#' [dtiDiffusionModel()] or [bedpostDiffusionModel()], rather than directly.
#'
#' @field pointer An external pointer to the underlying C++ `DiffusionModel`
#'   object.
#' @field type A string naming the type of model, currently either `"dti"` or
#'   `"bedpost"`.
#'
#' @export
DiffusionModel <- setRefClass("DiffusionModel", contains="TractorObject", fields=list(pointer="externalptr",type="character"), methods=list(
    getPointer = function () { return (pointer) },
    
    getType = function () { return (type) }
))

.NilModel <- DiffusionModel$new()

#' Test for or create a nil diffusion model
#'
#' A "nil" [DiffusionModel] is used to indicate the absence of a valid,
#' fitted diffusion model, for example when creating a [Tracker] that will
#' not itself be used to generate streamlines (as when reading them from
#' file instead).
#'
#' @param object An object, or `NULL`.
#' @return If `object` is `NULL`, the nil model singleton is returned.
#'   Otherwise, a logical value indicating whether `object` is identical to
#'   the nil model.
#' @author Jon Clayden
#' @export
nilModel <- function (object = NULL)
{
    if (is.null(object)) .NilModel else identical(object, .NilModel)
}

#' Create a diffusion tensor model
#'
#' Create a [DiffusionModel] representing a diffusion tensor fit, based on an
#' image of principal diffusion directions.
#'
#' @param directionsPath A character vector giving the path to the principal
#'   diffusion direction image or file stem.
#' @return A [DiffusionModel] object of type `"dti"`.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
dtiDiffusionModel <- function (directionsPath)
{
    if (!imageFileExists(directionsPath))
        report(OL$Error, "The specified principal directions image does not exist")
    
    pointer <- .Call("createDtiModel", directionsPath, PACKAGE="tractor.track")
    
    return (DiffusionModel$new(pointer=pointer, type="dti"))
}

#' Count the number of fibres in a BEDPOST directory
#'
#' Determine the number of fibre compartments modelled within a BEDPOST(X)
#' output directory, by counting the number of `mean_f<n>samples` images
#' present.
#'
#' @param bedpostDir A character string giving the path to a BEDPOST(X)
#'   output directory.
#' @return An integer giving the number of fibres represented in the
#'   directory.
#' @author Jon Clayden
#' @export
getBedpostNumberOfFibres <- function (bedpostDir)
{
    i <- 1
    while (imageFileExists(file.path(bedpostDir, es("mean_f#{i}samples"))))
        i <- i + 1
    
    return (i-1)
}

#' Create a ball-and-sticks diffusion model
#'
#' Create a [DiffusionModel] representing a ball-and-sticks (BEDPOST) fit,
#' based on the contents of a BEDPOST(X) output directory.
#'
#' @param bedpostDir A character string giving the path to a BEDPOST(X)
#'   output directory.
#' @param avfThreshold A numeric value giving the anisotropic volume fraction
#'   below which a fibre compartment will be ignored during tractography.
#' @return A [DiffusionModel] object of type `"bedpost"`.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
bedpostDiffusionModel <- function (bedpostDir, avfThreshold = 0.05)
{
    if (length(bedpostDir) != 1)
        report(OL$Error, "BEDPOST directory should be specified as a single string")
    if (!file.exists(bedpostDir) || !file.info(bedpostDir)$isdir)
        report(OL$Error, "The specified BEDPOST directory does not exist, or is a file")
    
    nFibres <- getBedpostNumberOfFibres(bedpostDir)
    if (nFibres < 1)
        report(OL$Error, "BEDPOST files do not seem to be present in directory #{bedpostDir}")
    
    files <- list(avf=file.path(bedpostDir, paste0("merged_f",1:nFibres,"samples")),
                  theta=file.path(bedpostDir, paste0("merged_th",1:nFibres,"samples")),
                  phi=file.path(bedpostDir, paste0("merged_ph",1:nFibres,"samples")))
    
    if (!all(imageFileExists(unlist(files))))
        report(OL$Error, "Some BEDPOST files are missing from directory #{bedpostDir}")
    
    pointer <- .Call("createBedpostModel", files, as.double(avfThreshold), PACKAGE="tractor.track")
    
    return (DiffusionModel$new(pointer=pointer, type="bedpost"))
}
