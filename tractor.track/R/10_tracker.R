# NB: The underlying C++ class is not thread-safe, so a Tracker object should not be run multiple times concurrently
#' The Tracker class
#'
#' This class represents a streamline tractography engine, associated with a
#' particular [DiffusionModel], seeding mask and set of tracking parameters.
#' It is used to generate streamlines from seed points, via
#' [generateStreamlines()].
#'
#' @field model The [DiffusionModel] object used to guide tracking.
#' @field pointer An external pointer to the underlying C++ `Tracker` object.
#'
#' @export
Tracker <- setRefClass("Tracker", contains="TractorObject", fields=list(model="DiffusionModel",pointer="externalptr"), methods=list(
    initialize = function (model = nilModel(), mask = NULL, curvatureThreshold = 0.2, loopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, oneWay = FALSE, ...)
    {
        if (nilModel(model))
            ptr <- nilPointer()
        else
            ptr <- .Call("createTracker", model$getPointer(), mask, maxSteps, stepLength, curvatureThreshold, loopcheck, oneWay, PACKAGE="tractor.track")
        
        initFields(model=model, pointer=ptr)
    },
    
    getModel = function () { return (model) },
    
    getPointer = function () { return (pointer) },
    
    setTargets = function (image, indices = NULL, labels = NULL, terminate = FALSE)
    {
        "Set or clear target regions used to constrain or terminate streamlines"
        if (is.list(image))
        {
            indices <- indices %||% image$indices
            labels <- labels %||% image$labels
            image <- image$image
        }
        
        if (is.character(image) && length(image) == 1 && identifyImageFileNames(image)$format == "Mrtrix")
        {
            path <- threadSafeTempFile("target")
            image <- writeImageFile(image, path, "NIFTI")$fileStem
        }
        
        .Call("setTrackerTargets", pointer, list(image=image,indices=indices,labels=labels), terminate, PACKAGE="tractor.track")
        return (.self)
    }
))

#' Create a streamline tracker
#'
#' Create a [Tracker] object, which encapsulates a fitted diffusion model,
#' seeding mask and set of tracking parameters, and is used to generate
#' streamlines from seed points.
#'
#' @param model A [DiffusionModel] object, as created by [dtiDiffusionModel()]
#'   or [bedpostDiffusionModel()].
#' @param mask An optional mask image (or path to one), restricting the
#'   region within which tracking is permitted.
#' @param curvatureThreshold A numeric value giving the minimum permitted dot
#'   product between successive tracking steps, controlling how sharply a
#'   streamline can turn.
#' @param loopcheck Logical value: should streamlines be terminated when they
#'   start to loop back on themselves?
#' @param maxSteps An integer giving the maximum number of steps to take in
#'   each direction from a seed point.
#' @param stepLength A numeric value giving the distance, in mm, moved at
#'   each tracking step.
#' @param oneWay Logical value: if `TRUE`, tracking will proceed in only one
#'   direction from each seed point, rather than the usual two.
#' @return A [Tracker] object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#' @export
createTracker <- function (model, mask, curvatureThreshold = 0.2, loopcheck = TRUE, maxSteps = 2000, stepLength = 0.5, oneWay = FALSE)
{
    Tracker$new(model, mask, curvatureThreshold=curvatureThreshold, loopcheck=loopcheck, maxSteps=maxSteps, stepLength=stepLength, oneWay=oneWay)
}
