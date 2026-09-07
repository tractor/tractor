setOldClass("tractOptions")

#' The ReferenceTract class
#'
#' This class represents a reference tract for probabilistic neighbourhood
#' tractography (PNT): a [BSplineTract] together with information about the
#' standard-space seed point that anchors it, the options used to generate
#' it, and (optionally) the session it was derived from. Reference tracts are
#' used as the basis for candidate tract matching and model fitting, and are
#' usually created by [newReferenceTractWithTract()].
#'
#' @field tract A [BSplineTract] object giving the reference tract itself.
#' @field standardSeed A numeric vector giving the location of the seed point
#'   used to generate `tract`, in standard (MNI) space.
#' @field seedUnit A string, either `"vox"` or `"mm"`, giving the units in
#'   which `standardSeed` (and any native-space seed point derived from it)
#'   are expressed.
#' @field session An optional [tractor.session::MriSession] object giving the
#'   session that `tract` was originally derived from, or `NULL` if the
#'   reference is considered to be in standard space only.
#' @field options An object of class `tractOptions`, as created by
#'   [createTractOptionList()], recording the options used to generate
#'   `tract` and candidate tracts to be matched against it.
#'
#' @export
ReferenceTract <- setRefClass("ReferenceTract", contains="SerialisableObject", fields=list(tract="BSplineTract",standardSeed="numeric",seedUnit="character",session=Optional("MriSession"),options="tractOptions"), methods=list(
    initialize = function (tract = nilObject(), standardSeed = NULL, seedUnit = "", session = NULL, options = NULL)
    {
        if (is.null(options))
            options <- structure(list(), class="tractOptions")
        else if (is.list(options) && !identical(class(options),"tractOptions"))
            options <- structure(options, class="tractOptions")
        
        if (is.nilObject(tract))
            object <- initFields(tract=as(tract,"BSplineTract"), standardSeed=as.numeric(standardSeed), seedUnit=seedUnit, session=session, options=as.list(options))
        else
            object <- initFields(tract=tract, standardSeed=as.numeric(standardSeed), seedUnit=seedUnit, session=session, options=as.list(options))
        
        if (length(object$options) > 0)
            names(object$options) <- names(options)
        if (!(object$seedUnit %in% c("vox","mm","")))
            report(OL$Error, "Seed point units must be \"vox\" or \"mm\"")
        
        return (object)
    },
    
    getTractOptions = function () { return (options) },
    
    getSeedUnit = function () { return (seedUnit) },
    
    getStandardSpaceSeedPoint = function () { return (standardSeed) },
    
    getSourceSession = function () { return (session) },
    
    getTract = function () { return (tract) },
    
    inStandardSpace = function () { return (is.null(session)) },
    
    summarise = function ()
    {
        "Summarise the reference tract"
        labels <- c("Tract class", "In standard space", "Standard space seed")
        values <- c(class(tract)[1], .self$inStandardSpace(), paste(implode(round(standardSeed,2), ","), " (", seedUnit, ")",sep=""))
        
        if (!is.null(session))
        {
            labels <- c(labels, "Source session")
            values <- c(values, session$getDirectory())
        }
        if (length(options) > 0)
        {
            labels <- c(labels, "Point type", "Length quantile", "Knot spacing")
            values <- c(values, options$pointType, options$lengthQuantile, round(options$knotSpacing,2))
            
            if (!is.null(options$maxPathLength))
            {
                labels <- c(labels, "Maximum knot count")
                values <- c(values, options$maxPathLength)
            }
        }
        
        return (list(labels=labels, values=values))
    }
))

#' Create a reference tract
#'
#' This function creates a [ReferenceTract] object from a fitted
#' [BSplineTract] and a seed point, converting a native-space seed point to
#' standard (MNI) space via the session's registration if a standard-space
#' seed is not supplied directly.
#'
#' @param tract A [BSplineTract] object giving the reference tract.
#' @param standardSeed A numeric vector giving the location of the seed point
#'   in standard (MNI) space, or `NULL` if `nativeSeed` and `session` are to
#'   be used to calculate it.
#' @param nativeSeed A numeric vector giving the location of the seed point
#'   in the native space of `session`. Only used if `standardSeed` is `NULL`.
#' @param session A [tractor.session::MriSession] object giving the session
#'   that `tract` was derived from, or `NULL` if a `standardSeed` is given
#'   directly and no source session is to be recorded.
#' @param options An object of class `tractOptions`, as created by
#'   [createTractOptionList()], or a plain list with the same elements. The
#'   default, `NULL`, indicates that no options should be stored.
#' @param seedUnit A string, either `"vox"` (the default) or `"mm"`, giving
#'   the units of `standardSeed` and `nativeSeed`.
#' @return A [ReferenceTract] object.
#' @author Jon Clayden
#' @references Please cite the following reference when using TractoR in your
#' work:
#'
#' J.D. Clayden, S. Muñoz Maniega, A.J. Storkey, M.D. King, M.E. Bastin & C.A.
#' Clark (2011). TractoR: Magnetic resonance imaging and tractography with R.
#' Journal of Statistical Software 44(8):1-18. \doi{10.18637/jss.v044.i08}.
#'
#' For the probabilistic neighbourhood tractography method specifically, see
#'
#' J.D. Clayden, A.J. Storkey & M.E. Bastin (2007). A probabilistic
#' model-based approach to consistent white matter tract segmentation. IEEE
#' Transactions on Medical Imaging 26(11):1555-1561.
#' @export
newReferenceTractWithTract <- function (tract, standardSeed = NULL, nativeSeed = NULL, session = NULL, options = NULL, seedUnit = c("vox","mm"))
{
    if (is.null(session) && is.null(standardSeed))
        report(OL$Error, "A standard space seed point must be given if the reference tract is not associated with a session")
    if (!is(tract,"BSplineTract"))
        report(OL$Error, "Only B-spline tracts can currently be used as references")
    
    seedUnit <- match.arg(seedUnit)
    
    if (is.null(standardSeed))
    {
        if (is.null(nativeSeed))
            report(OL$Error, "The native space point associated with the tract must be specified")
        
        transform <- session$getTransformation("diffusion", "mni")
        standardSeed <- tractor.reg::transformPoints(transform, nativeSeed, voxel=(seedUnit!="mm"))
    }
    
    reference <- ReferenceTract$new(tract=tract, standardSeed=standardSeed, seedUnit=seedUnit, session=session, options=options)
    invisible (reference)
}
