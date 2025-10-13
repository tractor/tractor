setOldClass("tractOptions")

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
