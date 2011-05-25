setClassUnion("Tract", c("FieldTract","BSplineTract"))

setClassUnion("MriSessionOrNull", c("MriSession","NULL"))

ReferenceTract <- setRefClass("ReferenceTract", contains="SerialisableObject", fields=list(tract="Tract",standardSeed="numeric",seedUnit="character",session="MriSessionOrNull",options="list"), methods=list(
    initialize = function (tract = nilObject(), standardSeed = NULL, seedUnit = NULL, session = NULL, options = list())
    {
        object <- initFields(tract=tract, standardSeed=standardSeed, seedUnit=seedUnit, session=session, options=options)
        
        if (length(object$options) > 0)
            names(object$options) <- names(options)
        if (length(object$seedUnit) > 0 && !(object$seedUnit %in% c("vox","mm")))
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
        report(OL$Info, "Tract class        : ", class(tract)[1])
        report(OL$Info, "In standard space  : ", .self$inStandardSpace())
        report(OL$Info, "Standard space seed: ", implode(round(standardSeed,2), ","), " (", seedUnit, ")")
        if (!is.null(session))
            report(OL$Info, "Source session     : ", session$getDirectory())
        if (length(options) > 0)
        {
            report(OL$Info, "Point type         : ", options$pointType)
            report(OL$Info, "Length quantile    : ", options$lengthQuantile)
            report(OL$Info, "Knot spacing       : ", round(options$knotSpacing,2))
            if (!is.null(options$maxPathLength))
                report(OL$Info, "Maximum knot count : ", options$maxPathLength)
        }
    }
))

newReferenceTractWithTract <- function (tract, standardSeed = NULL, nativeSeed = NULL, session = NULL, options = NULL, seedUnit = c("vox","mm"))
{
    if (is.null(session) && is.null(standardSeed))
        report(OL$Error, "A standard space seed point must be given if the reference tract is not associated with a session")
    if (!is(tract,"FieldTract") && !is(tract,"BSplineTract"))
        report(OL$Error, "Only field tracts and B-spline tracts can currently be used as references")
    
    seedUnit <- match.arg(seedUnit)
    
    if (is.null(standardSeed))
    {
        if (is(tract, "FieldTract"))
            nativeSeed <- tract$getSeedPoint()
        else if (is.null(nativeSeed))
            report(OL$Error, "The native space point associated with the tract must be specified")
        
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        
        if (seedUnit == "mm")
            standardSeed <- transformWorldPointsWithAffine(xfm, nativeSeed)
        else
            standardSeed <- transformVoxelPointsWithAffine(xfm, nativeSeed)
    }
    
    reference <- ReferenceTract$new(tract=tract, standardSeed=standardSeed, seedUnit=seedUnit, session=session, options=options)
    invisible (reference)
}
