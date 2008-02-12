.ReferenceTract <- function (.tract, .standardSeed, .session = NULL, .options = NULL)
{
    if (length(grep("tract", class(.tract))) == 0)
        output(OL$Error, "The specified tract object is not valid")
    if (!is.numeric(.standardSeed) || length(.standardSeed) != 3)
        output(OL$Error, "Seed point must be given as a 3-vector")
    
    self <- list(
        getTractOptions = function () { return (.options) },
        
        getStandardSpaceSeedPoint = function () { return (.standardSeed) },
        
        getSourceSession = function () { return (.session) },
        
        getTract = function () { return (.tract) },
        
        inStandardSpace = function () { return (is.null(.session)) }
    )
    
    class(self) <- c("metatract.reference", "list.object", "list")
    self <- inherit(self, .tract)
    invisible (self)
}

isReferenceTract <- function (object)
{
    return ("metatract.reference" %in% class(object))
}

newReferenceTractWithTract <- function (tract, standardSeed = NULL, nativeSeed = NULL, session = NULL, options = NULL)
{
    if (is.null(session) && is.null(standardSeed))
        output(OL$Error, "A standard space seed point must be given if the reference tract is not associated with a session")
    if (!isFieldTract(tract) && !isBSplineTract(tract))
        output(OL$Error, "Only field tracts and B-spline tracts can currently be used as references")
    
    if (is.null(standardSeed))
    {
        if (isFieldTract(tract))
            nativeSeed <- tract$getSeedPoint()
        else if (is.null(nativeSeed))
            output(OL$Error, "The native space point associated with the tract must be specified")
        
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        standardSeed <- transformVoxelPointsWithAffine(xfm, nativeSeed)
    }
    
    reference <- .ReferenceTract(tract, standardSeed, session, options)
    invisible (reference)
}
