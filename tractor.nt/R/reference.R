.ReferenceTract <- function (.tract, .standardSeed, .seedUnit, .session, .options)
{
    if (length(grep("tract", class(.tract))) == 0)
        output(OL$Error, "The specified tract object is not valid")
    if (!is.numeric(.standardSeed) || length(.standardSeed) != 3)
        output(OL$Error, "Seed point must be given as a 3-vector")
    if (!(.seedUnit %in% c("vox","mm")))
        output(OL$Error, "Seed point units must be \"vox\" or \"mm\"")
    
    self <- list(
        getTractOptions = function () { return (.options) },
        
        getSeedUnit = function () { return (.seedUnit) },
        
        getStandardSpaceSeedPoint = function () { return (.standardSeed) },
        
        getSourceSession = function () { return (.session) },
        
        getTract = function () { return (.tract) },
        
        inStandardSpace = function () { return (is.null(.session)) },
        
        summarise = function ()
        {
            output(OL$Info, "Tract class        : ", class(.tract)[1])
            output(OL$Info, "In standard space  : ", self$inStandardSpace())
            output(OL$Info, "Standard space seed: ", implode(round(.standardSeed,2), ","), " (", .seedUnit, ")")
            if (!is.null(.session))
                output(OL$Info, "Source session     : ", .session$getBaseDirectory())
            if (isTractOptionList(.options))
            {
                output(OL$Info, "Point type         : ", .options$pointType)
                output(OL$Info, "Length quantile    : ", .options$lengthQuantile)
                output(OL$Info, "Knot spacing       : ", round(.options$knotSpacing,2))
                if (!is.null(.options$maxPathLength))
                    output(OL$Info, "Maximum knot count : ", .options$maxPathLength)
            }
        }
    )
    
    class(self) <- c("metatract.reference", "list.object", "list")
    self <- inherit(self, .tract)
    invisible (self)
}

isReferenceTract <- function (object)
{
    return ("metatract.reference" %in% class(object))
}

deserialiseReferenceTract <- function (file = NULL, object = NULL)
{
    if (is.null(object))
        object <- deserialiseListObject(file, raw=TRUE)
    
    if (isDeserialisable(object$tract, "tract.bspline"))
        object$tract <- deserialiseBSplineTract(object=object$tract)
    else if (isDeserialisable(object$tract, "tract.field"))
        object$tract <- deserialiseFieldTract(object=object$tract)
    else
        output(OL$Error, "Deserialised object contains no valid tract")
    
    if (isDeserialisable(object$session, "session.mri"))
        object$session <- deserialiseMriSession(object=object$session)
    
    reference <- deserialiseListObject(NULL, object, .ReferenceTract, defaults=list(seedUnit="vox"))
    invisible (reference)
}

newReferenceTractWithTract <- function (tract, standardSeed = NULL, nativeSeed = NULL, session = NULL, options = NULL, finalSeedUnit = c("vox","mm"))
{
    if (is.null(session) && is.null(standardSeed))
        output(OL$Error, "A standard space seed point must be given if the reference tract is not associated with a session")
    if (!isFieldTract(tract) && !isBSplineTract(tract))
        output(OL$Error, "Only field tracts and B-spline tracts can currently be used as references")
    
    finalSeedUnit <- match.arg(finalSeedUnit)
    
    if (is.null(standardSeed))
    {
        if (isFieldTract(tract))
            nativeSeed <- tract$getSeedPoint()
        else if (is.null(nativeSeed))
            output(OL$Error, "The native space point associated with the tract must be specified")
        
        xfm <- newAffineTransform3DByInversion(getMniTransformForSession(session))
        standardSeed <- transformVoxelPointsWithAffine(xfm, nativeSeed)
    }
    
    if (finalSeedUnit == "mm")
    {
        standardSpaceMetadata <- newMriImageMetadataFromFile(getFileNameForStandardImage("brain"))
        standardSeed <- transformRVoxelToWorld(standardSeed, standardSpaceMetadata, useOrigin=TRUE)
    }
    
    reference <- .ReferenceTract(tract, standardSeed, finalSeedUnit, session, options)
    invisible (reference)
}
