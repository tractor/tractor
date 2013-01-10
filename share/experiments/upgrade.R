#@desc Upgrade a serialised R object to the format used by the latest version of TractoR. A backup copy of the original ".Rdata" file will be created first.
#@args file name

library(tractor.session)
library(splines)
library(tractor.nt)

runExperiment <- function ()
{
    requireArguments("file name")
    
    convertObject <- function (x)
    {
        originalClass <- attr(x, "originalClass")
        if (is.null(originalClass))
            report(OL$Error, "This does appear to be a valid TractoR object")
        else if (originalClass[1] == "session.mri")
            object <- MriSession$new(x$directory)
        else if (originalClass[1] == "metadata.image.mri")
            object <- MriImage$new(imageDims=x$imagedims, voxelDims=x$voxdims, voxelDimUnits=x$voxunit, source=x$source, origin=x$origin)
        else if (originalClass[1] == "image.mri")
            object <- MriImage$new(imageDims=x$metadata$imagedims, voxelDims=x$metadata$voxdims, voxelDimUnits=x$metadata$voxunit, source=x$metadata$source, origin=x$metadata$origin, data=x$data)
        else if (originalClass[1] == "tract.field")
            object <- FieldTract$new(x$seed, convertObject(x$image))
        else if (originalClass[1] == "tract.bspline")
            object <- BSplineTract$new(splineDegree=x$degree, splineModels=x$splineModels, knotPositions=attr(x$splineBasis,"knots"), knotLocations=x$knotLocations, seedKnot=x$seedKnot)
        else if (originalClass[1] == "model.tract.uninformative")
            object <- UninformativeTractModel$new(lengthDistributions=x$lengthDistributions, refLengths=x$refLengths, pointType=x$pointType)
        else if (originalClass[1] == "model.tract.matching")
        {
            # Deal with old symmetric models without left and right distributions
            if (!is.list(x$cosineDistributions[[1]]))
                x$cosineDistributions <- list(left=x$cosineDistributions[1:x$refLengths$left], right=x$cosineDistributions[1:x$refLengths$right])
            
            # Update the class of the cosine distributions
            x$cosineDistributions$left <- lapply(x$cosineDistributions$left, function (d) { if (identical(d,NA)) d else structure(d,class="betaDistribution") })
            x$cosineDistributions$right <- lapply(x$cosineDistributions$right, function (d) { if (identical(d,NA)) d else structure(d,class="betaDistribution") })
            
            # Update the class of the length distributions
            x$lengthDistributions <- lapply(x$lengthDistributions, structure, class="multinomialDistribution")
            
            object <- MatchingTractModel$new(cosineDistributions=x$cosineDistributions, lengthDistributions=x$lengthDistributions, refSpline=convertObject(x$refSpline), refLengths=x$refLengths, pointType=x$pointType)
        }
        else if (originalClass[1] == "metatract.reference")
        {
            if (!is.null(x$session))
                x$session <- convertObject(x$session)
            if (!is.null(x$options))
                x$options <- structure(x$options, class="tractOptions")
            
            object <- ReferenceTract$new(convertObject(x$tract), x$standardSeed, x$seedUnit, x$session, x$options)
        }
        else if (originalClass[1] == "results.nt.heuristic")
            object <- HeuristicNTResults$new(results=x$results)
        else if (originalClass[1] == "results.nt.probabilistic")
        {
            if (!is.null(x$uninformativeModel))
                x$uninformativeModel <- convertObject(x$uninformativeModel)
            
            object <- ProbabilisticNTResults$new(tractPosteriors=x$tractPosteriors, nullPosteriors=x$nullPosterior, matchingModel=convertObject(x$matchingModel), uninformativeModel=x$uninformativeModel)
        }
        else
            report(OL$Error, "I don't know how to convert objects of class \"", originalClass[1], "\"")
        
        return (object)
    }
    
    object <- convertObject(deserialiseReferenceObject(Arguments[1], raw=TRUE))
    
    backupFileName <- ensureFileSuffix(paste(ensureFileSuffix(Arguments[1],NULL,strip="Rdata"), "backup", sep="_"), "Rdata")
    file.copy(Arguments[1], backupFileName, overwrite=TRUE)
    
    object$serialise(Arguments[1])
    
    invisible(NULL)
}
