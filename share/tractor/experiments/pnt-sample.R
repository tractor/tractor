library(tractor.nt)
library(splines)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    nSamples <- getConfigVariable("Samples", 1000)
    jitterSeed <- getConfigVariable("JitterSeed", TRUE)
    pointSeparation <- getConfigVariable("PointSeparation", 0.5)
    
    reference <- getNTResource("reference", "pnt", list(tractName=tractName))
    model <- getNTResource("model", "pnt", list(modelName=modelName,tractName=tractName))
    
    if (!reference$inStandardSpace() || reference$getSeedUnit() != "mm")
        report(OL$Error, "Reference must be in standard space and specified in millimetres")
    
    tract <- reference$getTract()
    tractOptions <- reference$getTractOptions()
    seed <- reference$getStandardSpaceSeedPoint()
    
    steps <- calculateSplineStepVectors(tract, "knot")
    leftSteps <- t(apply(steps$left, 1, function(x) x/vectorLength(x)))
    rightSteps <- t(apply(steps$right, 1, function(x) x/vectorLength(x)))
    
    # axes <- plot(tract)
    
    sampleMultinomialDistribution <- function(d) d$values[which(rmultinom(1,1,d$probs) == 1)]
    sampleStepVector <- function(v,a) {
        # Ref. for this algorithm: http://stackoverflow.com/a/29354206/3921960
        # 1. Generate an initial vector on the plane v.x = 0
        if (v[1] == 0)
            w <- vectorCrossProduct(v, c(1,0,0))
        else
            w <- vectorCrossProduct(v, c(0,0,1))
        # 2. Sample a rotation angle
        t <- runif(1, 0, 2*pi)
        # 3. Rotate the initial vector about the normal by the required angle
        w <- w * cos(t) + vectorCrossProduct(v,w) * sin(t) + v * (v %*% w) * (1 - cos(t))
        # 4. Scale the vector to the required length
        w <- w * tractOptions$knotSpacing * sin(a) / vectorLength(w)
        # 5. Translate the centre to the truncated target vector
        w <- w + (v * tractOptions$knotSpacing * cos(a))
        return (w)
    }
    
    brain <- getStandardImage("brain")
    voxelDims <- brain$getVoxelDimensions()
    sink <- getRefClass("StreamlineSink")$new(basename(tractName), brain)
    
    for (i in seq_len(nSamples))
    {
        leftLength <- sampleMultinomialDistribution(model$getLengthDistribution("left"))
        rightLength <- sampleMultinomialDistribution(model$getLengthDistribution("right"))
        
        currentSeed <- seed
        if (jitterSeed)
            currentSeed <- currentSeed + runif(3,-0.5,0.5)
        
        loc <- currentSeed
        points <- matrix(currentSeed, 1, 3)
        for (j in seq_len(min(leftLength, model$getRefLeftLength()))[-1])
        {
            angle <- acos(rbeta(1, model$getCosineDistribution(j,"left")$alpha, 1) * 2 - 1)
            loc <- loc + sampleStepVector(leftSteps[j,], angle)
            points <- rbind(points, loc)
        }
        seedIndex <- nrow(points)
        
        loc <- currentSeed
        points <- points[nrow(points):1,,drop=FALSE]
        for (j in seq_len(min(rightLength, model$getRefRightLength()))[-1])
        {
            angle <- acos(rbeta(1, model$getCosineDistribution(j,"right")$alpha, 1) * 2 - 1)
            loc <- loc + sampleStepVector(rightSteps[j,], angle)
            points <- rbind(points, loc)
        }
        
        indices <- seq(1, nrow(points), pointSeparation/tractOptions$knotSpacing)
        interpolatedPoints <- promote(apply(points, 2, mmand::resample, indices, mmand::mnKernel(1,0)), byrow=TRUE)
        # lines(interpolatedPoints[,axes[1]]-seed[axes[1]], interpolatedPoints[,axes[2]]-seed[axes[2]], col=rgb(1,0,0,0.1))
        
        seedIndex <- which.min(abs(indices - seedIndex))
        interpolatedPoints <- RNifti::worldToVoxel(interpolatedPoints,brain) - 1
        streamline <- getRefClass("Streamline")$new(interpolatedPoints, seedIndex, voxelDims, coordUnit="mm")
        sink$append(streamline)
    }
    
    ask("Press Enter to exit:")
}
