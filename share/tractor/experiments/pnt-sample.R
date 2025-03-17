#@desc Sample synthetic streamlines, in MNI standard space, using a reference tract and PNT dispersion model. Beginning at the seed or anchor point stored with the reference tract, the algorithm steps outwards in each direction in turn. At each step it samples an angular deviation from the reference tract from the model, and a direction from a uniform distribution over the resulting circle (see Muñoz Maniega et al., J. Imaging, 2018 for details). Each streamline is resampled to the specified point separation, and then saved to a .trk file for visualisation. The result is an indication of the range of "plausible" deviations from the reference tract, under the model.
#@group Probabilistic neighbourhood tractography

library(tractor.reg)
library(tractor.track)
library(tractor.nt)

runExperiment <- function ()
{
    tractName <- getConfigVariable("TractName", NULL, "character", errorIfMissing=TRUE)
    modelName <- getConfigVariable("ModelName", NULL, "character")
    nSamples <- getConfigVariable("Samples", 1000)
    jitterSeed <- getConfigVariable("JitterSeed", TRUE)
    pointSeparation <- getConfigVariable("PointSeparation", 0.5)
    
    reference <- getNTResource("reference", list(tractName=tractName))
    model <- getNTResource("model", list(modelName=modelName,tractName=tractName))
    
    if (!reference$inStandardSpace() || reference$getSeedUnit() != "mm")
        report(OL$Error, "Reference must be in standard space and specified in millimetres")
    
    tract <- reference$getTract()
    tractOptions <- reference$getTractOptions()
    seed <- reference$getStandardSpaceSeedPoint()
    
    steps <- calculateSplineStepVectors(tract, "knot")
    leftSteps <- t(apply(steps$left, 1, function(x) x*c(-1,1,1)/vectorLength(x)))
    rightSteps <- t(apply(steps$right, 1, function(x) x*c(-1,1,1)/vectorLength(x)))
    
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
        w <- w * cos(t) + vectorCrossProduct(v,w) * sin(t) + v * as.vector(v %*% w) * (1 - cos(t))
        # 4. Scale the vector to the required length
        w <- w * tractOptions$knotSpacing * sin(a) / vectorLength(w)
        # 5. Translate the centre to the truncated target vector
        w <- w + (v * tractOptions$knotSpacing * cos(a))
        return (w)
    }
    
    brain <- getStandardImage("brain")
    
    # Ellipsis argument is ignored, but lapply() always passes an argument
    synthesiseStreamline <- function (...)
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
        
        seedIndex <- which.min(abs(indices - seedIndex))
        interpolatedPoints <- transformWorldToVoxel(interpolatedPoints, brain)
        return (asStreamline(interpolatedPoints, seedIndex, image=brain, coordUnit="vox"))
    }
    
    streamlines <- lapply(seq_len(nSamples), synthesiseStreamline)
    attachStreamlines(streamlines)$writeStreamlines(basename(tractName))
}
