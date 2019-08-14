#@args session directory, text file, b-values
#@desc Read diffusion gradient directions from a text file, and update the specified session directory. The text file should contain the diffusion gradient vectors applied to the data set, given either one-per-column or one-per-row, normalised or unnormalised, and with or without zeroes for b=0 measurements. If exactly one b-value is given then zero is assumed to be used in addition; otherwise all b-values must be given. A file containing the b-values for each volume in order may also be given as the third argument.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "text file", "b-values")
    session <- attachMriSession(Arguments[1])
    fileName <- Arguments[2]
    
    bValueFile <- FALSE
    if (nArguments() == 3 && file.exists(Arguments[3]))
    {
        bValueFile <- TRUE
        bvals <- as.vector(read.table(Arguments[3]))
    }
    else
    {
        bValues <- sort(as.numeric(Arguments[-(1:2)]))
        if (length(bValues) == 1)
            bValues <- c(0, bValues)
        nBValues <- length(unique(bValues))
    }
    
    bvecs <- as.matrix(read.table(fileName))
    if (nrow(bvecs) != 3)
    {
        if (ncol(bvecs) == 3)
        {
            report(OL$Info, "Transposing gradient vector matrix")
            bvecs <- t(bvecs)
        }
        else
            report(OL$Error, "The gradient vector matrix must have one dimension equal to 3")
    }
    
    if (!bValueFile)
    {
        report(OL$Info, "Loading raw data image")
        dataImage <- session$getImageByType("rawdata", "diffusion")
        
        nVectorsFile <- ncol(bvecs)
        nVectorsImage <- dataImage$getDimensions()[4]
        bvals <- rep(NA, nVectorsImage)
        report(OL$Info, "Gradient vector matrix has #{nVectorsFile} columns; data image contains #{nVectorsImage} volumes")
        
        meanIntensities <- dataImage$apply(4, mean, na.rm=TRUE)
        kMeansResult <- kmeans(meanIntensities, nBValues, nstart=3)
    
        if (nVectorsFile != nVectorsImage)
        {
            if (min(bValues) != 0 || nBValues > 2)
                report(OL$Error, "Partial gradient vector matrices can only be used when there is one nonzero b-value")
            else if (!(nVectorsFile %in% kMeansResult$size))
                report(OL$Error, "K-means cluster sizes are #{kMeansResult$size[1]} and #{kMeansResult$size[2]} - neither matches your text file")
            else
            {
                highBValueCluster <- which(kMeansResult$size == nVectorsFile)
                lowBValueCluster <- ifelse(highBValueCluster==1, 2, 1)
                if (kMeansResult$centers[highBValueCluster] > kMeansResult$centers[lowBValueCluster])
                    report(OL$Warning, "High b-value images seem to have higher mean signal")
            
                highBValueIndices <- which(kMeansResult$cluster == highBValueCluster)
                lowBValueIndices <- which(kMeansResult$cluster == lowBValueCluster)
                report(OL$Info, "Image volume(s) ", implode(lowBValueIndices,sep=", ",finalSep=" and "), " appear(s) to have lower diffusion weighting")
            
                newBVecs <- matrix(NA, nrow=3, ncol=nVectorsImage)
                newBVecs[,highBValueIndices] <- bvecs
                newBVecs[,lowBValueIndices] <- 0
                bvecs <- newBVecs
                bvals[highBValueIndices] <- max(bValues)
                bvals[lowBValueIndices] <- 0
            }
        }
        else
        {
            clusterOrder <- order(kMeansResult$centers, decreasing=TRUE)
            for (i in seq_len(nBValues))
            {
                indices <- which(kMeansResult$cluster == clusterOrder[i])
                bvals[indices] <- bValues[i]
            }
        }
    }
    
    report(OL$Info, "Writing gradient direction files")
    scheme <- SimpleDiffusionScheme$new(bvals, t(bvecs))
    session$updateDiffusionScheme(scheme, unrotated=TRUE)
    
    print(scheme)
}
