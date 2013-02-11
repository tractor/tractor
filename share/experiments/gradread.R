#@args session directory, text file, large b-value, [small b-value]
#@desc Read diffusion gradient directions from a text file, and update the specified session directory. The text file should contain the diffusion gradient vectors applied to the data set, given either one-per-column or one-per-row, normalised or unnormalised, and with or without zeroes for b=0 measurements. The small b-value is assumed to be zero if it is not explicitly specified.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "text file", "large b-value")
    session <- newSessionFromDirectory(Arguments[1])
    fileName <- Arguments[2]
    largeBValue <- as.numeric(Arguments[3])
    
    if (nArguments() > 3)
        smallBValue <- as.numeric(Arguments[4])
    else
        smallBValue <- 0
    
    metadata <- session$getImageByType("rawdata", "diffusion", metadataOnly=TRUE)
    
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
    
    nVectorsFile <- ncol(bvecs)
    nVectorsImage <- metadata$getDimensions()[4]
    report(OL$Info, "Gradient vector matrix has ", nVectorsFile, " columns; data image contains ", nVectorsImage, " volumes")
    
    bvals <- rep(NA, nVectorsImage)
    
    if (nVectorsFile == nVectorsImage && smallBValue == 0)
    {
        lowBValueIndices <- which(bvecs[1,]==0 & bvecs[2,]==0 & bvecs[3,]==0)
        highBValueIndices <- setdiff(1:nVectorsFile, lowBValueIndices)
    }
    else
    {
        report(OL$Info, "Loading data image for more information")
        dataImage <- session$getImageByType("rawdata", "diffusion")
        
        meanIntensities <- numeric(nVectorsImage)
        for (i in seq_len(nVectorsImage))
            meanIntensities[i] <- mean(dataImage[,,,i])
        
        kMeansResult <- kmeans(meanIntensities, 2, nstart=3)
        
        if (nVectorsFile != nVectorsImage)
        {
            if (!(nVectorsFile %in% kMeansResult$size))
                report(OL$Error, "K-means cluster sizes are ", kMeansResult$size[1], " and ", kMeansResult$size[2], " - neither matches your text file")
            else
            {
                highBValueCluster <- which(kMeansResult$size == nVectorsFile)
                lowBValueCluster <- ifelse(highBValueCluster==1, 2, 1)
                if (kMeansResult$centers[highBValueCluster] > kMeansResult$centers[lowBValueCluster])
                    report(OL$Warning, "High b-value images seem to have higher mean signal")
            }
        }
        else
        {
            highBValueCluster <- which.min(kMeansResult$centers)
            lowBValueCluster <- which.max(kMeansResult$centers)
        }
        
        highBValueIndices <- which(kMeansResult$cluster == highBValueCluster)
        lowBValueIndices <- which(kMeansResult$cluster == lowBValueCluster)
        report(OL$Info, "Image volume(s) ", implode(lowBValueIndices,sep=", ",finalSep=" and "), " appear(s) to have lower diffusion weighting")
        
        if (nVectorsFile != nVectorsImage)
        {
            newBVecs <- matrix(NA, nrow=3, ncol=nVectorsImage)
            newBVecs[,highBValueIndices] <- bvecs
            newBVecs[,lowBValueIndices] <- 0
            bvecs <- newBVecs
        }
    }
    
    bvals[highBValueIndices] <- largeBValue
    bvals[lowBValueIndices] <- smallBValue
    
    report(OL$Info, "Writing gradient direction files")
    scheme <- newSimpleDiffusionSchemeWithDirections(bvecs, bvals)
    writeSimpleDiffusionSchemeForSession(session, scheme)
}
