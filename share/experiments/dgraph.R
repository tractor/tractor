#@args [session directory]

library(splines)
library(tractor.session)
library(tractor.nt)
library(tractor.graph)
library(tractor.native)
library(RNiftyReg)
suppressPackageStartupMessages(library(oro.nifti))

runExperiment <- function ()
{
    registerImages <- function (from, to, ...)
    {
        from <- as(from, "nifti")
        to <- as(to, "nifti")
        result <- niftyreg(from, to, ...)
        result$image <- as(result$image, "MriImage")
        invisible(result)
    }
    
    selectionFunction <- function (x, values)
    {
        dims <- dim(x)
        data <- ifelse(x %in% values, 1, 0)
        dim(data) <- dims
        return (data)
    }
    
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    if (file.exists(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt")))
        lookupTable <- read.table(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"))
    else
        lookupTable <- read.table(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "FreeSurferColorLUT.txt"))
    
    diffusionDir <- session$getDirectory("diffusion")
    freesurferDir <- session$getDirectory("freesurfer")
    diffusionRoiDir <- session$getDirectory("diffusion-rois", createIfMissing=TRUE)
    freesurferRoiDir <- session$getDirectory("freesurfer-rois", createIfMissing=TRUE)
    
    if (!imageFileExists(file.path(freesurferDir, "mri", "rawavg")))
        report(OL$Error, "Freesurfer has not yet been run for this session")
    
    report(OL$Info, "Registering structural image to diffusion space")
    averageT1wImage <- newMriImageFromFile(file.path(freesurferDir, "mri", "rawavg"))
    refb0 <- session$getImageByType("refb0","diffusion")
    result <- registerImages(averageT1wImage, refb0, scope="affine")
    result <- registerImages(averageT1wImage, refb0, scope="nonlinear", initAffine=result$affine)
    controlPoints <- result$control[[1]]
    
    report(OL$Info, "Reading parcellation")
    parcellation <- session$getImageByType("desikan_killiany")
    
    regions <- tractor.graph:::.FreesurferRegionNameMapping$aparc
    allRegionNames <- paste(rep(names(regions),2), rep(c("left","right"),each=length(regions)), sep="_")
    nRegions <- length(allRegionNames)
    regionLocations <- matrix(NA, nrow=nRegions, ncol=3)
    regionSizes <- numeric(nRegions)
    
    report(OL$Info, "Transforming regions to diffusion space")
    i <- 1
    for (regionName in names(regions))
    {
        leftIndices <- lookupTable[,2] %in% paste("ctx-lh-",regions[[regionName]],sep="")
        rightIndices <- lookupTable[,2] %in% paste("ctx-rh-",regions[[regionName]],sep="")
        indices <- list(left=leftIndices, right=rightIndices)
        
        for (side in names(indices))
        {
            report(OL$Verbose, "Transforming region \"", regionName, "_", side, "\"")
            freesurferRoi <- newMriImageWithSimpleFunction(parcellation, selectionFunction, values=lookupTable[indices[[side]],1])
            writeMriImageToFile(freesurferRoi, file.path(freesurferRoiDir,paste(regionName,side,sep="_")))
            result <- registerImages(freesurferRoi, refb0, scope="nonlinear", initControl=controlPoints, nLevels=0, finalInterpolation=1)
            result$image <- newMriImageWithSimpleFunction(result$image, function (x) ifelse(x>0.2,1,0))
            writeMriImageToFile(result$image, file.path(diffusionRoiDir,paste(regionName,side,sep="_")))
            
            regionLocations[i,] <- apply(which(result$image$getData() > 0, arr.ind=TRUE), 2, median)
            regionLocations[i,] <- transformRVoxelToWorld(regionLocations[i,], result$image$getMetadata(), useOrigin=FALSE)
            regionSizes[i] <- sum(result$image$getData() > 0)
            i <- i + 1
        }
    }
    
    order <- c(seq(1,nRegions,2), seq(2,nRegions,2))
    regionLocations <- regionLocations[order,]
    regionSizes <- regionSizes[order]
    
    report(OL$Info, "Performing tractography")
    fa <- session$getImageByType("FA")
    mask <- newMriImageByThresholding(fa, 0.2)
    seeds <- which(mask$getData() > 0, arr.ind=TRUE)
    seeds <- seeds + runif(length(seeds), -0.5, 0.5)
    result <- trackWithSession(session, seeds, nSamples=1, requireImage=FALSE, requireStreamlines=TRUE)
    
    report(OL$Info, "Finding streamlines passing through each region")
    matchingIndices <- parallelApply(seq_along(allRegionNames), function (i) {
        report(OL$Verbose, "Matching region \"", allRegionNames[i], "\"")
        region <- newMriImageFromFile(file.path(diffusionRoiDir,allRegionNames[i]))
        return (findWaypointHits(result$streamlines, list(region)))
    })
    
    report(OL$Info, "Creating connectivity matrix")
    connectivityMatrix <- matrix(NA, nrow=nRegions, ncol=nRegions)
    for (i in seq_along(allRegionNames))
    {
        for (j in seq_along(allRegionNames))
            connectivityMatrix[i,j] <- length(intersect(matchingIndices[[i]], matchingIndices[[j]])) / mean(regionSizes[c(i,j)])
    }
    connectivityMatrix[is.nan(connectivityMatrix)] <- 0
    rownames(connectivityMatrix) <- allRegionNames
    colnames(connectivityMatrix) <- allRegionNames
    
    report(OL$Info, "Creating and writing graph")
    graph <- newGraphFromConnectionMatrix(connectivityMatrix, directed=FALSE)
    graph$setVertexLocations(regionLocations, "mm")
    graph$serialise("dgraph.Rdata")
    
    invisible(NULL)
}
