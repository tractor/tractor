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
    structuralDir <- session$getDirectory("structural")
    freesurferDir <- session$getDirectory("freesurfer")
    diffusionRoiDir <- session$getDirectory("diffusion-rois", createIfMissing=TRUE)
    structuralRoiDir <- session$getDirectory("structural-rois", createIfMissing=TRUE)
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
    
    report(OL$Info, "Transforming regions to diffusion space")
    regions <- tractor.graph:::.FreesurferRegionNameMapping$aparc
    for (regionName in names(regions))
    {
        report(OL$Verbose, "Transforming region \"", regionName, "\"")
        leftIndices <- lookupTable[,2] %in% paste("ctx-lh-",regions[[regionName]],sep="")
        rightIndices <- lookupTable[,2] %in% paste("ctx-rh-",regions[[regionName]],sep="")
        indices <- list(left=leftIndices, right=rightIndices)
        
        for (side in names(indices))
        {
            freesurferRoi <- newMriImageWithSimpleFunction(parcellation, selectionFunction, values=lookupTable[indices[[side]],1])
            writeMriImageToFile(freesurferRoi, file.path(freesurferRoiDir,paste(regionName,side,sep="_")))
            result <- registerImages(freesurferRoi, refb0, scope="nonlinear", initControl=controlPoints, nLevels=0)
            writeMriImageToFile(result$image, file.path(diffusionRoiDir,paste(regionName,side,sep="_")))
        }
    }
    
    report(OL$Info, "Performing tractography")
    fa <- session$getImageByType("FA")
    mask <- newMriImageByThresholding(fa, 0.2)
    seeds <- which(mask$getData() > 0, arr.ind=TRUE)
    seeds <- seeds + runif(length(seeds), -0.5, 0.5)
    result <- trackWithSession(session, seeds, nSamples=1, requireImage=FALSE, requireStreamlines=TRUE)
    
    report(OL$Info, "Creating connectivity matrix")
    allRegionNames <- paste(rep(names(regions),2), rep(c("left","right"),each=length(regions)), sep="_")
    connectivityMatrix <- matrix(NA, nrow=length(allRegionNames), ncol=length(allRegionNames))
    rownames(connectivityMatrix) <- allRegionNames
    colnames(connectivityMatrix) <- allRegionNames
    
    for (i in 1:length(allRegionNames))
    {
        region1 <- newMriImageFromFile(file.path(diffusionRoiDir,allRegionNames[i]))
        for (j in i:length(allRegionNames))
        {
            region2 <- newMriImageFromFile(file.path(diffusionRoiDir,allRegionNames[j]))
            streamlines <- newStreamlineCollectionTractWithWaypointConstraints(result$streamlines, list(region1,region2))
            if (!is.null(streamlines))
                connectivityMatrix[i,j] <- connectivityMatrix[j,i] <- streamlines$nStreamlines() / ((sum(region1$getData()>0)+sum(region2$getData()>0)) / 2)
        }
    }
    
    report(OL$Info, "Creating and writing graph")
    graph <- newGraphFromConnectionMatrix(connectivityMatrix)
    graph$serialise("dgraph.Rdata")
    
    invisible(NULL)
}
