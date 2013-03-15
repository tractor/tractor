#@args T1W reference image, fMRI reference image, fMRI data image

library(RNiftyReg)
library(tractor.graph)

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
    
    maskImageNames <- getConfigVariable("MaskImageNames", NULL, "character")
    
    report(OL$Info, "Reading images")
    t1wImage <- readImageFile(Arguments[1])
    fmriRefImage <- readImageFile(Arguments[2])
    dataImage <- readImageFile(Arguments[3])
    maskImages <- lapply(maskImageNames, readImageFile, sparse=TRUE)
    maskNames <- sapply(maskImages, function (x) basename(x$getSource()))
    
    report(OL$Info, "Registering T1w image to fMRI space")
    reg <- registerImages(t1wImage, fmriRefImage, scope="affine")
    
    report(OL$Info, "Transforming masks into fMRI space...")
    transformedMaskImages <- lapply(maskImages, function (image) {
        report(OL$Verbose, "Transforming \"", basename(image$getSource()), "\"")
        currentReg <- registerImages(image, fmriRefImage, scope="affine", initAffine=reg$affine, nLevels=0, finalInterpolation=1)
        return (newMriImageWithDataRepresentation(currentReg$image, "coordlist"))
    })
    
    report(OL$Info, "Building up a composite mask image")
    mergedMask <- array(0L, dim=dim(fmriRefImage))
    maxValues <- array(0, dim=dim(fmriRefImage))
    for (i in seq_along(transformedMaskImages))
    {
        nonzeroLocs <- transformedMaskImages[[i]]$getData()$getCoordinates()
        imageValues <- transformedMaskImages[[i]]$getData()$getData()
        locsToUpdate <- which(maxValues[nonzeroLocs] < imageValues)
        if (length(locsToUpdate) > 0)
        {
            mergedMask[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- i
            maxValues[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- imageValues[locsToUpdate]
        }
        else
            report(OL$Warning, "Region \"", maskNames[i], "\" is unrepresented in the composite mask")
    }
    
    report(OL$Info, "Extracting principal components...")
    signals <- sapply(seq_along(transformedMaskImages), function (i) {
        currentLocs <- which(mergedMask == i, arr.ind=TRUE)
        if (nrow(currentLocs) == 0)
            return (rep(NA, dim(dataImage)[4]))
        
        currentData <- apply(dataImage$getData(), 4, "[", currentLocs)
        pca <- prcomp(t(currentData), scale.=FALSE)
        varianceExplained <- (pca$sdev^2)[1] / sum(pca$sdev^2) * 100
        report(OL$Verbose, "First principal component for region \"", maskNames[i], "\" covers ", round(varianceExplained,2), "% of the variance")
        return (pca$x[,1])
    })
    
    colnames(signals) <- maskNames
    graph <- newGraphFromTable(signals, method="correlation", allVertexNames=maskNames)
    graph$serialise("rsgraph.Rdata")
    
    invisible(NULL)
}
