createCaminoFilesForSession <- function (session, diffusionTime = NULL)
{
    if (!isMriSession(session))
        output(OL$Error, "The specified session is not an MriSession object")
    
    # This may be integrated into the session object at some point
    caminoDir <- file.path(session$getWorkingDirectory(), "camino")
    if (file.exists(caminoDir))
    {
        ans <- output(OL$Question, "Internal directory ", caminoDir, " exists. This operation will DESTROY it. Continue? [yn]")
        if (tolower(ans) != "y")
            return (invisible(NULL))
        else
            unlink(caminoDir, recursive=TRUE)
    }
    
    sourceDir <- session$getPreBedpostDirectory()
    if (!file.exists(file.path(sourceDir,"bvals")) || !file.exists(file.path(sourceDir,"bvecs")) || !imageFileExists(file.path(sourceDir,"data")) || !imageFileExists(file.path(sourceDir,"nodif_brain_mask")))
        output(OL$Error, "Some required files are missing - the session must be processed for FSL first")
    
    dir.create(caminoDir)
    
    output(OL$Info, "Creating a scheme file")
    bvals <- unlist(read.table(file.path(sourceDir, "bvals")))
    bvecs <- as.matrix(read.table(file.path(sourceDir, "bvecs")))
    
    if (min(bvals) != 0)
        output(OL$Info, "Minimal b-value in this data set is ", min(bvals), " rather than 0")
    
    zeroes <- which(bvals == min(bvals))
    if (length(zeroes) == 0)
        output(OL$Error, "No b-values are specified for this data set")
    nonzeroes <- setdiff(seq_along(bvals), zeroes)
    volumeOrder <- c(zeroes, nonzeroes)
    
    if (is.null(diffusionTime))
        diffusionTime <- as.numeric(output(OL$Question, "What is the diffusion time for this data set in seconds?"))

    scheme <- c(diffusionTime, length(bvals))
    for (i in volumeOrder)
    {
        modQ <- sqrt(bvals[i] * 1e6 / diffusionTime)
        bvecLength <- vectorLength(bvecs[,i])
        scheme <- c(scheme, bvecs[,i] / ifelse(bvecLength==0,1,bvecLength) * modQ)
    }
    scheme <- round(scheme, 3)
    write.table(as.matrix(scheme), file.path(caminoDir,"sequence.scheme"), row.names=FALSE, col.names=FALSE)
    
    output(OL$Info, "Copying data and mask images")
    dataImage <- newMriImageFromFile(file.path(sourceDir,"data"))
    data <- dataImage[,,,volumeOrder]
    newDataImage <- newMriImageWithData(data, dataImage$getMetadata())
    writeMriImageToCamino(newDataImage, file.path(caminoDir,"data"))
    maskImage <- newMriImageFromFile(file.path(sourceDir,"nodif_brain_mask"))
    writeMriImageToCamino(maskImage, file.path(caminoDir,"mask"))
    
    invisible (NULL)
}
