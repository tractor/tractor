newMriImageAsShapeOverlay <- function (type = c("cross","block"), baseImage, ...)
{
    type <- match.arg(type)
    data <- generateImageDataForShape(type, baseImage$getDimensions(), ...)
    datatype <- list(type="integer", size=1, isSigned=FALSE)
    metadata <- newMriImageMetadataFromTemplate(baseImage$getMetadata(), datatype=datatype)
    image <- newMriImageWithData(data, metadata)
    
    invisible (image)
}

generateImageDataForShape <- function (type = c("cross","block"), dim, background = 0, centre = NA, width = NA)
{
    type <- match.arg(type)
    data <- array(background, dim=dim)
    
    if (type %in% c("cross","block"))
    {
        if (is.na(centre) || is.na(width))
            output(OL$Error, "The ", type, " shape requires a centre and width")
        if (width %% 2 != 1)
            output(OL$Error, "Width in voxels should be an odd number")
        
        steps <- buildStepVectors(width)
        if (type == "cross")
            steps <- steps[, colSums(steps!=0) < 2]
        
        locs <- t(centre + steps)
        outOfBounds <- as.logical(colSums(apply(locs,1,">",dim) + t(locs<=0)))
        locs <- locs[!outOfBounds,]
        
        data[locs] <- 1
    }
    
    invisible (data)
}
