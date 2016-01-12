#' @rdname newMriImageWithData
#' @export
newMriImageAsShapeOverlay <- function (type = c("cross","block"), baseImage, ...)
{
    type <- match.arg(type)
    data <- generateImageDataForShape(type, baseImage$getDimensions(), ...)
    image <- newMriImageWithData(data, baseImage)
    
    invisible (image)
}

#' @rdname newMriImageWithData
#' @export
generateImageDataForShape <- function (type = c("cross","block"), dim, background = 0, centre = NA, width = NA)
{
    type <- match.arg(type)
    data <- array(background, dim=dim)
    
    if (type %in% c("cross","block"))
    {
        if (is.na(centre) || is.na(width))
            report(OL$Error, "The ", type, " shape requires a centre and width")
        if (width %% 2 != 1)
            report(OL$Error, "Width in voxels should be an odd number")
        
        steps <- buildStepVectors(width)
        if (type == "cross")
            steps <- steps[, colSums(steps!=0) < 2]
        
        locs <- t(centre + steps)
        outOfBounds <- as.logical(colSums(apply(locs,1,">",dim) + t(locs<=0)))
        locs <- locs[!outOfBounds,,drop=FALSE]
        
        data[locs] <- 1
    }
    
    invisible (data)
}
