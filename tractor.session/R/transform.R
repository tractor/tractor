readEddyCorrectTransformsForSession <- function (session, index = NULL)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    require("tractor.reg")
    
    logFile <- file.path(session$getDirectory("fdt"), "data.ecclog")
    if (!file.exists(logFile))
        report(OL$Error, "Eddy current correction log not found")
    logLines <- readLines(logFile)
    logLines <- subset(logLines, logLines %~% "^[0-9\\-\\. ]+$")
    
    connection <- textConnection(logLines)
    matrices <- as.matrix(read.table(connection))
    close(connection)
    
    if (is.null(index))
        index <- seq_len(nrow(matrices) / 4)
    
    matrices <- lapply(index, function(i) matrices[(((i-1)*4)+1):(i*4),])
    
    image <- session$getImageByType("refb0", "diffusion")
    transform <- Transformation$new(sourceImage=image, targetImage=image, affineMatrices=matrices, controlPointImages=list(), reverseControlPointImages=list(), method="flirt")
    
    invisible (transform)
}

.resolveSpace <- function (space, session)
{
    spacePieces <- unlist(strsplit(space, ":", fixed=TRUE))
    if (length(spacePieces) == 2)
    {
        if (spacePieces[1] != session$getDirectory())
            report(OL$Error, "The current space does not correspond to the specified session")
        else
            return (spacePieces[2])
    }
    else
        return (spacePieces[1])
}

.constructSpace <- function (space, session)
{
    if (tolower(space) == "mni")
        return ("mni")
    else
        return (paste(session$getDirectory(), space, sep=":"))
}

transformImageToSpace <- function (image, session, newSpace, oldSpace = NULL, preferAffine = FALSE, reverseRegister = FALSE, finalInterpolation = 1, ...)
{
    require("tractor.reg")
    
    guessSpace <- function ()
    {
        if (image$isInternal())
            return (NULL)
        else if (dirname(image$getSource()) == .StandardBrainPath)
            return ("mni")
        else
        {
            for (space in setdiff(names(.RegistrationTargets),"mni"))
            {
                if (dirname(image$getSource()) == session$getDirectory(space))
                    return (space)
            }
        }
        
        return (NULL)
    }
    
    if (is.null(oldSpace))
        oldSpace <- guessSpace()
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    if (reverseRegister)
        transform <- session$getTransformation(.resolveSpace(newSpace,session), .resolveSpace(oldSpace,session), ...)
    else
        transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session), ...)
    
    newImage <- transformImage(transform, image, preferAffine=preferAffine, reverse=reverseRegister, finalInterpolation=finalInterpolation)
    
    return (newImage)
}

transformPointsToSpace <- function (points, session, newSpace, oldSpace = NULL, pointType = NULL, outputVoxel = FALSE, preferAffine = FALSE, reverseRegister = FALSE, nearest = FALSE, ...)
{
    require("tractor.reg")
    
    if (is.null(pointType))
    {
        pointType <- attr(points, "pointType")
        if (is.null(pointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    pointType <- match.arg(tolower(pointType), c("fsl","r","vox","mm"))
    if (pointType == "fsl")
        points <- points + 1
    
    if (is.null(oldSpace))
        oldSpace <- attr(points, "space")
    
    if (is.null(oldSpace) || is.null(newSpace))
        report(OL$Error, "Source and target spaces are not both defined")
    
    if (reverseRegister)
        transform <- session$getTransformation(.resolveSpace(newSpace,session), .resolveSpace(oldSpace,session), ...)
    else
        transform <- session$getTransformation(.resolveSpace(oldSpace,session), .resolveSpace(newSpace,session), ...)
    
    if (outputVoxel && pointType == "mm")
    {
        if (reverseRegister)
            points <- changePointType(points, transform$getTargetImage(), "r", "mm")
        else
            points <- changePointType(points, transform$getSourceImage(), "r", "mm")
        pointType <- "r"
    }
    
    newPoints <- transformPoints(transform, points, voxel=(pointType!="mm"), preferAffine=preferAffine, reverse=reverseRegister, nearest=nearest)
    
    attr(newPoints, "space") <- .constructSpace(newSpace, session)
    attr(newPoints, "pointType") <- ifelse(pointType=="mm", "mm", "r")
    
    return (newPoints)
}

changePointType <- function (points, image, newPointType, oldPointType = NULL)
{
    require("tractor.reg")
    
    if (is.null(oldPointType))
    {
        oldPointType <- attr(points, "pointType")
        if (is.null(oldPointType))
            report(OL$Error, "Point type is not stored with the points and must be specified")
    }
    
    # NB: point types "r" and "vox" are equivalent
    oldPointType <- match.arg(tolower(oldPointType), c("fsl","r","vox","mm"))
    newPointType <- match.arg(tolower(newPointType), c("fsl","r","vox","mm"))
    
    offsets <- list(fsl=1, r=0, vox=0)
    
    if (oldPointType == newPointType)
        newPoints <- points
    else if (oldPointType == "mm" && newPointType != "mm")
        newPoints <- transformWorldToVoxel(points, image) - offsets[[newPointType]]
    else if (oldPointType != "mm" && newPointType == "mm")
        newPoints <- transformVoxelToWorld(points + offsets[[oldPointType]], image)
    else
        newPoints <- points + offsets[[oldPointType]] - offsets[[newPointType]]
    
    attr(newPoints, "pointType") <- ifelse(newPointType=="vox", "r", newPointType)
    
    return (newPoints)
}
