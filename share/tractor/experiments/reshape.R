#@desc Reshape the data in an image, and/or permute its dimensions. This script should be used with care, and only if you know what you are doing. A typical usage might be after inaccurate reconstruction of a complex DICOM file series. Each argument after the image should be either a series of comma-separated integers (e.g. 128,128,50), giving the new dimensions of the data; or a series of comma-separated letters, possibly with negation signs (e.g. x,-z,y), giving the dimension permutation required. These operations will be performed in the order specified. Note that only the data in the image is changed: the metadata stays the same, except for padding where required. If you don't understand what that means, you should probably not use this script! The original image will be OVERWRITTEN.
#@args image file, new dimensions and/or axis permutations
#@group General analysis

runExperiment <- function ()
{
    requireArguments("image file", "new dimensions and/or axis permutations")
    
    image <- readImageFile(Arguments[1])
    data <- image$getData()
    
    voxelDims <- voxelDimUnits <- origin <- NA
    
    for (i in 2:nArguments())
    {
        request <- splitAndConvertString(Arguments[i], ",", fixed=TRUE)
        if (isValidAs(request, "integer"))
        {
            dim(data) <- as.integer(request)
            dimensionalityDifference <- length(dim(data)) - image$getDimensionality()
            
            if (dimensionalityDifference > 0)
            {
                voxelDims <- c(image$getVoxelDimensions(), rep(1,dimensionalityDifference))
                origin <- c(1,1,1,0,0,0,0)[1:length(dim(data))]
                origin[1:image$getDimensionality()] <- image$getOrigin()
                
                # If we're adding a temporal dimension where none previously existed, don't pretend we know the units
                if (length(dim(data)) > 3 && image$getDimensionality() <= 3 && "temporal" %in% names(image$getVoxelUnits()))
                    voxelDimUnits <- image$getVoxelUnits()[setdiff(names(image$getVoxelUnits()),"temporal")]
            }
        }
        else if (all(request %~% "^-?[t-zT-Z]$"))
        {
            nDims <- length(dim(data))
            
            dimPermutation <- match(sub("^-","",tolower(request),perl=TRUE), c(letters[24:26],letters[20:23]))
            if (!all(1:nDims %in% dimPermutation))
                report(OL$Error, "The axis labels ", implode(c(letters[24:26],letters[20:23])[1:nDims],sep=", ",finalSep=" and "), " must all appear in the permutation string")
            
            data <- aperm(data, dimPermutation)
            
            flips <- which(request[dimPermutation] %~% "^-")
            if (length(flips) > 0)
            {
                indices <- alist(i=,j=,k=,t=,u=,v=,w=)[1:nDims]
                for (j in flips)
                    indices[[j]] <- dim(data)[j]:1
                data <- as.array(do.call("[", c(list(data),indices,list(drop=FALSE))))
            }
        }
    }
    
    newImage <- asMriImage(data, image, voxelDims=voxelDims, voxelDimUnits=voxelDimUnits, origin=origin)
    writeImageFile(newImage, image$getSource())
    
    invisible(NULL)
}
