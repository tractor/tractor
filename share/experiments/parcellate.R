#@args session directory, segmentation images
#@desc Read and merge together parcellations for a T1-weighted image. Labels in segmentations specified later in the command will take priority over duplicates appearing in earlier segmentations. The special symbol '@' can be used to indicate that the session hierarchy should be checked for the parcellation in question, which is useful in combination with the "freesurf" script. Any existing parcellation will be taken as a starting point unless IgnoreExisting:true is given. A region description file in the format used in $TRACTOR_HOME/etc/parcellations must be provided for any parcellation type which is not specified there.

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "segmentation images")
    
    types <- getConfigVariable("Types", NULL, "character", errorIfMissing=TRUE)
    ignoreExisting <- getConfigVariable("IgnoreExisting", FALSE)
    
    session <- newSessionFromDirectory(Arguments[1])
    
    segmentationFiles <- splitAndConvertString(Arguments[-1], ",", fixed=TRUE)
    if (!is.null(types))
        types <- splitAndConvertString(types, ",", fixed=TRUE)
    if (length(segmentationFiles) != length(types))
        report(OL$Error, "A type must be specified for each segmentation file")
    
    parcellation <- NULL
    if (!ignoreExisting && session$imageExists("parcellation","structural"))
        parcellation <- readParcellation(session$getImageFileNameByType("parcellation","structural"))
    
    for (i in 2:nArguments())
    {
        regionFileName <- ensureFileSuffix(types[i], "txt")
        if (file.exists(regionFileName))
            regionFilePath <- regionFileName
        else if (file.exists(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "parcellations", regionFileName)))
            regionFilePath <- file.path(Sys.getenv("TRACTOR_HOME"), "etc", "parcellations", regionFileName)
        else
            report(OL$Error, "No region description file was found for parcellation type \"#{types[i]}\"")
        
        if (segmentationFiles[i] == "@")
            currentParcellation <- readParcellation(session$getImageFileNameByType(types[i]), regionFilePath)
        else
            currentParcellation <- readParcellation(segmentationFiles[i], regionFilePath)
        
        if (is.null(parcellation))
            parcellation <- currentParcellation
        else
        {
            duplicates <- which(parcellation$regions$label %in% currentParcellation$regions$label)
            if (length(duplicates) > 0)
            {
                parcellation$regions <- parcellation$regions[-duplicates,]
                parcellation$image <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x %in% parcellation$regions$index[duplicates], 0, x)
            }
            
            if (any(currentParcellation$regions$index %in% parcellation$regions$index))
                currentParcellation$regions$index <- currentParcellation$regions$index + max(parcellation$regions)
            parcellation$regions <- rbind(parcellation$regions, currentParcellation$regions)
            parcellation$image <- newMriImageWithBinaryFunction(parcellation$image, currentParcellation$image, function(x,y) ifelse(y==0,x,y))
        }
    }
    
    writeParcellation(parcellation$image, parcellation$regions, session$getImageFileNameByType("parcellation","structural"))
}
