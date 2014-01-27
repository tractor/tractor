#@args session directory, segmentation images
#@desc Read and merge together parcellations for a T1-weighted image. Labels in segmentations specified later in the command will take priority over duplicates appearing in earlier segmentations. The special symbol '@' can be used to indicate that the session hierarchy should be checked for the parcellation in question, which is useful in combination with the "freesurf" script. Any parcellation type without Any existing parcellation will be taken as a starting point unless IgnoreExisting:true is given.

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
    
    if (!ignoreExisting && session$imageExists("parcellation","structural"))
    {
        
    }
}
