#@args session directory, image type
#@desc Print the full path to the specified image type within a session directory. Some image types, such as "data", can exist in multiple places, and in these cases the Place option can be used to disambiguate the request. The Index option can be use to choose between indexed images, such as diffusion tensor eigenvalue maps.
#@nohistory TRUE

library(tractor.session)

runExperiment <- function ()
{
    requireArguments("session directory", "image type")
    
    place <- getConfigVariable("Place", NULL, "character")
    index <- getConfigVariable("Index", 1L, "integer")
    
    session <- attachMriSession(Arguments[1])
    path <- session$getImageFileNameByType(Arguments[2], place, index)
    
    cat(paste(path, "\n", sep=""))
}
