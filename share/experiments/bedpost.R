#@args [session directory]
#@desc Runs the FSL BEDPOSTX algorithm to fit a "ball-and-sticks" model to diffusion MRI data using Markov chain Monte Carlo. Note that BEDPOSTX will be run using a 2 fibre model at each voxel by default - this allows for (up to) 2 fibre directions within each voxel. This maximum can be changed with the NumberOfFibres option, although run time will increase somewhat if it is set higher. In any case this process typically takes several hours. If BEDPOSTX has previously been run for the specified session directory (default "."), then it will not be run again unless Force:true is given.

library(tractor.session)

runExperiment <- function ()
{
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    nFibres <- getConfigVariable("NumberOfFibres", 2, "integer")
    howRunBedpost <- getConfigVariable("Context", "fg", validValues=c("fg","bg","screen"))
    force <- getConfigVariable("Force", FALSE)
    
    if (!force && imageFileExists(session$getImageFileNameByType("avf")))
        report(OL$Info, "BEDPOSTX has been previously run for this session - use Force:true to run it again")
    else
    {
        scheme <- session$getDiffusionScheme()
        if (is.null(scheme))
            report(OL$Error, "No diffusion gradient directions are stored for this session")
        
        dataMetadata <- session$getImageByType("data", "diffusion", metadataOnly=TRUE)
        maskMetadata <- session$getImageByType("mask", "diffusion", metadataOnly=TRUE)
        
        if (!equivalent(dataMetadata$getDimensions()[1:3], maskMetadata$getDimensions()))
            report(OL$Error, "Data and mask dimensions do not match")
        if (dataMetadata$getDimensions()[4] != sum(scheme$nDirections()))
            report(OL$Error, "Total number of directions (", sum(scheme$nDirections()), ") does not match the data file (", dataMetadata$getDimensions()[4], " volumes)")
        
        runBedpostWithSession(session, nFibres, how=howRunBedpost)
    }
}
