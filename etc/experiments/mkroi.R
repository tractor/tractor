suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("session directory", "centre point")
    
    session <- newSessionFromDirectory(Arguments[1])
    if (is.character(Arguments[2]))
    {
        centre <- as.numeric(unlist(strsplit(Arguments[2], ",")))
        if (length(centre) == 1 && is.na(centre))
            output(OL$Error, "Centre point must be comma or space separated")
    }
    else if (is.numeric(Arguments[2]))
        centre <- Arguments[-1]
    
    if (!exists("centre") || length(centre) != 3)
        output(OL$Error, "Centre point must be given as a single vector in 3D space")
    
    pointType <- getWithDefault("PointType", NULL, mode="character", errorIfMissing=TRUE)
    pointType <- match.arg(tolower(pointType), c("fsl","r","mm"))
    isStandardPoint <- getWithDefault("CentreInMNISpace", FALSE)
    
    width <- getWithDefault("Width", 7)
    roiName <- getWithDefault("ROIName", "roi")
    
    t2Image <- session$getImageByType("t2")
    
    if (pointType == "fsl")
        centre <- transformFslVoxelToRVoxel(centre)
    
    if (isStandardPoint)
        centre <- transformStandardSpaceSeeds(session, centre, unit=ifelse(pointType=="mm","mm","vox"))
    else if (pointType == "mm")
        centre <- transformWorldToRVoxel(centre, t2Image$getMetadata(), useOrigin=TRUE)
    
    roiImage <- newMriImageAsShapeOverlay("block", t2Image, centre=round(centre), width=width)
    writeMriImageToFile(roiImage, roiName)
}
