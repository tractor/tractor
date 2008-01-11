suppressPackageStartupMessages(require(tractor.fsl))

runExperiment <- function ()
{
    requireArguments("session directory", "centre point")
    
    session <- newSessionFromDirectory(Arguments[1])
    
    centre <- as.numeric(unlist(strsplit(Arguments[-1], ",")))
    if (!exists("centre") || length(centre) != 3)
        output(OL$Error, "Centre point must be given as a single vector in 3D space, comma or space separated")
    
    pointType <- getWithDefault("PointType", NULL, mode="character", errorIfMissing=TRUE)
    isStandardPoint <- getWithDefault("CentreInMNISpace", FALSE)
    
    width <- getWithDefault("Width", 7)
    roiName <- getWithDefault("ROIName", "roi")
    
    t2Image <- session$getImageByType("t2")
    centre <- getNativeSpacePointForSession(session, centre, pointType, isStandardPoint)
    
    roiImage <- newMriImageAsShapeOverlay("block", t2Image, centre=round(centre), width=width)
    writeMriImageToFile(roiImage, roiName)
}
