#@args session directory, seed point
#@desc Run tractography using the specified session, seedMaskFile and a mask, which constrains tractography. The seed points are estimated once the nifti files are loaded and therefore the seed points is normally in native diffusion space. Note that in FSL (voxel coordinates, beginning at zero), R (voxel coordinates, beginning at one) or mm (world coordinates) conventions. The PointType and SeedInMNISpace options need to be set to reflect these choices. Standard space seed points are transferred to native space using the FLIRT registration algorithm. The tract produced may be visualised in projection by giving CreateImages:true.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "seed mask")
    
    session <- newSessionFromDirectory(Arguments[1])
	writeDir <- session$getDirectory(type="probtrack",createIfMissing=TRUE)	
	
    seedMaskFile <- Arguments[2]
	restrictMaskFile <- Arguments[3]
	print(session)
	print(seedMaskFile)
	print(restrictMaskFile)
	
	force_track <- getConfigVariable("force_track", FALSE)  #if files already exist tracking will be aborted
	
	oldStrmsFiles <- list.files(writeDir, pattern="tract_.*_streamlines.Rdata", full.names = TRUE)
	if(length(oldStrmsFiles)>0){
		if(force_track){
			report(OL$Warning, "tract_.*_streamlines.Rdata files are being removed!")
			for(i in 1:length(oldStrmsFiles))
				execute("rm", paramString = oldStrmsFiles[i])
		}
		else{
			return( invisible(NULL) )
		}
	}
	
	seedMask <- readImageFile(seedMaskFile)
	seed0 <- which(seedMask$getData()!=0,arr.ind = T)
	numSd <- dim(seed0)[1]
	     
    pointType <- getConfigVariable("PointType", "r", "character", validValues=c("fsl","r","mm"), errorIfInvalid=TRUE)
    isStandardSeed <- getConfigVariable("SeedInMNISpace", FALSE)
    
    tracker <- getConfigVariable("Tracker", "tractor", validValues=c("fsl","tractor"))
    
    useGradientAscent <- getConfigVariable("UseGradientAscent", FALSE)
    thresholdType <- getConfigVariable("GradientAscentThresholdType", "FA", validValues=c("FA","MD","axialdiff","radialdiff"), deprecated=TRUE)
    thresholdLevel <- getConfigVariable("GradientAscentThresholdLevel", 0.2, deprecated=TRUE)
    newThresholdLevel <- getConfigVariable("GradientAscentAnisotropyThreshold", 0.2)
    
    # The new threshold level takes priority if it differs from the old one
    if (thresholdLevel != newThresholdLevel)
        thresholdLevel <- newThresholdLevel
    
    nSamples <- getConfigVariable("NumberOfSamples", 100)
    force <- getConfigVariable("Force", FALSE)
    
    createVolumes <- getConfigVariable("CreateVolumes", FALSE)
    createImages <- getConfigVariable("CreateImages", FALSE)
    storeStreamlines <- getConfigVariable("StoreStreamlines", TRUE)
    vizThreshold <- getConfigVariable("VisualisationThreshold", 0.01)
    showSeed <- getConfigVariable("ShowSeedPoint", TRUE)
    
    if (storeStreamlines && tracker == "fsl")
        report(OL$Error, "Streamlines may only be stored when using the internal tracker")
    
    #seed0 <- getNativeSpacePointForSession(session, seed0, pointType, isStandardSeed)
    
    if (useGradientAscent)
    {
        thresholdImage <- session$getImageByType(tolower(thresholdType), "diffusion")
        currentSeed <- seed
        currentValue <- thresholdImage$getDataAtPoint(seed)
        
        while (currentValue < thresholdLevel)
        {
            neighbourhood <- createNeighbourhoodInfo(centre=currentSeed, width=3)
            nextLoc <- which.max(thresholdImage[t(neighbourhood$vectors)])
            
            if (equivalent(neighbourhood$vectors[,nextLoc], currentSeed))
            {
                report(OL$Warning, "Dead end reached")
                break
            }
            else
            {
                currentSeed <- neighbourhood$vectors[,nextLoc]
                currentValue <- thresholdImage$getDataAtPoint(currentSeed)
            }
            
            report(OL$Verbose, "Ascending to voxel ", implode(currentSeed,","), " with ", thresholdType, " value ", currentValue)
        }
        
        seed <- currentSeed
    }
    
	#regulate the number of seeds per run
	const0 <- 100000/nSamples
	if(numSd>const0)
		countT <- ceiling(numSd/const0)
	else
		countT <- 1
			
	for(j in 1:countT){
		
		print( paste(as.character(j),"from",as.character(countT),sep=" ") )
		lim0 <- (j-1)*const0 + 1
		lim1 <- j*const0
		if(lim1>numSd)
			lim1=numSd
		seed <- seed0[lim0:lim1,]
		tractName <- getConfigVariable( "TractName", paste("tract",as.character(j), sep="_") )
	    #report(OL$Info, "Using seed point ", implode(seed,","), " for tractography")
	    if (tracker == "fsl")
	        result <- runProbtrackWithSession(session, seed, mode="simple", requireImage=TRUE, nSamples=nSamples, force=force)
	    else
	    {
	        require("tractor.track")
	        result <- trackWithSession(session, seed, requireImage=TRUE, nSamples=nSamples, requireStreamlines=storeStreamlines, maskName = restrictMaskFile, terminateOutsideMask=TRUE)
	        if (storeStreamlines)
	            result$streamlines$serialise(file.path(writeDir, paste(tractName,"streamlines",sep="_") ) )
	    }
        
		report(OL$Info, "Creating tract images")
    	if (createVolumes)
        	writeImageFile(result$image, tractName)
    	if (createImages)
        	writePngsForResult(result, prefix=tractName, threshold=vizThreshold, showSeed=showSeed)
		rm(result)
	}	#for(j in 1:countT)
    
    invisible (NULL)
}
