#@desc Create a graph based on diffusion data in the specified session (default "."). This script performs coregistration of T1-weighted structural data to the reference b=0 volume, extraction of segmented ROIs, tractography and graph creation. FreeSurfer and FSL-BEDPOSTX must have been run first (see "freesurf" and "bedpost").
#@args [session directory]

library(splines)
library(tractor.session)
library(tractor.nt)
library(tractor.graph)
library(tractor.track)
library(tractor.reg)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
	#specify lookuptable, parcellation file and T1 file for adding extra regions of interest
	lookupTableFile <- getConfigVariable("LookupTableFile", NULL, "character")  #specify extra regions to be incorporated
	parcellationFile <- getConfigVariable("ParcellationFile", NULL, "character")
	T1File <- getConfigVariable("T1File", NULL, "character")
	terminationFlag <- getConfigVariable("TerminationFlag", TRUE)
	saveStreamLFlag <- getConfigVariable("SaveStreamLFlag", FALSE)
	numStrPerSeed <- getConfigVariable("NumStrPerSeed",2, "numeric")
	suffixS <- getConfigVariable("SuffixS", "test","character")

    if (file.exists(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"))){
        lookupTable <- read.table(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"))
    } else
        lookupTable <- read.table(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "FreeSurferColorLUT.txt"))

			
	if ( !is.null(lookupTableFile) )
		lookupTable2 <- read.table(lookupTableFile, stringsAsFactors=FALSE)
	
    tmpDir <- file.path(session$getDirectory(), "rois" )
	if(!file.exists(tmpDir))
		dir.create(tmpDir)	
    diffusionRoiDir <- file.path( tmpDir,"diffusion" )
	if(!file.exists(diffusionRoiDir))
		dir.create(diffusionRoiDir)	
    #diffusionRoiDir <- session$getDirectory("diffusion-rois", createIfMissing=TRUE)
	
	outputDirDebug <- file.path(diffusionRoiDir,"debug")
	if(saveStreamLFlag){
		if(!file.exists(outputDirDebug))
			dir.create(outputDirDebug)
	}

	freesurferDir <- session$getDirectory("freesurfer")
	if (!imageFileExists(file.path(freesurferDir, "mri", "rawavg")))
		report(OL$Error, "Freesurfer has not yet been run for this session")
	averageT1wImage <- newMriImageFromFile(file.path(freesurferDir, "mri", "rawavg"))
	
	if( !is.null(T1File) )
		averageT1wImage2 <- newMriImageFromFile(T1File)
	
	freesurferRoiDir <- session$getDirectory("freesurfer-rois", createIfMissing=TRUE)
	report(OL$Info, "Reading freesurfer parcellation")
	parcellation <- session$getImageByType("desikan_killiany")
	

	if ( !is.null(parcellationFile) ){
	    #customRoiDir <- session$getDirectory("customparcellation-rois", createIfMissing=TRUE)
	    report(OL$Info, "Reading custom parcellation")
	    parcellation2 <- newMriImageFromFile( file.path(parcellationFile) )
	}
	
	if( !is.null(parcellationFile) | !is.null(lookupTableFile) ){
	    #perform some checks on regions
		parc_labels2 <- unique(as.vector(parcellation2$data))
		parc_labels2 <- setdiff( parc_labels2,0 )  #exclude zero
		areasDif1 <- setdiff( parc_labels2,lookupTable2[,1] ) #areas in parc_labels that are not included in lookupTable
		areasDif2 <- setdiff( lookupTable2[,1], parc_labels2 ) #areas in lookupTable that are not included in parcellation file
	
		if(length(areasDif1)>0)
			report(OL$Warning, 'Labels in parcellation that are not included in lookupTable will be ignored:',areasDif1 )

		if(length(areasDif2)>0)
			report(OL$Warning, 'Labels in lookupTable are not found in parcellation:',areasDif2 )
	
		parc_labels2 <- sort( intersect(parc_labels2,lookupTable2[,1]) )
		regions2 <- list()
		for(i in 1:length(parc_labels2) ){
			index <- which(lookupTable2[,1]==parc_labels2[i])
			regions2[[i]] <- lookupTable2[index,2] 
		}
	
		allRegionNames2 <- regions2		
	} 

    regions <- tractor.graph:::.FreesurferRegionNameMapping$aparc
    allRegionNames <- paste(rep(names(regions),2), rep(c("left","right"),each=length(regions)), sep="_")
	allRegionNames <- c(allRegionNames,allRegionNames2)
    nRegions <- length(allRegionNames)
    regionLocations <- matrix(NA, nrow=nRegions, ncol=3)
    regionSizes <- numeric(nRegions)		
	
    
    report(OL$Info, "Registering structural image to diffusion space: Freesurfer")
    refb0 <- session$getImageByType("refb0","diffusion")
    result <- registerImages(averageT1wImage, refb0, method="niftyreg", types=c("affine","nonlinear"), estimateOnly=TRUE)
	
	if( !is.null(T1File) ){
	    report(OL$Info, "Registering structural image to diffusion space: Specified T1 File")
        result2 <- registerImages(averageT1wImage2, refb0, method="niftyreg", types=c("affine","nonlinear"), estimateOnly=TRUE)
	}
	
		
    report(OL$Info, "Transforming freesurfer regions to diffusion space")
    i <- 1
	transformedMaskImages <- list()
    for (regionName in names(regions))
    {
        leftIndices <- lookupTable[,2] %in% paste("ctx-lh-",regions[[regionName]],sep="")
        rightIndices <- lookupTable[,2] %in% paste("ctx-rh-",regions[[regionName]],sep="")
        indices <- list(left=leftIndices, right=rightIndices)
        
        for (side in names(indices))
        {
            report(OL$Verbose, "Transforming region \"", regionName, "_", side, "\"")
			freesurferRoi <- newMriImageWithData(parcellation$getData(),parcellation$getMetadata() )
			index <- which(as.array(freesurferRoi$getData()!=0) & as.array(freesurferRoi$getData())!=lookupTable[indices[[side]],1] )
			freesurferRoi$data[index] <- 0
			index <- which( as.array(freesurferRoi$getData()!=0) )
			freesurferRoi$data[index] <- 1
			freesurferRoi <- newMriImageWithDataRepresentation(freesurferRoi,"coordlist")    #use a sparse representation
						
            #writeMriImageToFile(freesurferRoi, file.path(freesurferRoiDir,paste(regionName,side,sep="_")))
            transformedImage <- transformImage(result$transform, freesurferRoi)
            transformedMaskImages[[i]] <- newMriImageWithDataRepresentation(transformedImage, "coordlist")
			
			allRegionNames[[i]] <- paste(side,"_",regions[[regionName]],sep="")

            i <- i + 1
        }
    }	

	#create sub-mask images -- custom parcellation
	maskImages2 <- list()
	for (label in parc_labels2){
		mask <- newMriImageWithData(parcellation2$getData(),parcellation2$getMetadata() )
		index <- which(as.array(mask$getData()!=0) & as.array(mask$getData())!=label )
		mask$data[index] <- 0
		index <- which( as.array(mask$getData()!=0)  )
		mask$data[index] <- 1
		mask <- newMriImageWithDataRepresentation(mask,"coordlist")    #use a sparse representation
		#maskName <- file.path(customRoiDir,paste('tmp_',label,'.nii.gz', sep='') )
		maskImages2 <- c(maskImages2,mask)
	}
	
	
    report(OL$Info, "Transforming custom parcellation masks into native space...")
    transformedMaskImages2 <- lapply(maskImages2, function (image) {
        report(OL$Verbose, "Transforming \"", basename(image$getSource()), "\"")
        transformedImage <- transformImage(result2$transform, image)
        return (newMriImageWithDataRepresentation(transformedImage, "coordlist"))
    })	
	rm(maskImages2)
	
	#concatenate transformed masks
	if( !is.null(parcellationFile) ){
		transformedMaskImages <- c(transformedMaskImages,transformedMaskImages2)
	}
   
    report(OL$Info, "Building up a composite mask image")
    mergedMask <- array(0L, dim=dim(refb0))
    maxValues <- array(0, dim=dim(refb0))
    for (i in seq_along(transformedMaskImages))
    {
        nonzeroLocs <- transformedMaskImages[[i]]$getData()$getCoordinates()
        imageValues <- transformedMaskImages[[i]]$getData()$getData()
        locsToUpdate <- which( maxValues[nonzeroLocs] < imageValues & imageValues >= 0.2 )
        if (length(locsToUpdate) > 0)
        {
            mergedMask[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- i
            maxValues[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- imageValues[locsToUpdate]
        }
        else
            report(OL$Warning, "Region \"", allRegionNames[i], "\" is unrepresented in the composite mask")
		
		regionLocations[i,] <- apply(which(mergedMask==i, arr.ind=TRUE), 2, median)
		regionLocations[i,] <- transformVoxelToWorld(regionLocations[i,], transformedMaskImages[[i]]$getMetadata() )
		#regionLocations[i,] <- transformRVoxelToWorld(regionLocations[i,], transformedMaskImages[[i]]$getMetadata(), useOrigin=FALSE)
		regionSizes[i] <- sum(as.array(transformedMaskImages[[i]]$getData()) > 0)
    }
	
    report(OL$Info, "Saving the composite mask image")
	mergedMaskMriImg <- newMriImageWithData(mergedMask,refb0$getMetadata() ) 
	mergedMaskName <- file.path(diffusionRoiDir,"mergedMask.nii.gz")
	writeMriImageToFile(mergedMaskMriImg,fileName=mergedMaskName)  #save merged masked to be used for tractography

	#create the termination mask:
	if(terminationFlag){
		terminateOutsideMask <- TRUE 
		maskName <- session$getImageFileNameByType("mask", "diffusion")
		brainMaskImg <- newMriImageFromFile(maskName)
		brainMask <- brainMaskImg$getData()
		terminationMask <- mergedMask==0 & brainMask==1
		terminationMask <- terminationMask*1
		terminationMaskMri <- newMriImageWithData( terminationMask,refb0$getMetadata() )
		terminationMaskMriName <- file.path(diffusionRoiDir,"terminationMask.nii.gz")
		writeMriImageToFile(terminationMaskMri,fileName=terminationMaskMriName)  #save merged masked to be used for tractography
	} else{
		terminationMaskMriName <- NULL
		terminateOutsideMask <- FALSE 
	}
	       
    report(OL$Info, "Performing tractography")
    fa <- session$getImageByType("FA")
    mask <- newMriImageByThresholding(fa, 0.2)
	if(!is.na(wmLabelsF)){
		wmLabelsF <- getConfigVariable("WMLabelsFile", NULL, "character")
		wmLabels <- as.numeric(read.table(wmLabelsF)[,1])
        parc_b0 <- transformImage(result$transform, parcellation, finalInterpolation=0)
		parc_b0 <- parc_b0$getData()  #use white matter for seeding
		parc_wm <- parc_b0 %in% wmLabels
		parc_wm <- array(parc_wm,dim(parc_b0))
		seeds <- which(parc_wm,arr.ind=TRUE)
	}else 
	{
		seeds <- which((mask$getData() > 0.2 & terminationMask==1), arr.ind=TRUE)
	}
	
    seeds <- seeds + runif(length(seeds), -0.5, 0.5)
    result <- trackWithSession(session, seeds, nSamples=numStrPerSeed, requireImage=FALSE, maskName=terminationMaskMriName, requireStreamlines=TRUE, terminateOutsideMask=terminateOutsideMask)
	if( saveStreamLFlag )
		result$streamlines$serialise( file.path(outputDirDebug,"streamlines.Rdata") )
    
    report(OL$Info, "Finding streamlines passing through each region")
	matchingIndices <- list()
	for (i in seq_along(transformedMaskImages)){
        report(OL$Verbose, "Matching region \"", allRegionNames[i], "\"")
		dataM <- array( 0, dim(mergedMask) )
		indextmp <- which(mergedMask==i)
		dataM[indextmp] <- i
		region <- newMriImageWithData( dataM, mergedMaskMriImg$getMetadata() )
        matchingIndices[[i]] <- findWaypointHits(result$streamlines, list(region))
	}
    
    report(OL$Info, "Creating connectivity matrix")
    NumStreamsConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)
	FAConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)      #sums fa values in voxels only once
	FAWConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)		#sums fa values in voxels each time they are visited
	MDConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)      #sums md values in voxels only once
	MDWConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)		#sums md values in voxels each time they are visited
	LenStreamsMatrix <- matrix(0, nrow=nRegions, ncol=nRegions) #average length of streamlines connecting each pair of regions
	NumUniqueVoxs <- matrix(0, nrow=nRegions, ncol=nRegions)    #number of voxels that have been visited (Voxels only counted once)
	NumVisVoxs <- matrix(0, nrow=nRegions, ncol=nRegions)		#number of visits across all voxels of the tracks
	connectivityMatrix <- matrix(0, nrow=nRegions, ncol=nRegions) #here it is intantiated as number of streamlines divided by average voxel number
	md <- session$getImageByType("MD")
	flag_notPass <- TRUE
	totSN <- 0
    for (i in seq_along(allRegionNames))
    {
        for (j in seq_along(1:i)){
			if(i==j)
				next
			ConStreams <- intersect(matchingIndices[[i]], matchingIndices[[j]])  #streamlines indices that connect two regions
			lenC <- length(ConStreams) 

			if( lenC==0 )
				next
            NumStreamsConMatrix[j,i] <- lenC 
			totSN <- totSN+lenC 
			# report(OL$Info,i," ",j," ", lenC," ",totSN)                   
			newTrack <- newStreamlineCollectionTractBySubsetting(result$streamlines,ConStreams)
			startInd <- newTrack$getStartIndices()
			endInd <- newTrack$getEndIndices()
			LenStreamsMatrix[j,i] <- mean(endInd-startInd+1)
			
			visitationMap <- newMriImageAsVisitationMap( newTrack,fa$getMetadata() )
			
			if( saveStreamLFlag && i!=j ){
				if(flag_notPass){  #initialise
					totTrack <- newTrack$copy()
					flag_notPass <- FALSE
				} else{
					totTrack <- newStreamlineCollectionTractByMerging(totTrack,newTrack)
				}
			}
			
			indVisVox <- which( visitationMap$getData()!=0 & !is.nan(visitationMap$getData()) )
			FAWConMatrix[j,i] <- sum( fa$getData()[indVisVox] * visitationMap$getData()[indVisVox] )
			FAConMatrix[j,i] <- sum( fa$getData()[indVisVox] )
			MDWConMatrix[j,i] <- sum( md$getData()[indVisVox] * visitationMap$getData()[indVisVox] )
			MDConMatrix[j,i] <- sum( md$getData()[indVisVox] )
			NumUniqueVoxs[j,i] <- length(indVisVox)
			NumVisVoxs[j,i] <- sum( visitationMap$getData()[indVisVox] )
						
		}  #for (j in seq_along(allRegionNames))
	}  #for (i in seq_along(allRegionNames))
	
	if( saveStreamLFlag ){
		maskPath <- file.path(outputDirDebug,"track_masks")
		if(!file.exists(maskPath))
			dir.create(maskPath)
		perm <- unique( as.vector(mergedMask) )
		numROIs <- length(perm)-1 #exclude zero label
		fileNBase <- paste("streamlinesExtracted",as.character(numROIs),sep="_") 

		report( OL$Info,"Saving track to Rdata format...")
		totTrack$serialise( file.path(outputDirDebug, fileNBase) )
		report( OL$Info,"Saving track to trackvis format..." )
		writeStreamlineCollectionTractToTrackvis(totTrack, file.path(outputDirDebug, paste(fileNBase,".trk",sep="") ) )		
		report( OL$Info,"Export masks in nifti format...")
		for(r in seq_along(perm)){
			if(perm[r]==0)
				next
			roi <- array(0,dim=dim(refb0))
			ind <- which(mergedMask==perm[r])
			roi[ind] <- 1
			newRoiImg <- newMriImageWithData(roi,templateImage=refb0)
			writeMriImageToFile(newRoiImg,fileName=file.path(maskPath, paste(allRegionNames[perm[r]],".nii.gz",sep="")),fileType="NIFTI_GZ")
		}		
	}
	
    NumStreamsConMatrix[lower.tri(NumStreamsConMatrix,diag=FALSE)] <- NA
	diag(NumStreamsConMatrix) <- NA
	indEdges <- which(!is.na(NumStreamsConMatrix) & NumStreamsConMatrix != 0, arr.ind=TRUE)
    connectivityMatrix[indEdges] <- 1
    rownames(connectivityMatrix) <- allRegionNames
    colnames(connectivityMatrix) <- allRegionNames
	
	names(regionSizes) <- allRegionNames

    VoxelDims <- abs(refb0$voxelDims)  #voxel dimensions for estimating volume
	
	stepLength <- result$streamlines$summarise()$values[2]
	m <- gregexpr('.*\\s',stepLength)
	stepLength <- as.numeric(regmatches(stepLength,m))
	LenStreamsMatrix <- LenStreamsMatrix * stepLength 
	
    report(OL$Info, "Creating and writing graph")
    graph <- asGraph(connectivityMatrix, directed=FALSE, ignoreSelfConnections=TRUE)
	graph$setVertexAttributes(NumVoxelsRegion=regionSizes, VoxelDims=VoxelDims)
	graph$setEdgeAttributes(FAConMatrix=FAConMatrix[indEdges], FAWConMatrix=FAWConMatrix[indEdges], MDConMatrix=MDConMatrix[indEdges], MDWConMatrix=MDWConMatrix[indEdges], LenStreamsMatrix=LenStreamsMatrix[indEdges], NumUniqueVoxs=NumUniqueVoxs[indEdges], NumVisVoxs=NumVisVoxs[indEdges], NumStreamsConMatrix=NumStreamsConMatrix[indEdges])
    graph$setVertexLocations(regionLocations, "mm")
    graph$serialise( paste("dgraph_",suffixS,".Rdata",sep="") )
    
    invisible(NULL)
}
