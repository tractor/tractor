#@desc Create a graph based on diffusion data in the specified session (default "."). This script performs coregistration of T1-weighted structural data to the reference b=0 volume, extraction of segmented ROIs, tractography and graph creation. FreeSurfer and FSL-BEDPOSTX must have been run first (see "freesurf" and "bedpost").
#@args [session directory]

library(splines)
library(tractor.session)
library(tractor.nt)
library(tractor.graph)
library(tractor.native)
library(RNiftyReg)
suppressPackageStartupMessages(library(oro.nifti))

runExperiment <- function ()
{
    registerImages <- function (from, to, ...)
    {
        from <- as(from, "nifti")
        to <- as(to, "nifti")
        result <- niftyreg(from, to, ...)
        result$image <- as(result$image, "MriImage")
        invisible(result)
    }
    
    selectionFunction <- function (x, values)
    {
        dims <- dim(x)
        data <- ifelse(x %in% values, 1, 0)
        dim(data) <- dims
        return (data)
    }
    
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
	#specify lookuptable, parcellation file and T1 file for adding extra regions of interest
	lookupTableFile <- getConfigVariable("lookupTableFile", NULL, "character")  #specify extra regions to be incorporated
	parcellationFile <- getConfigVariable("parcellationFile", NULL, "character")
	T1File <- getConfigVariable("T1File", NULL, "character")

    if (file.exists(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"))){
        lookupTable <- read.table(file.path(Sys.getenv("FREESURFER_HOME"), "FreeSurferColorLUT.txt"))
    } else
        lookupTable <- read.table(file.path(Sys.getenv("TRACTOR_HOME"), "etc", "FreeSurferColorLUT.txt"))

			
	if ( !is.null(lookupTableFile) )
		lookupTable2 <- read.table(lookupTableFile, stringsAsFactors=FALSE)
	
    
    diffusionDir <- session$getDirectory("diffusion")
    diffusionRoiDir <- session$getDirectory("diffusion-rois", createIfMissing=TRUE)

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
	    customRoiDir <- session$getDirectory("customparcellation-rois", createIfMissing=TRUE)
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
	
	    nRegions2 <- length(parc_labels2)
	    regionLocations2 <- matrix(NA, nrow=nRegions2, ncol=3)
	    regionSizes2 <- numeric(nRegions2)
		allRegionNames2 <- regions2		
	} 

    regions <- tractor.graph:::.FreesurferRegionNameMapping$aparc
    allRegionNames <- paste(rep(names(regions),2), rep(c("left","right"),each=length(regions)), sep="_")
    nRegions <- length(allRegionNames)
    regionLocations <- matrix(NA, nrow=nRegions, ncol=3)
    regionSizes <- numeric(nRegions)		
	
    
    report(OL$Info, "Registering structural image to diffusion space: Freesurfer")
    refb0 <- session$getImageByType("refb0","diffusion")
    result <- registerImages(averageT1wImage, refb0, scope="affine")
    result <- registerImages(averageT1wImage, refb0, scope="nonlinear", initAffine=result$affine)
    controlPoints <- result$control[[1]]
	
	if( !is.null(T1File) ){
	    report(OL$Info, "Registering structural image to diffusion space: Specified T1 File")
	    result2 <- registerImages(averageT1wImage2, refb0, scope="affine")
	    result2 <- registerImages(averageT1wImage2, refb0, scope="nonlinear", initAffine=result$affine)
	    controlPoints2 <- result2$control[[1]]
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
            freesurferRoi <- newMriImageWithSimpleFunction(parcellation, selectionFunction, values=lookupTable[indices[[side]],1])
			mask <- newMriImageWithDataRepresentation(freesurferRoi, "coordlist")
			rm(freesurferRoi)
			
            #writeMriImageToFile(freesurferRoi, file.path(freesurferRoiDir,paste(regionName,side,sep="_")))
            currentReg <- registerImages(mask, refb0, scope="nonlinear", initControl=controlPoints, nLevels=0, finalInterpolation=1)
            transformedMaskImages[[i]] <- newMriImageWithDataRepresentation(currentReg$image, "coordlist")
            #writeMriImageToFile(result$image, file.path(diffusionRoiDir,paste(regionName,side,sep="_")))
            
            regionLocations[i,] <- apply(which(currentReg$image$getData() > 0, arr.ind=TRUE), 2, median)
            regionLocations[i,] <- transformRVoxelToWorld(regionLocations[i,], currentReg$image$getMetadata(), useOrigin=FALSE)
            regionSizes[i] <- sum(result$image$getData() > 0)
            i <- i + 1
        }
    }	

    order <- c(seq(1,nRegions,2), seq(2,nRegions,2))
    regionLocations <- regionLocations[order,]
    regionSizes <- regionSizes[order]
	
	#create sub-mask images -- custom parcellation
	maskImages2 <- list()
	for (label in parc_labels2){
		mask <- newMriImageWithData(parcellation2$getData(),parcellation2$getMetadata() )
		index <- which(as.array(mask$getData()!=0) & as.array(mask$getData())!=label )
		mask$data[index] <- 0
		mask <- newMriImageWithDataRepresentation(mask,"coordlist")    #use a sparse representation
		maskName <- file.path(customRoiDir,paste('tmp_',label,'.nii.gz', sep=''))
		maskImages2 <- c(maskImages2,mask)
		#writeMriImageToFile(mask,fileName=maskName)
	}
	
	
    report(OL$Info, "Transforming custom parcellation masks into native space...")
    transformedMaskImages2 <- lapply(maskImages2, function (image) {
        report(OL$Verbose, "Transforming \"", basename(image$getSource()), "\"")
        currentReg <- registerImages(image, refb0, scope="nonlinear", initControl=controlPoints2, nLevels=0, finalInterpolation=1)
        return (newMriImageWithDataRepresentation(currentReg$image, "coordlist"))
    })	
	rm(maskImages2)
	
	#concatenate transformed masks
	if( !is.null(parcellationFile) ){
		transformedMaskImages <- c(transformedMaskImages,transformedMaskImages2)
		allRegionNames <- c(allRegionNames,allRegionNames2)
		nRegions <- nRegions + nRegions2
		regionLocations <- rbind(regionLocations,regionLocations2)
		regionSizes <- c(regionSizes,regionSizes2)
	}
   
    report(OL$Info, "Building up a composite mask image")
    mergedMask <- array(0L, dim=dim(refb0))
    maxValues <- array(0, dim=dim(refb0))
    for (i in seq_along(transformedMaskImages))
    {
        nonzeroLocs <- transformedMaskImages[[i]]$getData()$getCoordinates()
        imageValues <- transformedMaskImages[[i]]$getData()$getData()
        locsToUpdate <- which(maxValues[nonzeroLocs] < imageValues & imageValues >= 0.2 )
        if (length(locsToUpdate) > 0)
        {
            mergedMask[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- i
            maxValues[nonzeroLocs[locsToUpdate,,drop=FALSE]] <- imageValues[locsToUpdate]
        }
        else
            report(OL$Warning, "Region \"", parc_labels[i], "\" is unrepresented in the composite mask")
		
		regionLocations[i,] <- apply(which(mergedMask==i, arr.ind=TRUE), 2, median)
		regionLocations[i,] <- transformRVoxelToWorld(regionLocations[i,], transformedMaskImages[[i]]$getMetadata(), useOrigin=FALSE)
		regionSizes[i] <- sum(as.array(transformedMaskImages[[i]]$getData()) > 0)
    }
	
    report(OL$Info, "Saving the composite mask image")
	mergedMaskMriImg <- newMriImageWithData(mergedMask,refb0$getMetadata() ) 
	mergedMaskName <- file.path(diffusionRoiDir,"mergedMask.nii.gz")
	writeMriImageToFile(mergedMaskMriImg,fileName=mergedMaskName)  #save merged masked to be used for tractography
	       
    report(OL$Info, "Performing tractography")
    fa <- session$getImageByType("FA")
    mask <- newMriImageByThresholding(fa, 0.2)
    seeds <- which(mask$getData() > 0, arr.ind=TRUE)
    seeds <- seeds + runif(length(seeds), -0.5, 0.5)
    result <- trackWithSession(session, seeds, nSamples=1, requireImage=FALSE, requireStreamlines=TRUE)
    
    report(OL$Info, "Finding streamlines passing through each region")
	matchingIndices <- list()
	for (i in seq_along(transformedMaskImages)){
        report(OL$Verbose, "Matching region \"", allRegionNames[i], "\"")
		dataM <- array( 0, dim(mergedMask) )
		indextmp <- which(mergedMask==i)
		dataM[indextmp] <- i
		region <- newMriImageWithData( dataM, mergedMaskMriImg$getMetadata()  )
        matchingIndices[[i]] <-findWaypointHits(result$streamlines, list(region))
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
	faVals <- fa$getData()
	faVals[is.nan(faVals)] <- 0
	md <- session$getImageByType("MD")
	mdVals <- md$getData()
	mdVals[is.nan(mdVals)] <- 0
    for (i in seq_along(allRegionNames))
    {
        for (j in seq_along(1:i)){
			ConStreams <- intersect(matchingIndices[[i]], matchingIndices[[j]])  #streamlines indices that connect two regions
			lenC <- length(ConStreams) 
			if( lenC==0 )
				next
            NumStreamsConMatrix[i,j] <- lenC                     
			#connectivityMatrix[i,j] <-  lenC / mean(regionSizes[c(i,j)])
			#print( NumStreamsConMatrix[i,j] )
			startInd <- result$streamlines$startIndices[ConStreams]
			endInd <- result$streamlines$getEndIndices()[ConStreams]
			LenStreamsMatrix[i,j] <- mean(endInd-startInd+1)
			pntstmp <- array(0,c(1,3))
			for (k in seq_along(ConStreams))
				pntstmp <- rbind(pntstmp, result$streamlines$points[startInd[k]:endInd[k],])
			pntstmp <- pntstmp[2:dim(pntstmp)[1],]
			pntstmp <- round(pntstmp)
			FAWConMatrix[i,j] <- sum(faVals[pntstmp])
			FAConMatrix[i,j] <- sum(faVals[unique(pntstmp,by='rows')])
			MDWConMatrix[i,j] <- sum(mdVals[pntstmp])
			MDConMatrix[i,j] <- sum(mdVals[unique(pntstmp,by='rows')])
			NumUniqueVoxs[i,j] <- dim( unique(pntstmp,by='rows') )[1]
			NumVisVoxs[i,j] <- dim(pntstmp)[1]
			
            NumStreamsConMatrix[j,i] <- NumStreamsConMatrix[i,j]          
			LenStreamsMatrix[j,i] <- LenStreamsMatrix[i,j]
			FAWConMatrix[j,i] <- FAWConMatrix[i,j]
			FAConMatrix[j,i] <- FAConMatrix[i,j]
			NumUniqueVoxs[j,i] <- NumUniqueVoxs[i,j]
			NumVisVoxs[j,i] <- NumVisVoxs[i,j]
			#connectivityMatrix[j,i] <- connectivityMatrix[i,j]
			
		}  #for (j in seq_along(allRegionNames))
	}  #for (i in seq_along(allRegionNames))
    rownames(NumStreamsConMatrix) <- allRegionNames
    colnames(NumStreamsConMatrix) <- allRegionNames
    rownames(FAConMatrix) <- allRegionNames
    colnames(FAConMatrix) <- allRegionNames
    rownames(FAWConMatrix) <- allRegionNames
    colnames(FAWConMatrix) <- allRegionNames
    rownames(MDConMatrix) <- allRegionNames
    colnames(MDConMatrix) <- allRegionNames
    rownames(MDWConMatrix) <- allRegionNames
    colnames(MDWConMatrix) <- allRegionNames
	rownames(LenStreamsMatrix) <- allRegionNames
    colnames(LenStreamsMatrix) <- allRegionNames
    rownames(NumUniqueVoxs) <- allRegionNames
    colnames(NumUniqueVoxs) <- allRegionNames
    rownames(NumVisVoxs) <- allRegionNames
    colnames(NumVisVoxs) <- allRegionNames
	
    NumStreamsConMatrix[lower.tri(connectionMatrix,diag=FALSE)] <- NA
	indEdges <- which(!is.na(NumStreamsConMatrix) & NumStreamsConMatrix != 0, arr.ind=TRUE)
    connectivityMatrix[indEdges] <- 1
    rownames(connectivityMatrix) <- allRegionNames
    colnames(connectivityMatrix) <- allRegionNames
	
	names(regionSizes) <- allRegionNames

    VoxelDims <- abs(refb0$voxelDims)  #voxel dimensions for estimating volume
	
	stepLength <- result$streamlines$summarise()$values[2]
	m <- gregexpr('.*\\s',tmp00)
	stepLength <- as.numeric(regmatches(stepLength,m))
	LenStreamsMatrix <- LenStreamsMatrix * stepLength 
	
    report(OL$Info, "Creating and writing graph")
    graph <- newGraphFromConnectionMatrix(connectivityMatrix, directed=FALSE)
	graph$setVertexAttributes( list(NumVoxelsRegion=regionSizes, VoxelDims=VoxelDims) )
	graph$setEdgeAttributes( list(FAConMatrix=FAConMatrix[indEdges],FAWConMatrix=FAWConMatrix[indEdges],MDConMatrix=MDConMatrix[indEdges],MDWConMatrix=MDWConMatrix[indEdges],LenStreamsMatrix=LenStreamsMatrix[indEdges],NumUniqueVoxs=NumUniqueVoxs[indEdges],NumVisVoxs=NumVisVoxs[indEdges]) )
    graph$setVertexLocations(regionLocations, "mm")
    graph$serialise("dgraph.Rdata")
    
    invisible(NULL)
}
