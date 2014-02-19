#@args session directory, streamlines file, parcellation file in B0 space, Table with GM regions
#@desc Converts streamline data to a graph object with attributes that represent number of streamlines, weighted FA, weighted MD, unweighted FA and MD. It assumes that streamlines are 'clean', ie. travel along white matter.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
	require("reportr")
	require("tractor.session")
	require("tractor.graph")
	require("tractor.nt")
	require("RNiftyReg")
	require("mmand")
	
	boundExtractROIs <- function (parc_img, gmRoi, wmLabels){		#use this function to extract the wm voxels that surround a given region
		gmR <- array(as.numeric(parc_img$getData() == gmRoi ), dim=parc_img$getDimensions())
		wm <- parc_img$getData() %in% wmLabels	
		wm <- array( as.numeric(wm), dim(parc_img$getData()) )	
			
		d_gmR <- dilate(gmR,shapeKernel(2,3))
		bound2<-(d_gmR-gmR)*wm
		invisible(bound2)
	}	
	
    requireArguments("session directory", "streamlines file-basename", "parcellation file in B0 space", "File with GM labels")
    
	
    session <- newSessionFromDirectory(Arguments[1])

	multipleFilesFlag <- getConfigVariable("MultipleFiles", FALSE) #streamlines split to multiple files and sorted according to which lobes they connect. In this case streamlines is a basename
	wmLabelsF <- getConfigVariable("WMLabelsF","NULL","character") #text file with labels that correspond to wm labels. To be used for extracting the white matter boundary voxels
	suffixS <- getConfigVariable("SuffixS","NULL","character")
	
	streamlines_file <- Arguments[2]
	if( !file.exists(streamlines_file) && !multipleFilesFlag )
		report(OL$Error,"File with streamlines does not exist!")
	
	parcB0_file <- Arguments[3]
	if( !file.exists(parcB0_file) )
		report(OL$Error,"Parcellation file does not exist!")

	roisPermu <- Arguments[4]
	if( !file.exists(roisPermu) )
		report(OL$Error,"ROIs label file does not exist!")
	
	writeDir <- dirname(streamlines_file)


	#load the regions to be used for sorting out the extracted fibers
	permTable <- read.table(roisPermu)
	permTable <- as.matrix(permTable)
	perm_s <- sort( as.numeric(permTable[,1]),index.return=TRUE)
	perm <- perm_s$x
	permInd <- perm_s$ix
	lobesS <- (permTable[,4])[permInd]   #find the label of the lobe
	namesR <- (permTable[,2])[permInd]   #sort regions' names	
	
	if(!is.na(wmLabelsF))
		wmLabels <- as.matrix(read.table(wmLabelsF))

	lobesU <- unique(lobesS)
	areasPerLobe <- list()
	indexPerLobe <- list()
	for(i in seq(lobesU)){
		indLobe <- which(lobesS==lobesU[i])
		areasPerLobe[[i]] <- perm[indLobe]
		indexPerLobe[[i]] <- indLobe
	}
	
	#output filenames
	nRegions <- length(perm)
	fileNameCon <- file.path(writeDir,paste("graphObj_",as.character(nRegions),".Rdata",sep="") )

	#load free surfer segmentation
	parc_img <- readImageFile( parcB0_file )
	parc <- parc_img$getData()
	gm <- parc %in% perm
	gm <- array(gm,dim(parc))
	
	#load fa and md maps
	fa <- session$getImageByType("FA")
	md <- session$getImageByType("MD")

	if(!multipleFilesFlag){
		report(OL$Info,"Reading streamlines:", streamlines_file)
		streamL <- deserialiseReferenceObject( file=streamlines_file )
		stPntsInd <- streamL$getStartIndices()
		endPntsInd <- streamL$getEndIndices()
		stPnts <- streamL$points[ stPntsInd, ]
		endPnts <- streamL$points[ endPntsInd, ] 
		stPntsGM <- parc[round(stPnts)]
		endPntsGM <- parc[round(endPnts)]
		
		indLabels <- seq_along(perm)    #use this index later to recover the position in the connectivity matrix 
		matchingIndices <- list()
		count <- 1
		for(area1 in perm){
			matchingIndices[[count]] <- which( ((stPntsGM %in% area1) | (endPntsGM %in% area1)) & stPntsGM!=endPntsGM ) 
			count <- count+1
		}
		
	}

    report(OL$Info, "Measuring number of streamlines, FA and MD along connections...")
    NumStreamsConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)
	FAConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)      #sums fa values in voxels only once
	FAWConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)		#sums fa values in voxels each time they are visited
	MDConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)      #sums md values in voxels only once
	MDWConMatrix <- matrix(0, nrow=nRegions, ncol=nRegions)		#sums md values in voxels each time they are visited
	LenStreamsMatrix <- matrix(0, nrow=nRegions, ncol=nRegions) #average length of streamlines connecting each pair of regions
	NumUniqueVoxs <- matrix(0, nrow=nRegions, ncol=nRegions)    #number of voxels that have been visited (Voxels only counted once)
	NumVisVoxs <- matrix(0, nrow=nRegions, ncol=nRegions)		#number of visits across all voxels of the tracks

	for(i in 1:length(lobesU)){
		lobeA <- lobesU[i]
		areasInLobeA <- areasPerLobe[[i]]
		indInLobeA <- indexPerLobe[[i]]
		for(j in 1:i){ 			#deal with connections between lobes i and j
			lobeB <- lobesU[j]
			areasInLobeB <- areasPerLobe[[j]]
			indInLobeB <- indexPerLobe[[j]]
			if(multipleFilesFlag){
				#load corresponding fiber set
				fiberName <- paste(streamlines_file, lobeA, "2", lobeB,".Rdata", sep="")
				if( !file.exists(fiberName) )
					fiberName <- paste(streamlines_file, lobeB, "2", lobeA,".Rdata", sep="")
				if( !file.exists(fiberName) ){
					report(OL$Warning,"Streamline file between lobes:", lobeA, " and ", lobeB, " does not exist! Skip to next!")
					next
				}
				report(OL$Info,"Reading streamlines:", fiberName)
				streamL <- deserialiseReferenceObject( file=fiberName )
				stPntsInd <- streamL$getStartIndices()
				endPntsInd <- streamL$getEndIndices()
				stPnts <- streamL$points[ stPntsInd, ]
				endPnts <- streamL$points[ endPntsInd, ] 
				stPntsGM <- parc[round(stPnts)]
				endPntsGM <- parc[round(endPnts)]		

				corLabels <- union(areasInLobeA,areasInLobeB)
				indLabels <- union(indInLobeA,indInLobeB)		 #use this index later to recover the position in the connectivity matrix 
				matchingIndices <- list()
				length(matchingIndices) <- length(perm)
				for(area1 in seq_along(corLabels) )
					matchingIndices[[indLabels[area1]]] <- which( ((stPntsGM %in% corLabels[area1]) | (endPntsGM %in% corLabels[area1])) & stPntsGM!=endPntsGM ) 

			}	#if(multipleFilesFlag)
			
			for(area1 in seq_along(areasInLobeA) ){
				indtmp1 <- indInLobeA[area1]
				for(area2 in seq_along(areasInLobeB) ){
					indtmp2 <- indInLobeB[area2]
					if(areasInLobeA[area1]==areasInLobeB[area2])
						next
					
					ConStreams <- intersect(matchingIndices[[indtmp1]], matchingIndices[[indtmp2]])  #streamlines indices that connect two regions
					lenC <- length(ConStreams) 
					if( lenC==0 )
						next
		            NumStreamsConMatrix[indtmp1,indtmp2] <- lenC 
					newTrack <- newStreamlineCollectionTractBySubsetting(streamL,ConStreams)
					startInd <- newTrack$getStartIndices()
					endInd <- newTrack$getEndIndices()
					LenStreamsMatrix[indtmp1,indtmp2] <- mean(endInd-startInd+1)
					visitationMap <- newMriImageAsVisitationMap( newTrack,fa$getMetadata() )
					indVisVox <- which( visitationMap$getData()!=0 & !is.na(visitationMap$getData()) & !is.na(fa$getData())  )
					FAWConMatrix[indtmp1,indtmp2] <- sum( fa$getData()[indVisVox] * visitationMap$getData()[indVisVox] )
					FAConMatrix[indtmp1,indtmp2] <- sum( fa$getData()[indVisVox] )
					MDWConMatrix[indtmp1,indtmp2] <- sum( md$getData()[indVisVox] * visitationMap$getData()[indVisVox] )
					MDConMatrix[indtmp1,indtmp2] <- sum( md$getData()[indVisVox] )
					NumUniqueVoxs[indtmp1,indtmp2] <- length(indVisVox)
					NumVisVoxs[indtmp1,indtmp2] <- sum( visitationMap$getData()[indVisVox] )
					
				}  #for(area2 in areasInLobeB)
			}  #for(area1 in areasInLobeA)
			
			
		}	#for(j in 1:i)
	}	#for(i in 1:length(lobesU))
	
	#symmetrise square matrices
	indtmp <- which( NumStreamsConMatrix!=0, arr.ind=TRUE )
	NumStreamsConMatrix[indtmp[,c(2,1)]] <- NumStreamsConMatrix[indtmp]
	LenStreamsMatrix[indtmp[,c(2,1)]] <- LenStreamsMatrix[indtmp]
	FAWConMatrix[indtmp[,c(2,1)]] <- FAWConMatrix[indtmp]
	FAConMatrix[indtmp[,c(2,1)]] <- FAConMatrix[indtmp]
	MDWConMatrix[indtmp[,c(2,1)]] <- MDWConMatrix[indtmp]
	MDConMatrix[indtmp[,c(2,1)]] <- MDConMatrix[indtmp]
	NumUniqueVoxs[indtmp[,c(2,1)]] <- NumUniqueVoxs[indtmp]
	NumVisVoxs[indtmp[,c(2,1)]] <- NumVisVoxs[indtmp]
	
	#create binary connectivity matrix 
    NumStreamsConMatrix[lower.tri(NumStreamsConMatrix,diag=FALSE)] <- NA     #note lower.tri must be compatible with the initialisation of the graph object
	diag(NumStreamsConMatrix) <- NA
	indEdges <- which(!is.na(NumStreamsConMatrix) & NumStreamsConMatrix != 0, arr.ind=TRUE)
	connectivityMatrix <- matrix(0, nrow=nRegions, ncol=nRegions) 
	connectivityMatrix[indEdges] <- 1
    rownames(connectivityMatrix) <- namesR
    colnames(connectivityMatrix) <- namesR

    VoxelDims <- abs(parc_img$voxelDims)  #voxel dimensions for estimating volume
	
	stepLength <- streamL$summarise()$values[2]
	m <- gregexpr('.*\\s',stepLength)
	stepLength <- as.numeric(regmatches(stepLength,m))
	LenStreamsMatrix <- LenStreamsMatrix * stepLength 
	
	#count number of voxels per region and number of white matter voxels that surround a region
	numVoxPerRoi <- list()	
	length(numVoxPerRoi) <- nRegions
	numWMVoxPerRoi <- list()
	length(numWMVoxPerRoi) <- nRegions
	regionLocations <- matrix(NA, nrow=nRegions, ncol=3)
	rownames(regionLocations) <- namesR
	names(numVoxPerRoi) <- namesR
	names(numWMVoxPerRoi) <- namesR
	for( i in 1:nRegions ){
		gm <- array( parc == perm[i], dim=parc_img$getDimensions() )
		regionLocations[i,] <- apply(which(parc==perm[i], arr.ind=TRUE), 2, median)
		regionLocations[i,] <- transformVoxelToWorld(regionLocations[i,], parc_img$getMetadata() )
		
		numVoxPerRoi[i] <- length(which(gm))
		if(!is.na(wmLabelsF)){
			boundVoxs <- boundExtractROIs(parc_img,perm[i],as.numeric(wmLabels[,1])) 
			numWMVoxPerRoi[i] <- length( which(boundVoxs!=0))
		}
	}
	
	
    report(OL$Info, "Creating and writing graph")
    graph <- newGraphFromConnectionMatrix(connectivityMatrix, directed=FALSE, selfConnections=FALSE)
	graph$setVertexAttributes( c(graph$vertexAttributes,list(numVoxPerRoi=as.numeric(numVoxPerRoi), numWMVoxPerRoi=as.numeric(numWMVoxPerRoi), VoxelDims=VoxelDims)) )
	graph$setEdgeAttributes( list(FAConMatrix=FAConMatrix[indEdges],FAWConMatrix=FAWConMatrix[indEdges],MDConMatrix=MDConMatrix[indEdges],MDWConMatrix=MDWConMatrix[indEdges],LenStreamsMatrix=LenStreamsMatrix[indEdges],NumUniqueVoxs=NumUniqueVoxs[indEdges],NumVisVoxs=NumVisVoxs[indEdges], NumStreamsConMatrix=NumStreamsConMatrix[indEdges]) )
    graph$setVertexLocations(regionLocations, "mm")
    graph$serialise( file.path(writeDir,paste("dgraph_",suffixS,".Rdata",sep="")) )
    
    invisible(graph)

}
