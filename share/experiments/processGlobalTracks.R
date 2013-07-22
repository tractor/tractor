#@args session directory, file of streamlines, parcellation file, Labels of gray matter regions
#@desc Transform raw streamlines to tractor space and extract the  relevant ones that connect: GM1->WM->GM2. If rile with white matter labels not pased then it will reconstruct streamlines GM1 -> any non gray matter label -> GM2 

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "file of streamlines", "parcellation file", "Labels of gray matter regions" )
	
	require("reportr")
	require("tractor.base")
	require("tractor.session")
	require("tractor.graph")
	require("tractor.utils")
	require("tractor.nt")
	require("RNiftyReg")
    
    session <- newSessionFromDirectory(Arguments[1])
	writeDir <- file.path(session$getDirectory(),"tractor","global_track")
	if( !file.exists(writeDir) )
		dir.create(writeDir)
	
	streamlines_file <- Arguments[2]
	if( !file.exists(streamlines_file) )
		report(OL$Error,"File with streamlines does not exist!")
	
	parcB0_file <- Arguments[3]
	if( !file.exists(parcB0_file) )
		report(OL$Error,"Parcellation file does not exist!")

	roisPermu <- Arguments[4]
	if( !file.exists(roisPermu) )
		report(OL$Error,"ROIs label file does not exist!")

	wmLabelsF <- getConfigVariable("WMLabelsF", NULL, "character")  #file with white matter labels
	debug_flag <- getConfigVariable("Debug_flag", FALSE)     #if true it will set some visualisations on (slow) 	
	suffixName <- getConfigVariable("SuffixName", NULL, "character")  #suffix to be used 
	if(debug_flag){
		report(OL$Warning,"Debug flag has been set on. It requires rgl and it is extremely slow!!!")
		require("rgl")
	}

	report( OL$Info, "session_dir: ", session$getDirectory() ) 
	report( OL$Info, "parcB0_file: ", parcB0_file )
	report( OL$Info, "roisLabel_file: ", roisPermu )
	
	if( is.null(wmLabelsF) ){
		report( OL$Warning, "White matter regions are not defined")
	} else {
		wmTable <- read.table(wmLabelsF)
		wmLabels <- as.matrix(wmTable[,1])	
	}
	
	#load the regions to be used for sorting out the extracted fibers
	permTable <- read.table(roisPermu)
	perm <- as.matrix(permTable[,1])
	perm <- sort(perm)
	numROIs <- length(perm)

	#define output file of streamlines that connect gray matter regions
	FfibersName <- paste("globalTrack","streamL",method, sep="_")
	if(is.na(suffixName)){
		writeFfibers <- file.path( writeDir, paste(FfibersName,"_",as.character(numROIs),".Rdata",sep="") )
		trackVisFileN <- file.path( writeDir, paste(FfibersName,"_",as.character(numROIs),".trk",sep="") )
	} else{
		writeFfibers <- file.path( writeDir, paste(FfibersName,"_",as.character(numROIs),"_",suffixName,".Rdata",sep="") )
		trackVisFileN <- file.path( writeDir, paste(FfibersName,"_",as.character(numROIs),"_",suffixName,".trk",sep="") )
	}
	

	#extract WM, GM from parcellation file
	parc_img <- newMriImageFromFile( parcB0_file )
	gm <- parc_img$getData() %in% perm
	gm <- array( gm, dim(parc_img$getData()) )
	if( is.null(wmLabelsF) ){  
		wm <- array( TRUE, dim(parc_img$getData()) ) #if white matter is not defined everything would be valid
	} else{
		wm <- parc_img$getData() %in% wmLabels	
		wm <- array( wm, dim(parc_img$getData()) )	
	}
	whole_brain <- parc_img$getData()
	
	#load streamlines
	load(streamlines_file) 		#this should give an object called res
			
	#transform streamlines to native space
	res$endIndex <- res$endIndex+1
	res$startIndex <- res$startIndex+1
	pntsW <- cbind(res$PntsX, res$PntsY, res$PntsZ)
	points1 <- transformWorldToVoxel(pntsW, parc_img$getMetadata() )
	points2 <- abs(points1)
		
	####################################################################################################
	#process streamlines: keep only those that travel via WM from one GM cortical regions to another#
	#need to find intermediate points in a voxel scale because the output of global tractography is based
	#on control points 
	
	#draw white matter
	if(debug_flag){
		#indexWM <- which(gm, arr.ind=TRUE)
		#points3d( indexWM[,1],indexWM[,2],indexWM[,3], color="yellow" )
	}

	numS <- length(res$endIndex)
	lenA <-res$endIndex-res$startIndex

	newIndexStart <- list()
	storePnts <- NULL
	count <- 1
	countPnts <- 1
	for( i in 1:numS ){
		ind0 <- res$startIndex[i]
		ind1 <- res$endIndex[i]
		fpnts <- points2[ind0:ind1,]
		#reconstruct fiber with one point per voxel
		numPnts <- ind1-ind0+1
		if(numPnts<2)
			next
		fpnt0 <- points2[ind0,]
		newPntsAll <- NULL
		for(j in 1:(numPnts-1)){
			fpnt1 <- points2[ind0+j,]
			distP <- dist( rbind(fpnt0,fpnt1) ) 
			if(distP==0)
				report(OL$Error,"stopped to prevent division by zero!")
			
			tmpNumPnts <- ceiling(distP)
			t_par <- seq(0,1,1.0/tmpNumPnts) #estimate line parameters x=x1+at  (here it estimates t intervals)
			sc_par <- fpnt1 - fpnt0		   #estimate line parameters x=x1+at  (here it estimates a,b,c)
			newPntsX <- fpnt0[1] + sc_par[1] * t_par
			newPntsY <- fpnt0[2] + sc_par[2] * t_par
			newPntsZ <- fpnt0[3] + sc_par[3] * t_par
			newPnts <- cbind(newPntsX,newPntsY,newPntsZ)
			#print(newPnts)
			if(j==2)
				newPntsAll <- rbind(newPntsAll,newPnts)
			else
				newPntsAll <- rbind(newPntsAll,newPnts[2:length(t_par),])
		
			fpnt0 <- fpnt1		
		}
	
		newPntsAllR <- round(newPntsAll)
		#decide what is white matter
		flagsWM <- as.numeric(wm[newPntsAllR])
		#decide what is gray matter	
		flagsGM <- gm[newPntsAllR]
		flagsWL <- whole_brain[newPntsAllR]
	
		#find and store valid fiberlines (valid: only those that GM1-WM-GM2)
		indGM <- which(flagsGM)
		if(length(indGM)<2)
			next
		newInd0 <- indGM[1]
		pnt0 <- newPntsAllR[indGM[1],]
		area0 <- whole_brain[ pnt0[1], pnt0[2], pnt0[3] ]

		for(j in 2:length(indGM) ){
			newInd1 <- indGM[j]
			pnt1 <- newPntsAllR[indGM[j],]
			area1 <- whole_brain[ pnt1[1], pnt1[2], pnt1[3] ]
			#Direct connections from one GM area to another are not allowed
			if(newInd1-newInd0==1){ #skip to next. 
				newInd0 <- newInd1
				pnt0 <- pnt1
				area0 <- area1
				next
			}
		
			#intermediate pnts should be all WM
			flags <- wm[ newPntsAllR[(newInd0+1):(newInd1-1),] ]
			if(!all(flags)){
				newInd0 <- newInd1
				pnt0 <- pnt1
				area0 <- area1
				next
			}
			
			#check if GM1 identical to GM2
			if(area0 == area1){
				newInd0 <- newInd1
				pnt0 <- pnt1
				area0 <- area1
				next
			}
		
			#finally a valid fiber segment
			newIndexStart[count] <- countPnts
			storePnts <- rbind( storePnts,newPntsAll[newInd0:newInd1,] )
		
			if(debug_flag){
				pntsLX <- newPntsAll[newInd0:newInd1,1]
				pntsLY <- newPntsAll[newInd0:newInd1,2]
				pntsLZ <- newPntsAll[newInd0:newInd1,3]	

				tmplen <- length(pntsLX)
				tmpnorm <- sqrt( diff(pntsLX)*diff(pntsLX) + diff(pntsLY)*diff(pntsLY) + diff(pntsLZ)*diff(pntsLZ))
				colorM <- rbind( diff(pntsLX), diff(pntsLY), diff(pntsLZ) ) / rbind(tmpnorm,tmpnorm,tmpnorm)
				tmpind <- which( is.nan(colorM), arr.ind=TRUE)
				colorM[tmpind] = 0
				colorM <- abs(colorM)
				lines3d( pntsLX, pntsLY, pntsLZ,col=rgb(colorM[1,],colorM[2,],colorM[3,]))
			}
			
			newInd0 <- newInd1
			pnt0 <- pnt1
			area0 <- area1
			count <- count+1	
			countPnts <- dim(storePnts)[1]+1
			
		} #for(j in 2:length(indGM) )
	
	
	} 	#for( i in 1:numS )
	report(OL$Info,"number of orignal streamlines:", numS, " number of streamlines extracted:", count)

	metadata2 <- newStreamlineTractMetadataFromImageMetadata(parc_img$getMetadata(), FALSE, "vox")	
	streamSet <- StreamlineCollectionTract$new( seedIndices=as.integer(newIndexStart),startIndices=as.integer(newIndexStart),points=as.matrix(storePnts), metadata=metadata2 )
	streamSet$serialise( writeFfibers )
    writeStreamlineCollectionTractToTrackvis(streamSet, trackVisFileN)
    
    invisible (NULL)
}
