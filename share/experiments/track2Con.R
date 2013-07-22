#@args sessiondirectory
#@desc Converts streamline data to connectivity matrices

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "ROIsPermuTable")
    
	require("reportr")
	require("tractor.session")
	require("tractor.nt")
	require("mmand")
	
    session <- newSessionFromDirectory(Arguments[1])
	roisPermu <- getConfigVariable("ROIsPermuTable",Arguments[2])
	save_flag <- getConfigVariable("save_flag", TRUE)
	force_con <- getConfigVariable("force_con", FALSE)  
    trackDirType <- getConfigVariable("TrackDirType", "probtrack", validValues=c("probtrack","global") )
	searchPattern <- getConfigVariable("searchPattern", ".*.Rdata")
	flagSaveTrack <- getConfigVariable("flagSaveTrack", FALSE)  #it requires loading the extract track. It can be very memory consuming
	
	print( paste("searchPattern:",searchPattern,sep=" ") )
	print( paste("trackDirType:",trackDirType,sep=" ") )
	print( paste("force_con:",force_con,sep=" ") )	
		
	if( trackDirType == "probtrack"){
		trackDir <- session$getDirectory(type="probtrack")
	}else if( trackDirType == "global" ){
		trackDir <- file.path(session$getDirectory(),"tractor","global_track")
	}

	#load the regions to be used for sorting out the extracted fibers
	permTable <- read.table(roisPermu)
	permTable <- as.matrix(permTable)
	perm_s <- sort( as.numeric(permTable[,1]),index.return=TRUE)
	perm <- perm_s$x
	permInd <- perm_s$ix
	lobesS <- (permTable[,4])[permInd]   #find the label of the lobe
	lobesU <- unique(lobesS)
	areasLobe <- list()
	for(i in seq(lobesU)){
		indLobe <- which(lobesS==lobesU[i])
		areasLobe[[i]] <- perm[indLobe]
	}
	
	#output filenames
	numROIs <- length(perm)
	fileNameCon <- file.path(trackDir,paste("conMat_",as.character(numROIs),".txt",sep="") )
	
	ConExFlag <- file.exists(fileNameCon)
	#The only case estimation is aborted is when both files exist and force_con is FALSE
	if(ConExFlag & !force_con){
		report(OL$Warning, "Estimation of connectivity matrix is aborted!")
		return( invisible(NULL) )
	}
					
	#load free surfer segmentation
	parcB0_file <- file.path( session_dir, "tractor", "T12B0", "aparc+aseg2B0.nii.gz" )
	parc_img <- newMriImageFromFile( parcB0_file )
	parc <- parc_img$getData()
	gm <- parc %in% perm
	gm <- array(gm,dim(parc))
	gmpnts <- which(gm,arr.ind = T)

	#load and process streamlines
	fileNames <- list.files(trackDir,pattern=searchPattern)
	stPntsTot <- list()
	endPntsTot <- list()
	totNumSL <- 0	
	for(i in 1:length(fileNames)) {
		print( paste("Read streamline file:",file.path(trackDir,fileNames[i]), sep=" " ) )
		streamL <- deserialiseReferenceObject(file=file.path(trackDir,fileNames[i]) )
		numSL <- streamL$nStreamlines()
		totNumSL <- totNumSL+numSL
		stPntsInd <- streamL$getStartIndices()
		endPntsInd <- streamL$getEndIndices()
		stPnts <- streamL$points[ stPntsInd, ]
		endPnts <- streamL$points[ endPntsInd, ] 
		stPntsGM <- parc[round(stPnts)]
		endPntsGM <- parc[round(endPnts)]
		#use streamlines that start and end in gray-matter
		indP <- which( (stPntsGM %in% perm) & (endPntsGM %in% perm) & stPntsGM!=endPntsGM )
		
		#merge relevant streamlines to one object
		if(flagSaveTrack){
			newTrack <- newStreamlineCollectionTractBySubsetting(streamL,indP)
			if(i==1)
				totTrack <- newTrack$copy()
			else
				totTrack <- newStreamlineCollectionTractByMerging(totTrack,newTrack)
		}

		#store start and end labels for further processing
		stPntsTot <- c( stPntsTot, stPntsGM[indP] )
		endPntsTot <- c( endPntsTot, endPntsGM[indP] )
	}
	totStrmsLab <- cbind(as.numeric(stPntsTot),as.numeric(endPntsTot))
	report( OL$Info, "Num of total streamlines versus Num of selected:", totNumSL, " ",length(stPntsTot) )
			
	#free some memory
	rm(stPntsTot,endPntsTot,stPntsInd,endPntsInd,stPntsGM,endPntsGM,indP,streamL,newTrack)
		
	#save merged track
	if(flagSaveTrack){
		maskPath <- file.path(trackDir,"track_masks")
		if(!file.exists(maskPath))
			dir.create(maskPath)
		#save in streamline-tractor format
		newPattern <- gsub(".Rdata","",searchPattern)
		newPattern <- gsub("[.*]","",newPattern)
		fileNBase <- paste("TotSL",as.character(numROIs),newPattern,sep="_") 
		report( OL$Info,"Saving track to Rdata format...")
		totTrack$serialise( file.path(maskPath, fileNBase ) )
		#save in trackvis format
		report( OL$Info,"Saving track to trackvis format...")
		writeStreamlineCollectionTractToTrackvis(totTrack, file.path(maskPath, paste(fileNBase,".trk",sep="") ) )
		#export a mask of each region seperately for use in trackvis
		for(r in seq(perm)){
			roi <- array(0,dim(parc))
			ind <- which(parc==perm[r])
			roi[ind] <- 1
			newRoiImg <- newMriImageWithData(roi,templateImage=parc_img)
			writeMriImageToFile(newRoiImg,fileName=file.path(maskPath, paste("mask_",as.character(perm[r]),".nii.gz",sep="")),fileType="NIFTI_GZ")
		}
	}

	#sort out array by cols first and then by rows
	totStrmsLab <- t(apply(totStrmsLab,1,sort)) 
	totStrmsLab <- totStrmsLab[do.call(order,lapply(1:NCOL(totStrmsLab),function(i) totStrmsLab[,i])), ]
	
	#find number of streamlines per pair of regions
	tmpD <- diff(totStrmsLab)
	indR <- which(tmpD[,1] | tmpD[,2] )
	indR <- c(indR,dim(totStrmsLab)[1] )
	numStrms <- c(indR[1],diff(indR)) 
	rowsUn <- unique(totStrmsLab)
	
	#construct connectivity matrix
	roisL <- unique( as.vector(rowsUn) ) #freesurfer regions' labels
	roisS <- list()	#number of voxels per region
	lenR <- length(roisL)
	for( i in 1:length(roisL) ){
		gm <- array(as.numeric(parc == roisL[i]), dim=parc_img$getDimensions() )
		boundVoxs <- boundExtractROIs(parc_img,roisL[i])  #this function is located at my_funs 
		roisS[i] <- length( which(boundVoxs!=0) )
	}
	roisS <- as.numeric(roisS)    #number of voxels per region: roisL 
	
	if( !is.null(roisPermu) ){
		lenPerm <- length(perm)
		conM <- array(0, c(lenPerm,lenPerm) )	
	}else{
		conM <- array(0, c(lenR,lenR) )
		perm <- sort(roisL)
	}
	
	
	for(i in 1:dim(rowsUn)[1]){
		r1 <- rowsUn[i,1]
		r2 <- rowsUn[i,2]
		ind1 <- which(roisL==r1)
		ind2 <- which(roisL==r2)
		indC1 <- which(perm==r1)
		indC2 <- which(perm==r2)
		conM[indC1,indC2] = 2.0*numStrms[i] / ( roisS[ind1]+roisS[ind2] )
		conM[indC2,indC1] = conM[indC1,indC2]
	}
	
	rownames(conM) <- paste("R",perm,sep="")
	colnames(conM) <- paste("C",perm,sep="")
	
	if(save_flag){
		#roisInfo <- perm
		#saveFileI <- file.path(trackDir, "roisInfo.txt")
		#write.table( roisInfo, file=saveFileI, append=FALSE, row.names=FALSE, col.names=FALSE )
		saveFileC <- file.path( trackDir, paste("conMat_",as.character(numROIs),".txt",sep="") )
		print( paste("Write connectivity matrix file:",saveFileC, sep=" " ) )
		write.table( conM, file=saveFileC, append=FALSE )
	}

	invisible(conM)
}
