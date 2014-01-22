#@args session directory, seed point
#@desc Run global tractography using the specified session and inclusion mask.

suppressPackageStartupMessages(require(tractor.session))

runExperiment <- function ()
{
    requireArguments("session directory", "inclusion mask", "parcellation file", "ROIsPermuTable")
	
	require("reportr")
	require("tractor.global")
	require("tractor.base")
	require("tractor.session")
	require("tractor.utils")
	require("tractor.nt")
    
    session <- newSessionFromDirectory(Arguments[1])
	writeDir <- file.path(session$getDirectory(),"tractor","global_track")
	if( !file.exists(writeDir) )
		dir.create(writeDir)
	
	restrictMaskFile <- Arguments[2]
	if( !file.exists(restrictMaskFile) )
		report(OL$Error,"Mask file does not exist!")

	parcB0_file <- Arguments[3]
	if( !file.exists(parcB0_file) )
		report(OL$Error,"Parcellation file does not exist!")

	roisPermu <- Arguments[4]
	if( !file.exists(roisPermu) )
		report(OL$Error,"ROIs label file does not exist!")
	
	#load the regions to be used for sorting out the extracted fibers
	permTable <- read.table(roisPermu)
	perm <- as.matrix(permTable[,1])
	perm <- sort(perm)
	numROIs <- length(perm)
	
	report( OL$Info, "session_dir: ", session$getDirectory() ) 
	report( OL$Info, "restrictMaskFile: ", restrictMaskFile )
	report( OL$Info, "parcB0_file: ", parcB0_file )
	report( OL$Info, "roisLabel_file: ", roisPermu )
	
	debug_flag <- getConfigVariable("debug_flag", FALSE)     #if true it will set some visualisations on (slow) 
	force_track <- getConfigVariable("force_track", FALSE)  #if fiber files already exist tracking will be aborted
	
	storeStreamlines <- getConfigVariable("storeStreamlines", TRUE)
	lutPath <- getConfigVariable("lutPath", "/Users/fanideligianni/Fani/Repo1/MITK/MITK-superbuild/MITK-build/bin/")
	method <- getConfigVariable( "method", "tensorfit", validValues=c("tensorfit","bedpost") )
	
	#set default parameters of global tractography
    m_StartTemperature <- getConfigVariable("StartTemperature", 0.1)
    m_EndTemperature <- getConfigVariable("EndTemperature", 0.0001)
    m_Iterations <- getConfigVariable("Iterations", 5000000)
    m_ConnectionPotential <- getConfigVariable("ConnectionPotential", 10)
    m_InexBalance <- getConfigVariable("InexBalance", 0)
    m_ParticlePotential <- getConfigVariable("ParticlePotential", 0.2)
    m_MinFiberLength <- getConfigVariable("MinFiberLength", 10)
    m_CurvatureThreshold <- getConfigVariable("CurvatureThreshold", 0.7)
    m_flagVisualise <- getConfigVariable("VTKvisualise", FALSE)
    numOfThreads <- getConfigVariable("numOfThreads", 1)
	
	FfibersName <- paste("globalTrack","streamL",method, sep="_")
	writeFfibers <- file.path( writeDir, paste(FfibersName,"_",as.character(numROIs),".Rdata",sep="") )
	writeRawRes <- file.path(writeDir,paste("globTrack",method,"rawRes.RData", sep="_") )
	report( OL$Info,paste("Raw tracking data:", writeRawRes, sep = " ") )
	
	if(debug_flag)
		require("rgl")

	if( method=="tensorfit" && (!file.exists(writeRawRes) || force_track) ){
		FileDir <- session$getDirectory("diffusion")
		if( !file.exists(FileDir) )
			report(OL$Error,"Diffusion directory does not exist")
		
		fname_eigval1 <- file.path(FileDir,"dti_eigval1.nii.gz")
		fname_eigval2 <- file.path(FileDir,"dti_eigval2.nii.gz")
		fname_eigval3 <- file.path(FileDir,"dti_eigval3.nii.gz")
		fname_eigvec1 <- file.path(FileDir,"dti_eigvec1.nii.gz")
		fname_eigvec2 <- file.path(FileDir,"dti_eigvec2.nii.gz")
		fname_eigvec3 <- file.path(FileDir,"dti_eigvec3.nii.gz")
		
		if( !file.exists(fname_eigval1) || !file.exists(fname_eigval2) || !file.exists(fname_eigval3) || !file.exists(fname_eigvec1) || !file.exists(fname_eigvec2) || !file.exists(fname_eigvec3) )
			report(OL$Error,"Eigvalues-vecs file does not exist")
			
		res<-call_trackGlobalTensor(fname_eigval1,fname_eigval2,fname_eigval3,fname_eigvec1,fname_eigvec2,fname_eigvec3,restrictMaskFile,lutPath,
							   m_StartTemperature=m_StartTemperature, m_EndTemperature=m_EndTemperature, m_Iterations=m_Iterations,
						   	m_ConnectionPotential=m_ConnectionPotential, m_InexBalance=m_InexBalance, m_ParticlePotential=m_ParticlePotential, 
						   m_MinFiberLength=m_MinFiberLength, m_CurvatureThreshold=m_CurvatureThreshold, m_flagVisualise=m_flagVisualise, numOfThreads=numOfThreads )
		save( res,file=writeRawRes )				   
	}  # if(method=="tensorfit")
	else if( method=="bedpost" && (!file.exists(writeRawRes) || force_track) ){
		FileDir <- session$getDirectory("bedpost")
		if( !file.exists(FileDir) )
			report(OL$Error,"Bedpost directory does not exist")
		
		FileNamesPhi = list.files( FileDir, pattern="merged_ph.*samples.nii.gz", full.names=TRUE )
		FileNamesTheta = list.files( FileDir, pattern="merged_th.*samples.nii.gz", full.names=TRUE )
		
		if(length(FileNamesPhi)<1 || length(FileNamesTheta)<1)
			report(OL$Error,"Phi and Theta files do not exist!")
		
		res<-call_trackGlobalBedpost( FileNamesPhi, FileNamesTheta, restrictMaskFile, lutPath,
		   						m_StartTemperature=m_StartTemperature, m_EndTemperature=m_EndTemperature, m_Iterations=m_Iterations,
	   							m_ConnectionPotential=m_ConnectionPotential, m_InexBalance=m_InexBalance, m_ParticlePotential=m_ParticlePotential, 
	   						 	m_MinFiberLength=m_MinFiberLength, m_CurvatureThreshold=m_CurvatureThreshold, m_flagVisualise=m_flagVisualise, numOfThreads=numOfThreads )
		save( res,file=writeRawRes )	
	}  #if(method=="bedpost")
	else{
		report(OL$Warning,"Global tracking has been aborted. Set flag force_track to TRUE!")
	}	
	
	if( !exists("res",mode="list") && storeStreamlines==TRUE ){
		report(OL$Info,paste("Raw data loaded from file:",writeRawRes,sep=" "))
		load(writeRawRes)
	}
	
	if(storeStreamlines==FALSE && debug_flag==FALSE){
		report(OL$Warning,"Both storeStreamlines and debug_flag is set to FALSE. Recalculation is aborted")
		invisible (NULL)
	}
		
	
	#transform streamlines to native space
	res$endIndex <- res$endIndex+1
	res$startIndex <- res$startIndex+1
	pntsW <- cbind(res$PntsX, res$PntsY, res$PntsZ)
	metadata <- readImageFile(restrictMaskFile,metadataOnly=TRUE)
	points1 <- transformWorldToRVoxel(pntsW, metadata)
	points2 <- abs(points1)
		
	
	#extract WM, GM from parcellation file: Perhaps this part must be coded as separate function
	parc_img <- readImageFile( parcB0_file )
	wm <- array(as.logical(parc_img$getData() == 2 | +   #Left-Cerebral white matter
						   parc_img$getData() == 7 | +   #Left-Cerebellum-White-Matter
		 				   parc_img$getData() == 41 | +  #Right-Cerebral White matter
						   parc_img$getData() == 46 | +  #Right-Cerebellum-White-Matter
						   parc_img$getData() == 86 | +  #corpus callosum
						   parc_img$getData() == 77 | +  #WM-hypointensities
						   parc_img$getData() == 85 | +  #optic chiasm
						   (parc_img$getData() >= 250 & parc_img$getData() <= 255) ), 								dim=parc_img$getDimensions() )  #CC-bits
					   
	gm <- parc_img$getData() %in% perm
	gm <- array( gm, dim(parc_img$getData()) )
	
	#gm <- array(as.logical( (parc_img$getData() > 1000 & parc_img$getData() <= 1035 & parc_img$getData() != 1004) | +
	#					(parc_img$getData() > 2000 & parc_img$getData() <= 2035 & parc_img$getData() != 2004) ), dim=parc_img$getDimensions())					   
	
	whole_brain <- parc_img$getData()

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
			t_par <- seq(0,1,1/tmpNumPnts) #estimate line parameters x=x1+at  (here it estimates t intervals)
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
	print( paste("percentage of streamlines kept", (100*count)/numS, sep=":") )

	metadata2 <- newStreamlineTractMetadataFromImageMetadata(metadata, FALSE, "vox")	
	streamSet <- StreamlineCollectionTract$new( seedIndices=as.integer(newIndexStart),startIndices=as.integer(newIndexStart),points=as.matrix(storePnts), metadata=metadata2 )
	if (storeStreamlines)
	    streamSet$serialise( writeFfibers )

    
    invisible (NULL)
}
