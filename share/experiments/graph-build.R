#@args [session directory]

library(tractor.graph)

runExperiment <- function ()
{
    targetRegions <- getConfigVariable("TargetRegions", "cerebral_cortex")
    parcellationConfidence <- getConfigVariable("ParcellationConfidence", 0.2)
    graphName <- getConfigVariable("GraphName", "graph")
    tractName <- getConfigVariable("TractName", NULL, "character")
    
    if (!is.null(tractName))
    {
        library(tractor.reg)
        library(tractor.session)
        library(tractor.nt)
        library(tractor.track)
        
        session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
        parcellation <- session$getParcellation("diffusion", threshold=parcellationConfidence)
        streamlines <- deserialiseReferenceObject(paste(tractName,"streamlines",sep="_"))
        
        targetMatches <- matchRegions(targetRegions, parcellation, labels=TRUE)
        report(OL$Info, "Using #{length(targetMatches)} matched target regions")
        
        report(OL$Info, "Finding streamlines passing through each region")
    	matchingIndices <- lapply(targetMatches, function(label) {
    	    report(OL$Verbose, "Matching region \"#{label}\"")
            index <- parcellation$regions$index[which(parcellation$regions$label == label)]
            regionImage <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x==index,1,0))
            return (findWaypointHits(streamlines, list(regionImage)))
    	})
        
        fa <- session$getImageByType("FA")
        md <- session$getImageByType("MD")
        
        report(OL$Info, "Creating connectivity matrix")
        nRegions <- length(targetMatches)
        adjacencyMatrix   <- matrix(0L, nrow=nRegions, ncol=nRegions)   # Binary adjacency matrix: 1 if any streamlines connect regions
        nStreamlines      <- numeric(0)                                 # Number of streamlines forming the connection
    	binaryFA          <- numeric(0)                                 # Mean FA, counting voxels only once
    	weightedFA        <- numeric(0)                                 # Mean FA, counting voxels each time they are visited
    	binaryMD          <- numeric(0)                                 # As above for MD
    	weightedMD        <- numeric(0)                             
    	streamlineLength  <- numeric(0)                                 # Average length of streamlines connecting each pair of regions
    	uniqueVoxels      <- numeric(0)                                 # Number of unique voxels visited
    	voxelVisits       <- numeric(0)                                 # Number of voxel visits across all streamlines
    	
        for (i in seq_len(nRegions))
        {
            for (j in seq_len(i-1))
            {
                # The subset of streamline indices connecting these two regions
    			currentStreamlineIndices <- intersect(matchingIndices[[i]], matchingIndices[[j]])
    			nCurrentStreamlines <- length(currentStreamlineIndices)
    			if (nCurrentStreamlines == 0)
    				next
                
                adjacencyMatrix[i,j] <- 1
                nStreamlines <- c(nStreamlines, nCurrentStreamlines)
                
                currentStreamlines <- newStreamlineCollectionTractBySubsetting(streamlines, currentStreamlineIndices)
                visitationMap <- newMriImageAsVisitationMap(currentStreamlines)
                visitedLocations <- visitationMap$getNonzeroIndices(positiveOnly=TRUE)
                
                binaryFA <- c(binaryFA, mean(fa[visitedLocations],na.rm=TRUE))
                weightedFA <- c(weightedFA, weighted.mean(fa[visitedLocations],visitationMap[visitedLocations],na.rm=TRUE))
                binaryMD <- c(binaryMD, mean(md[visitedLocations],na.rm=TRUE))
                weightedMD <- c(weightedMD, weighted.mean(md[visitedLocations],visitationMap[visitedLocations],na.rm=TRUE))
                streamlineLength <- c(streamlineLength, mean(currentStreamlines$getStreamlineLengths()))
                uniqueVoxels <- c(uniqueVoxels, nrow(visitedLocations))
                voxelVisits <- c(voxelVisits, sum(visitationMap[visitedLocations],na.rm=TRUE))
            }
        }
        
        adjacencyMatrix[upper.tri(adjacencyMatrix)] <- adjacencyMatrix[lower.tri(adjacencyMatrix)]
        
        graph <- asGraph(adjacencyMatrix, allRegionNames=targetMatches)
        
	
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
    }
}
