#@args [session directory]

library(tractor.graph)

runExperiment <- function ()
{
    targetRegions <- getConfigVariable("TargetRegions", "cerebral_cortex")
    parcellationConfidence <- getConfigVariable("ParcellationConfidence", 0.2)
    graphName <- getConfigVariable("GraphName", "graph")
    zeroDiagonal <- getConfigVariable("ZeroDiagonal", TRUE)
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
        nRegions <- length(targetMatches)
        report(OL$Info, "Using #{nRegions} matched target regions")
        
        report(OL$Info, "Finding streamlines passing through each region")
        matchingIndices <- vector("list", nRegions)                     # Indices of streamlines passing through each region
        regionLocations <- matrix(NA, nrow=nRegions, ncol=3)            # Physical location of each region's spatial median, in mm
        voxelCount      <- integer(nRegions)                            # Number of voxels
        volume          <- numeric(nRegions)                            # Volume in mm^3
        for (i in seq_len(nRegions))
        {
    	    report(OL$Verbose, "Matching region \"#{targetMatches[i]}\"")
            index <- parcellation$regions$index[which(parcellation$regions$label == targetMatches[i])]
            regionImage <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x==index,1,0))
            matchingIndices[[i]] <- findWaypointHits(streamlines, list(regionImage))
    		regionLocations[i,] <- apply(regionImage$getNonzeroIndices(array=TRUE), 2, median)
    		regionLocations[i,] <- transformVoxelToWorld(regionLocations[i,], regionImage)
            voxelCount[i] <- length(regionImage$getNonzeroIndices(array=FALSE))
            volume[i] <- voxelCount[i] * prod(regionImage$getVoxelDimensions())
        }
        
        fa <- session$getImageByType("FA")
        md <- session$getImageByType("MD")
        
        report(OL$Info, "Creating connectivity matrix")
        adjacencyMatrix   <- matrix(0L, nrow=nRegions, ncol=nRegions)   # Binary adjacency matrix: 1 if any streamlines connect regions
        nStreamlines      <- numeric(0)                                 # Number of streamlines forming the connection
    	binaryFA          <- numeric(0)                                 # Mean FA, counting voxels only once
    	weightedFA        <- numeric(0)                                 # Mean FA, counting voxels each time they are visited
    	binaryMD          <- numeric(0)                                 # Mean MD, counting voxels only once
    	weightedMD        <- numeric(0)                                 # Mean MD, counting voxels each time they are visited
    	streamlineLength  <- numeric(0)                                 # Average length of streamlines connecting each pair of regions
    	uniqueVoxels      <- numeric(0)                                 # Number of unique voxels visited
    	voxelVisits       <- numeric(0)                                 # Number of voxel visits across all streamlines
    	
        for (i in seq_len(nRegions))
        {
            for (j in seq_len(ifelse(zeroDiagonal,i-1,i)))
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
        
        report(OL$Info, "Creating and writing graph")
        adjacencyMatrix[upper.tri(adjacencyMatrix)] <- adjacencyMatrix[lower.tri(adjacencyMatrix)]
        graph <- asGraph(adjacencyMatrix, directed=FALSE, allRegionNames=targetMatches, ignoreSelfConnections=zeroDiagonal)
        
        graph$setVertexAttributes(voxelCount=voxelCount, volume=volume)
        graph$setVertexLocations(regionLocations, "mm")
        graph$setEdgeAttributes(nStreamlines=nStreamlines, binaryFA=binaryFA, weightedFA=weightedFA, binaryMD=binaryMD, weightedMD=weightedMD, streamlineLength=streamlineLength, uniqueVoxels=uniqueVoxels, voxelVisits=voxelVisits)
        graph$serialise(graphName)
    }
}
