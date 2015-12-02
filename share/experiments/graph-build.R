#@args [session directory]
#@desc Build a graph representing interregional connectivity from the specified data source. Diffusion tractography or functional data may be used. In the former case the source is specified using the TractName option, and a corresponding streamline file generated with "xtrack" or "mtrack" must exist. Estimation of functional connectivity can be regularised by use of a shrinkage approach, which is particularly recommended when the number of time points is not appreciably larger than the number of target regions. Target regions may be specified individually or by type, using names from the parcellation's lookup table. The ParcellationConfidence variable controls the inclusiveness of the transformed parcellation: the closer to 0, the more inclusive. Self-connections are included by default.

library(tractor.reg)
library(tractor.session)
library(tractor.graph)

runExperiment <- function ()
{
    targetRegions <- getConfigVariable("TargetRegions", "cerebral_cortex")
    parcellationConfidence <- getConfigVariable("ParcellationConfidence", 0.2)
    graphName <- getConfigVariable("GraphName", "graph")
    selfConnections <- getConfigVariable("SelfConnections", TRUE)
    type <- getConfigVariable("Type", "diffusion", validValues=c("diffusion","functional"))
    tractName <- getConfigVariable("TractName", NULL, "character")
    regionTimeSeries <- getConfigVariable("RegionTimeSeries", "mean", validValues=c("mean","pc"))
    useShrinkage <- getConfigVariable("UseShrinkage", FALSE)
    varianceLambda <- getConfigVariable("VarianceShrinkageIntensity", NULL, "numeric")
    correlationLambda <- getConfigVariable("CorrelationShrinkageIntensity", NULL, "numeric")
    
    targetRegions <- splitAndConvertString(targetRegions, ",", fixed=TRUE)
    
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    parcellation <- session$getParcellation(type, threshold=parcellationConfidence)
    
    targetMatches <- matchRegions(targetRegions, parcellation, labels=TRUE)
    nRegions <- length(targetMatches)
    report(OL$Info, "Using #{nRegions} matched target regions")
    
    if (type == "diffusion")
    {
        library(tractor.nt)
        library(tractor.track)
        
        streamSource <- StreamlineSource$new(tractName)
        
        report(OL$Info, "Finding streamlines passing through each region")
        matchingIndices <- vector("list", nRegions)             # Indices of streamlines passing through each region
        regionLocations <- matrix(NA, nrow=nRegions, ncol=3)    # Physical location of each region's spatial median, in mm
        voxelCount      <- integer(nRegions)                    # Number of voxels
        volume          <- numeric(nRegions)                    # Volume in mm^3
        for (i in seq_len(nRegions))
        {
            report(OL$Verbose, "Matching region \"#{targetMatches[i]}\"")
            index <- parcellation$regions$index[which(parcellation$regions$label == targetMatches[i])]
            matchingIndices[[i]] <- streamSource$select(labels=index)$getSelection()
            regionImage <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x==index,1,0))
            regionLocations[i,] <- apply(regionImage$getNonzeroIndices(array=TRUE), 2, median)
            regionLocations[i,] <- transformVoxelToWorld(regionLocations[i,], regionImage, simple=TRUE)
            voxelCount[i] <- length(regionImage$getNonzeroIndices(array=FALSE))
            volume[i] <- voxelCount[i] * abs(prod(regionImage$getVoxelDimensions()))
        }
        
        fa <- session$getImageByType("FA", "diffusion")
        md <- session$getImageByType("MD", "diffusion")
        
        report(OL$Info, "Creating connectivity matrix")
        edgeList         <- matrix(NA, nrow=0, ncol=2)      # Edge list, one edge per row
        nStreamlines     <- numeric(0)                      # Number of streamlines forming the connection
        binaryFA         <- numeric(0)                      # Mean FA, counting voxels only once
        weightedFA       <- numeric(0)                      # Mean FA, counting voxels each time they are visited
        binaryMD         <- numeric(0)                      # Mean MD, counting voxels only once
        weightedMD       <- numeric(0)                      # Mean MD, counting voxels each time they are visited
        streamlineLength <- numeric(0)                      # Average length of streamlines connecting each pair of regions
        uniqueVoxels     <- numeric(0)                      # Number of unique voxels visited
        voxelVisits      <- numeric(0)                      # Number of voxel visits across all streamlines
        
        for (i in seq_len(nRegions))
        {
            for (j in seq_len(ifelse(selfConnections,i,i-1)))
            {
                # The subset of streamline indices connecting these two regions
                currentStreamlineIndices <- intersect(matchingIndices[[i]], matchingIndices[[j]])
                nCurrentStreamlines <- length(currentStreamlineIndices)
                if (nCurrentStreamlines == 0)
                    next
                
                edgeList <- rbind(edgeList, c(j,i))
                nStreamlines <- c(nStreamlines, nCurrentStreamlines)
                
                streamSource$select(currentStreamlineIndices)
                visitationMap <- streamSource$getVisitationMap(fa)
                visitedLocations <- visitationMap$getNonzeroIndices(positiveOnly=TRUE)
                
                binaryFA <- c(binaryFA, mean(fa[visitedLocations],na.rm=TRUE))
                weightedFA <- c(weightedFA, weighted.mean(fa[visitedLocations],visitationMap[visitedLocations],na.rm=TRUE))
                binaryMD <- c(binaryMD, mean(md[visitedLocations],na.rm=TRUE))
                weightedMD <- c(weightedMD, weighted.mean(md[visitedLocations],visitationMap[visitedLocations],na.rm=TRUE))
                streamlineLength <- c(streamlineLength, mean(streamSource$getLengths()))
                uniqueVoxels <- c(uniqueVoxels, nrow(visitedLocations))
                voxelVisits <- c(voxelVisits, sum(visitationMap[visitedLocations],na.rm=TRUE))
            }
        }
        
        report(OL$Info, "Creating and writing graph")
        graph <- asGraph(edgeList, edgeList=TRUE, directed=FALSE, selfConnections=selfConnections, allVertexNames=targetMatches)
        graph$setVertexAttributes(voxelCount=voxelCount, volume=volume)
        graph$setVertexLocations(regionLocations, "mm", paste(session$getDirectory(),"diffusion",sep=":"))
        graph$setEdgeAttributes(nStreamlines=nStreamlines, binaryFA=binaryFA, weightedFA=weightedFA, binaryMD=binaryMD, weightedMD=weightedMD, streamlineLength=streamlineLength, uniqueVoxels=uniqueVoxels, voxelVisits=voxelVisits)
        graph$serialise(graphName)
    }
    else if (type == "functional")
    {
        library(corpcor)
        
        report(OL$Info, "Reading data")
        data <- session$getImageByType("data", "functional")
        nVolumes <- dim(data)[4]
        
        report(OL$Info, "Calculating representative time series for each region")
        timeSeries      <- matrix(NA, nrow=nVolumes, ncol=nRegions)
        regionLocations <- matrix(NA, nrow=nRegions, ncol=3)    # Physical location of each region's spatial median, in mm
        voxelCount      <- integer(nRegions)                    # Number of voxels
        volume          <- numeric(nRegions)                    # Volume in mm^3
        for (i in seq_len(nRegions))
        {
            report(OL$Verbose, "Extracting region \"#{targetMatches[i]}\"")
            index <- parcellation$regions$index[which(parcellation$regions$label == targetMatches[i])]
            regionImage <- newMriImageWithSimpleFunction(parcellation$image, function(x) ifelse(x==index,1,0))
            allLocations <- regionImage$getNonzeroIndices(array=TRUE)
            regionLocations[i,] <- apply(allLocations, 2, median)
            regionLocations[i,] <- transformVoxelToWorld(regionLocations[i,], regionImage, simple=TRUE)
            voxelCount[i] <- length(regionImage$getNonzeroIndices(array=FALSE))
            volume[i] <- voxelCount[i] * abs(prod(regionImage$getVoxelDimensions()))
            
            allTimeSeries <- t(data$apply(4, "[", allLocations))
            if (regionTimeSeries == "mean")
            {
                allTimeSeries <- scale(allTimeSeries)
                timeSeries[,i] <- rowMeans(allTimeSeries, na.rm=TRUE)
                report(OL$Verbose, "The mean time series captures #{var(timeSeries[,i])/sum(diag(var(allTimeSeries)))*100}% of the variance", round=2)
            }
            else if (regionTimeSeries == "pc")
            {
                pca <- prcomp(allTimeSeries, scale.=TRUE)
                timeSeries[,i] <- pca$x[,1]
                variances <- pca$sdev^2
                report(OL$Verbose, "The first PC captures #{variances[1]/sum(variances)*100}% of the variance", round=2)
            }
        }
        
        report(OL$Info, "Calculating interregional correlations")
        if (useShrinkage)
        {
            dropNull <- function(x) Filter(Negate(is.null), x)
            covariance <- do.call("cov.shrink", dropNull(list(timeSeries,verbose=FALSE,lambda=correlationLambda,lambda.var=varianceLambda)))
            correlation <- do.call("cor.shrink", dropNull(list(timeSeries,verbose=FALSE,lambda=correlationLambda)))
            precision <- do.call("invcov.shrink", dropNull(list(timeSeries,verbose=FALSE,lambda=correlationLambda,lambda.var=varianceLambda)))
            partialCorrelation <- do.call("pcor.shrink", dropNull(list(timeSeries,verbose=FALSE,lambda=correlationLambda)))
        }
        else
        {
            covariance <- cov(timeSeries)
            correlation <- cor(timeSeries)
            precision <- pseudoinverse(covariance)
            partialCorrelation <- cor2pcor(correlation)
        }
        
        isFiniteAndNonzero <- function(x) is.finite(x) & (x != 0)
        adjacencyMatrix <- as.integer(isFiniteAndNonzero(covariance) & isFiniteAndNonzero(correlation) & isFiniteAndNonzero(precision) & isFiniteAndNonzero(partialCorrelation))
        dim(adjacencyMatrix) <- rep(nRegions, 2)
        dimnames(adjacencyMatrix) <- list(targetMatches, targetMatches)
        
        report(OL$Info, "Creating and writing graph")
        graph <- asGraph(adjacencyMatrix, directed=FALSE, selfConnections=selfConnections, allVertexNames=targetMatches)
        edges <- graph$getEdges()
        graph$setVertexAttributes(voxelCount=voxelCount, volume=volume)
        graph$setVertexLocations(regionLocations, "mm", paste(session$getDirectory(),"functional",sep=":"))
        graph$setEdgeAttributes(covariance=covariance[edges], correlation=correlation[edges], precision=precision[edges], partialCorrelation=partialCorrelation[edges])
        graph$serialise(graphName)
    }
}
