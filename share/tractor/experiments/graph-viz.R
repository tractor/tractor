#@desc Visualise the specified graph, either topologically or as an association matrix. The graph can be shown overlaid on the brain if the vertex location space is stored with the graph, and a mask exists in that space.
#@args graph file
#@group Graph and network analysis
#@interactive TRUE
#@nohistory TRUE

library(tractor.graph)

runExperiment <- function ()
{
    requireArguments("graph file")
    
    matrixView <- getConfigVariable("MatrixView", FALSE)
    useAbsoluteWeights <- getConfigVariable("AbsoluteWeights", FALSE)
    weightLimits <- getConfigVariable("WeightLimits", NULL, "character")
    hideDisconnected <- getConfigVariable("HideDisconnected", NULL, "logical")
    useNames <- getConfigVariable("UseNames", FALSE)
    useLocations <- getConfigVariable("UseLocations", TRUE)
    showBrain <- getConfigVariable("ShowBrain", FALSE)
    
    if (is.null(hideDisconnected))
        hideDisconnected <- !matrixView
    
    if (!is.null(weightLimits))
    {
        weightLimits <- splitAndConvertString(weightLimits, ",", "numeric", fixed=TRUE, errorIfInvalid=TRUE)
        if (length(weightLimits) != 2)
            report(OL$Error, "Weight limits must be specified as a 2-vector giving the low and high limits")
    }
    
    graphName <- implode(Arguments, " ")
    graph <- readGraphFile(graphName)
    
    dev.new()
    
    if (matrixView)
    {
        library(lattice)
        print(levelplot(graph, useAbsoluteWeights=useAbsoluteWeights, weightLimits=weightLimits, ignoreBeyondLimits=FALSE, hideDisconnected=hideDisconnected, useNames=useNames))
    }
    else if (useLocations)
    {
        if (showBrain)
        {
            library(tractor.reg)
            library(mmand)
            library(tractor.session)
            space <- unlist(strsplit(graph$getVertexLocationSpace(), ":", fixed=TRUE))
            if (length(space) != 2)
                report(OL$Error, "Cannot resolve the space of the vertex locations")
            
            brainMask <- attachMriSession(space[1])$getImageByType("mask", space[2])
            kernel <- shapeKernel(c(3,3,3), type="box")
            brainMask$map(function(x) x - erode(x,kernel), sparse=TRUE)
            
            edgePoints <- brainMask$getNonzeroIndices()
            if (graph$getVertexLocationUnit() == "mm")
                edgePoints <- RNifti::voxelToWorld(edgePoints, brainMask, simple=TRUE)
            fieldOfView <- brainMask$getFieldOfView()
        }
        
        par(mar=c(0,0,0,0))
        layout(matrix(c(1:3,0), nrow=2, ncol=2))
        for (i in 1:3)
        {
            axes <- setdiff(1:3, i)
            if (showBrain)
            {
                alpha <- edgePoints[,i] / max(edgePoints[,i]) * 0.05
                asp <- ifelse(graph$getVertexLocationUnit() == "mm", 1, fieldOfView[axes[2]]/fieldOfView[axes[1]])
                plot(edgePoints[,axes[1]], edgePoints[,axes[2]], pch=19, cex=0.8, asp=asp, col=rgb(0,0,0,alpha), type="p", axes=FALSE)
            }
            plot(graph, useAbsoluteWeights=useAbsoluteWeights, weightLimits=weightLimits, ignoreBeyondLimits=FALSE, useAlpha=TRUE, hideDisconnected=hideDisconnected, useNames=useNames, useLocations=TRUE, locationAxes=axes, add=showBrain)
        }
    }
    else
        plot(graph, useAbsoluteWeights=useAbsoluteWeights, weightLimits=weightLimits, ignoreBeyondLimits=FALSE, useAlpha=TRUE, hideDisconnected=hideDisconnected, useNames=useNames, useLocations=FALSE)
    
    if (ask("Copy figure to pdf file? [yn]", valid=c("y","n")) == "y")
        dev.print(pdf, file=ensureFileSuffix(basename(graphName),"pdf",strip=c("Rdata","csv")))
    
    invisible(NULL)
}
