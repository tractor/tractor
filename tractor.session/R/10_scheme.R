SimpleDiffusionScheme <- setRefClass("SimpleDiffusionScheme", contains="SerialisableObject", fields=list(bValues="numeric",gradientDirections="matrix"), methods=list(
    initialize = function (bValues = NULL, directions = emptyMatrix(), ...)
    {
        if (is.list(directions))
            directions <- t(Reduce(cbind, directions))
        else if (!is.matrix(directions))
            report(OL$Error, "Gradient directions must be specified in a matrix")
        
        # NB: This changed in TractoR 3.0, from column-per-direction to row-per-direction
        if (ncol(directions) != 3 && nrow(directions) == 3)
        {
            report(OL$Info, "Transposing gradient direction matrix")
            directions <- t(directions)
        }
        
        if (length(bValues) != nrow(directions))
            report(OL$Error, "Gradient matrix doesn't match the length of the b-value vector")
        
        # Normalise directions to unit length
        directions <- t(apply(directions, 1, function (x) {
            length <- vectorLength(x)
            return (x / ifelse(length==0,1,length))
        }))
        
        initFields(bValues=as.numeric(bValues), gradientDirections=unname(directions))
    },
    
    getBValues = function () { return (bValues) },
    
    getGradientDirections = function () { return (gradientDirections) },
    
    nDirections = function () { return (tapply(seq_len(nrow(gradientDirections)), list(round(bValues)), length)) },
    
    nShells = function (threshold = 100) { return (sum(unique(round(bValues)) >= threshold)) },
    
    summarise = function ()
    {
        uniqueBValues <- sort(unique(round(bValues)))
        labels <- c("Number of shells", "Diffusion b-values", "Number of directions")
        values <- c(nShells(), paste(implode(uniqueBValues,", "), "s/mm^2"), implode(nDirections()[as.character(uniqueBValues)],", "))
        return (list(labels=labels, values=values))
    }
))

newSimpleDiffusionSchemeFromSession <- function (session, unrotated = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    
    diffusionDir <- session$getDirectory("diffusion")
    
    if (unrotated)
    {
        fileName <- file.path(diffusionDir, "directions-orig.txt")
        if (!file.exists(fileName))
            fileName <- file.path(diffusionDir, "directions.txt")
    }
    else
        fileName <- file.path(diffusionDir, "directions.txt")
    
    if (file.exists(fileName))
    {
        gradientSet <- as.matrix(read.table(fileName))
        dimnames(gradientSet) <- NULL
        
        scheme <- SimpleDiffusionScheme$new(gradientSet[,4], gradientSet[,1:3])
        invisible(scheme)
    }
    else
        invisible(NULL)
}

writeSimpleDiffusionSchemeForSession <- function (session, scheme, thirdPartyOnly = FALSE, unrotated = FALSE)
{
    if (!is(session, "MriSession"))
        report(OL$Error, "Specified session is not an MriSession object")
    if (!is(scheme, "SimpleDiffusionScheme"))
        report(OL$Error, "Specified scheme is not a SimpleDiffusionScheme object")
    
    diffusionDir <- session$getDirectory("diffusion")
    fslDir <- session$getDirectory("fdt")
    
    gradientSet <- cbind(scheme$getGradientDirections(), scheme$getBValues())
    
    if (!thirdPartyOnly)
    {
        lines <- sub("\\.0+\\s*$", "", apply(format(gradientSet,scientific=FALSE),1,implode,sep="  "), perl=TRUE)
        if (unrotated)
            writeLines(lines, file.path(diffusionDir,"directions-orig.txt"))
        else
            writeLines(lines, file.path(diffusionDir,"directions.txt"))
    }
    
    if (!unrotated && file.exists(fslDir))
    {
        write.table(matrix(scheme$getBValues(),nrow=1), file.path(fslDir,"bvals"), row.names=FALSE, col.names=FALSE)
        write.table(t(scheme$getGradientDirections()), file.path(fslDir,"bvecs"), row.names=FALSE, col.names=FALSE)
    }
}
