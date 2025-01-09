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
            report(OL$Verbose, "Transposing gradient direction matrix")
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
    },
    
    writeToFile = function (fileName)
    {
        tractor.base:::writeGradientDirections(gradientDirections, bValues, fileName)
    }
))
