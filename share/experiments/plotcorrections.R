#@desc Plot rotation angles, translations, scales or skews over the time course of an acquisition. These are obtained by reading and decomposing the transformation matrices created by the eddy current correction tool used. The "dpreproc" script must therefore have been run on the session directory (default ".") before running this experiment. A CSV file containing the correction numbers will also be produced if RequireValues:true is given.
#@args [session directory]
#@interactive TRUE

library(tractor.session)
library(tractor.reg)
library(lattice)

runExperiment <- function ()
{    
    session <- attachMriSession(ifelse(nArguments()==0, ".", Arguments[1]))
    
    mode <- getConfigVariable("Mode", "rotation", validValues=c("rotation","translation","scale","skew","all"))
    requireValues <- getConfigVariable("RequireValues", FALSE)
    interactive <- getConfigVariable("Interactive", TRUE)
    
    allValues <- NULL
    if (mode == "all")
        mode <- c("rotation","translation","scale","skew")
    nModes <- length(mode)
    
    if (!interactive)
        pdf(file="corrections.pdf", width=8, height=2.5*nModes)
    
    for (i in seq_len(nModes))
    {
        currentMode <- mode[i]
        ylab <- switch(currentMode, rotation="rotation angle, deg", translation="translation, mm", scale="scale factor", skew="skew")
        legend <- switch(currentMode, rotation=c("X (roll)","Y (pitch)","Z (yaw)"),
                                      translation=,
                                      scale=c("X (left-right)","Y (anterior-posterior)","Z (superior-inferior)"),
                                      skew=c("X-Y", "X-Z", "Y-Z"))
    
        transform <- getVolumeTransformationForSession(session, "diffusion")
        decomposition <- decomposeTransformation(transform)
        if (currentMode == "rotation")
            values <- sapply(decomposition, function(x) x$angles / pi * 180)
        else
        {
            elementName <- switch(currentMode, translation="translation", scale="scales", skew="skews")
            values <- sapply(decomposition, function(x) x[[elementName]])
        }
    
        data <- data.frame(index=seq_len(ncol(values)), x=values[1,], y=values[2,], z=values[3,])
        
        if (requireValues)
        {
            if (is.null(allValues))
                allValues <- data.frame(index=seq_len(ncol(values)), x=values[1,], y=values[2,], z=values[3,])
            else
                allValues <- cbind(allValues, data.frame(x=values[1,], y=values[2,], z=values[3,]))
            
            colnames(allValues)[(-2:0)+ncol(allValues)] <- paste(legend, currentMode)
        }
        
        print(xyplot(x + y + z ~ index, data, allow.multiple=TRUE, type="l", lwd=2, xlab="volume number", ylab=ylab, auto.key=list(space="right",text=legend,lines=TRUE,points=FALSE)), more=(i<nModes), split=c(1,i,1,nModes))
    }
    
    if (interactive)
    {
        ans <- ask("Copy figure to pdf file? [yn]")
        if (tolower(ans) == "y")
            dev.print(pdf, file="corrections.pdf")
    }
    else
        dev.off()
    
    if (requireValues)
        write.csv(allValues, "corrections.csv", row.names=FALSE)
}
