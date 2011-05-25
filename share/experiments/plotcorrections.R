#@desc Plot rotation angles, translations, scales or skews over the time course of an acquisition. These are obtained by reading and decomposing the transformation matrices created by FSL's eddy_correct. The "preproc" script must therefore have been run, up to at least stage 2, on the session directory (default ".") before running this experiment.
#@args [session directory]
#@interactive TRUE

library(tractor.session)
library(lattice)

runExperiment <- function ()
{    
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    mode <- getConfigVariable("Mode", "rotation", validValues=c("rotation","translation","scale","skew"))
    
    ylab <- switch(mode, rotation="rotation angle, deg", translation="translation, mm", scale="scale factor", skew="skew")
    legend <- switch(mode, rotation=c("X (roll)","Y (pitch)","Z (yaw)"),
                           translation=,
                           scale=c("X (left-right)","Y (anterior-posterior)","Z (superior-inferior)"),
                           skew=c("X-Y", "X-Z", "Y-Z"))
    
    transforms <- readEddyCorrectTransformsForSession(session)
    if (mode == "rotation")
    {
        values <- sapply(transforms, function (x) decomposeAffineTransform3D(x)$angles)
        values <- values / pi * 180
    }
    else
    {
        elementName <- switch(mode, translation="translation", scale="scales", skew="skews")
        values <- sapply(transforms, function (x) decomposeAffineTransform3D(x)[[elementName]])
    }
    
    data <- data.frame(index=seq_len(ncol(values)), x=values[1,], y=values[2,], z=values[3,])
    
    print(xyplot(x + y + z ~ index, data, allow.multiple=TRUE, type="l", lwd=2, xlab="volume number", ylab=ylab, auto.key=list(space="right",text=legend,lines=TRUE,points=FALSE)))
    
    ans <- report(OL$Question, "Copy figure to pdf file? [yn]")
    if (tolower(ans) == "y")
        dev.print(pdf, file=paste(mode,"s.pdf",sep=""))
}
