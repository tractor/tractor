#@desc Plot rotations about the X, Y and Z axes over the time course of an acquisition. These are obtained by reading and decomposing the transformation matrices created by FSL's eddy_correct. The "preproc" script must therefore have been run, up to at least stage 2, on the session directory (default ".") before running this experiment.
#@args [session directory]
#@interactive TRUE

library(tractor.session)
library(lattice)

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    transforms <- readEddyCorrectTransformsForSession(session)
    angles <- sapply(transforms, function (x) decomposeAffineTransform3D(x)$angles)
    angles <- angles / pi * 180
    data <- data.frame(index=seq_len(ncol(angles)), x=angles[1,], y=angles[2,], z=angles[3,])
    
    print(xyplot(x + y + z ~ index, data, allow.multiple=TRUE, type="l", lwd=2, xlab="volume number", ylab="rotation angle, deg", auto.key=list(space="right",text=c("X (roll)","Y (pitch)","Z (yaw)"),lines=TRUE,points=FALSE)))
    
    ans <- output(OL$Question, "Copy figure to pdf file? [yn]")
    if (tolower(ans) == "y")
        dev.print(pdf, file="rotations.pdf")
}
