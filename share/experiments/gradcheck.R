#@args [session directory]
#@desc Runs the standard FSL-FDT preprocessing pipeline on the specified session directory (or "." if none is specified). This pipeline consists of four stages: (1) convert DICOM files into a 4D Analyze/NIfTI/MGH volume; (2) correct the data set for eddy current induced distortions; (3) create a mask to extract only brain voxels; (4, optional) calculate diffusion tensor characteristics such as principal eigenvectors and FA values. If the pipeline was previously partly completed, the script will resume it where appropriate. (Starting from the beginning can be forced by specifying SkipCompletedStages:false.) The script asks the user about each stage unless Interactive:false is given.
#@interactive TRUE

runExperiment <- function ()
{
    session <- newSessionFromDirectory(ifelse(nArguments()==0, ".", Arguments[1]))
    
    runDtifitWithSession(session)
    
    if (interactive)
    {
        repeat
        {
            runDtifitAgain <- tolower(ask("Run dtifit again? [yn; s to show principal directions in fslview]"))
            
            if (runDtifitAgain == "n")
                break
            else if (runDtifitAgain == "s")
                runDtifitWithSession(session, showOnly=TRUE)
            else if (runDtifitAgain == "y")
            {
                ans <- ask("Flip diffusion gradient vectors along which axes? [123; Enter for none]")
                flipAxes <- splitAndConvertString(ans, "", "integer", fixed=TRUE, allowRanges=FALSE)
                
                if (length(flipAxes[!is.na(flipAxes)]) > 0)
                    flipGradientVectorsForSession(session, flipAxes)
                runDtifitWithSession(session)
            }
        }
    }
}
