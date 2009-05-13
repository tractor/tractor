.First.lib <- function (libname, pkgname)
{
    # For development, the tractor.base package may not be available, so we
    # assign into .GlobalEnv
    # In deployment this should never be necessary, but it's a useful fallback
    pos <- match(c("package:tractor.base",".GlobalEnv"), search())
    pos <- pos[!is.na(pos)][1]
    
    tempAnalyze <- list(
        typeCodes=c(2,          4,          8,          16,         64),
        typesR=c(  "integer",  "integer",  "integer",  "double",   "double"),
        sizes=c(    1,          2,          4,          4,          8),
        isSigned=c( FALSE,      TRUE,       TRUE,       TRUE,       TRUE))
    
    tempDicom <- list(
        nonCharTypes=list(codes=c("OF", "FL", "FD", "SL", "SS", "UL", "US", "AT"),
                          rTypes=c(rep("double",3), rep("integer",5)),
                          sizes=c(4, 4, 8, 4, 2, 4, 2, 2),
                          counts=c(1, 1, 1, 1, 1, 1, 1, 2),
                          isSigned=c(rep(TRUE,5), rep(FALSE,3))),
        longTypes=c("OB", "OW", "OF", "SQ", "UT", "UN"),
        convertibleTypes=c("OF", "FL", "FD", "SL", "SS", "UL", "US", "AT", "DS", "IS"))
    
    tempNifti <- list(
        datatypes=list(codes=c(     2,          4,          8,          16,         64,         256,        512,        768),
                       rTypes=c(   "integer",  "integer",  "integer",  "double",   "double",   "integer",  "integer",  "integer"),
                       sizes=c(     1,          2,          4,          4,          8,          1,          2,          4),
                       isSigned=c(  FALSE,      TRUE,       TRUE,       TRUE,       TRUE,       TRUE,       FALSE,      FALSE)),
        units=list(unknown=0, m=1, mm=2, um=3),
        xformCodes=list(unknown=0, scannerAnatomical=1, alignedAnatomical=2, talairach=3, mni=4))
    
    tempFileTypes <- list(
        typeNames=c(       "ANALYZE",  "NIFTI",    "NIFTI_PAIR",   "ANALYZE_GZ",   "NIFTI_GZ", "NIFTI_PAIR_GZ"),
        formatNames=c(     "Analyze",  "Nifti",    "Nifti",        "Analyze",      "Nifti",    "Nifti"),
        singleFile=c(       NA,         TRUE,       FALSE,          NA,             TRUE,       FALSE),
        gzipped=c(          FALSE,      FALSE,      FALSE,          TRUE,           TRUE,       TRUE),
        headerSuffixes=c(  "hdr",      "nii",      "hdr",          "hdr.gz",       "nii.gz",   "hdr.gz"),
        imageSuffixes=c(   "img",      "nii",      "img",          "img.gz",       "nii.gz",   "img.gz"))
    
    tempOL <- list(Debug=1, Verbose=2, Info=3, Warning=4, Question=5, Error=6)
    
    assign(".Analyze", tempAnalyze, pos=pos)
    assign(".Dicom", tempDicom, pos=pos)
    assign(".Nifti", tempNifti, pos=pos)
    assign(".FileTypes", tempFileTypes, pos=pos)
    assign("OL", tempOL, pos=pos)
    assign(".TractorFlags", NULL, pos=pos)
    
    if (is.null(getOption("tractorFileType")))
    {
        fileType <- toupper(Sys.getenv("TRACTOR_FILETYPE"))
        if (isTRUE(fileType %in% .FileTypes$typeNames))
            options(tractorFileType=as.vector(fileType))
        else
            options(tractorFileType="NIFTI_GZ")
    }
}
