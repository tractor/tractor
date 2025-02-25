.Workspace <- new.env()
.Workspace$deserialisers <- list()
.Workspace$pathHandlers <- list()

# RGB types are handled specially
.Datatypes <- list(int8=list(   rType="integer", size=1,  signed=TRUE),
                   uint8=list(  rType="integer", size=1,  signed=FALSE),
                   int16=list(  rType="integer", size=2,  signed=TRUE),
                   uint16=list( rType="integer", size=2,  signed=FALSE),
                   int32=list(  rType="integer", size=4,  signed=TRUE),
                   uint32=list( rType="integer", size=4,  signed=FALSE),
                   int64=list(  rType="integer", size=8,  signed=TRUE),
                   uint64=list( rType="integer", size=8,  signed=FALSE),
                   float=list(  rType="double",  size=4,  signed=TRUE),
                   double=list( rType="double",  size=8,  signed=TRUE),
                   cfloat=list( rType="complex", size=8,  signed=TRUE),
                   cdouble=list(rType="complex", size=16, signed=TRUE))

.DatatypeCodes <- list(Analyze=c(uint8=2L, int16=4L, int32=8L, float=16L, cfloat=32L, double=64L, rgb=128L),
                       Nifti=c(uint8=2L, int16=4L, int32=8L, float=16L, cfloat=32L, double=64L, rgb=128L, int8=256L, uint16=512L, uint32=768L, cdouble=1792L, rgba=2304L),
                       Mgh=c(uint8=0L, int32=1L, float=3L, int16=4L))

.Dicom <- list(
    nonCharTypes=list(codes=c("OF", "FL", "FD", "SL", "SS", "UL", "US", "AT"),
                      rTypes=c(rep("double",3), rep("integer",5)),
                      sizes=c(4, 4, 8, 4, 2, 4, 2, 2),
                      counts=c(1, 1, 1, 1, 1, 1, 1, 2),
                      isSigned=c(rep(TRUE,5), rep(FALSE,3))),
    longTypes=c("OB", "OW", "OF", "SQ", "UT", "UN"),
    convertibleTypes=c("OF", "FL", "FD", "SL", "SS", "UL", "US", "AT", "DS", "IS"),
    transferSyntaxes=list("1.2.840.10008.1.2"   = list(endian="little",explicitTypes=FALSE),
                          "1.2.840.10008.1.2.1" = list(endian="little",explicitTypes=TRUE),
                          "1.2.840.10008.1.2.2" = list(endian="big",explicitTypes=TRUE)))

.FileTypes <- list(
    typeNames=c(     "ANALYZE", "NIFTI", "NIFTI_PAIR", "ANALYZE_GZ", "NIFTI_GZ", "NIFTI_PAIR_GZ", "MGH", "MGH_GZ", "MRTRIX", "MRTRIX_GZ"),
    formatNames=c(   "Analyze", "Nifti", "Nifti",      "Analyze",    "Nifti",    "Nifti",         "Mgh", "Mgh",    "Mrtrix", "Mrtrix"),
    singleFile=c(     NA,        TRUE,    FALSE,        NA,           TRUE,       FALSE,           NA,    NA,       NA,       NA),
    gzipped=c(        FALSE,     FALSE,   FALSE,        TRUE,         TRUE,       TRUE,            FALSE, TRUE,     FALSE,    TRUE),
    headerSuffixes=c("hdr",     "nii",   "hdr",        "hdr.gz",     "nii.gz",   "hdr.gz",        "mgh", "mgz",    "mif",    "mif.gz"),
    imageSuffixes=c( "img",     "nii",   "img",        "img.gz",     "nii.gz",   "img.gz",        "mgh", "mgz",    "mif",    "mif.gz"))
