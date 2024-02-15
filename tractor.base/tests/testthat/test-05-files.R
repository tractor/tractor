context("File I/O")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

expect_flag <- function (object, ...) {
    object
    expect_output(reportr::reportFlags(), ...)
}

test_that("we can read and write NIfTI-1 files", {
    path <- system.file("extdata", "nifti", "maskedb0_lia.nii.gz", package="tractor.base")
    fileNames <- identifyImageFileNames(path)
    image <- readImageFile(path)
    unreorderedImage <- readImageFile(path, reorder=FALSE)
    
    expect_type(fileNames, "list")
    expect_equal(fileNames$fileStem, ensureFileSuffix(path,NULL,strip="nii.gz"))
    
    expect_false(image$isEmpty())
    expect_true(image$getMetadata()$isEmpty())
    expect_false(image$isInternal())
    expect_true(image$isReordered())
    expect_false(image$isSparse())
    expect_equal(image$getSource(), ensureFileSuffix(path,NULL,strip="nii.gz"))
    expect_equal(dim(image), c(96,96,60))
    expect_equal(image$getDimensionality(), 3)
    expect_equal(image$getDimensions(), c(96,96,60))
    expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
    expect_equal(image$getFieldOfView(), c(240,240,150))
    expect_equal(round(image$getOrigin()), c(50,39,23))
    expect_equal(image[50,59,33], 264)
    expect_equal(image$getDataAtPoint(50,59,33), 264)
    expect_equal(RNifti:::orientation(image$getXform()), "LAS")
    
    expect_null(show(image))
    
    expect_false(unreorderedImage$isReordered())
    expect_flag(unreorderedImage[50,59,33], "no consistent meaning")
    expect_equal(RNifti:::orientation(unreorderedImage$getXform()), "LIA")
    
    writePath <- image$writeToFile(tempfile(), "NIFTI_GZ", datatype="uint8")
    expect_lt(file.size(writePath$imageFile), file.size(path))
    rereadImage <- readImageFile(writePath$fileStem)
    expect_equal(image[50,59,33], rereadImage[50,59,33], tolerance=0.1)
    
    writePath <- writeImageFile(image, tempfile(), "NIFTI_GZ")
    rereadImage <- readImageFile(writePath$fileStem)
    expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), check.attributes=FALSE)
})

test_that("we can read a NIfTI-2 file", {
    path <- system.file("extdata", "nifti2", "avg152T1_LR_nifti2.nii.gz", package="tractor.base")
    image <- readImageFile(path)
    expect_equal(image$getDimensions(), c(91,109,91))
    expect_equal(image$getVoxelDimensions(), rep(2,3L))
    expect_equal(image$getOrigin(), c(46,64,37))
})

test_that("we can read ANALYZE files", {
    path <- system.file("extdata", "analyze", "maskedb0.img.gz", package="tractor.base")
    expect_flag(image <- readImageFile(path), "Image orientation for ANALYZE format is inconsistently interpreted")
    
    expect_equal(image$getDimensions(), c(96,96,60))
    expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
    expect_equal(image$getOrigin(), c(49,39,23))
    expect_equal(image[50,59,33], 264)
    expect_equal(RNifti:::orientation(image$getXform()), "LAS")
})

test_that("we can read and write MGH files", {
    path <- system.file("extdata", "mgh", "maskedb0.mgz", package="tractor.base")
    image <- readImageFile(path)
    
    expect_equal(image$getDimensions(), c(96,96,60))
    expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
    expect_equal(round(image$getOrigin()), c(50,39,23))
    expect_equal(image[50,59,33], 264)
    expect_equal(RNifti:::orientation(image$getXform()), "LAS")
    
    writePath <- writeImageFile(image, tempfile(), "MGH_GZ")
    rereadImage <- readImageFile(writePath$fileStem)
    expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), tol=1e-4)
})

test_that("we can sort and read DICOM files", {
    path <- system.file("extdata", "dicom", package="tractor.base")
    temp <- tempdir()
    file.copy(path, temp, recursive=TRUE)
    path <- file.path(temp, "dicom")
    
    sortDicomDirectories(path, deleteOriginals=TRUE)
    expect_equal(length(list.files(path)), 2L)
    expect_true(all(c("4630_DTIb3000s5","6157_fl3D_t1_sag") %in% list.files(path)))
    
    files <- list.files(file.path(path,"6157_fl3D_t1_sag"), full.names=TRUE)
    dicom <- readDicomFile(files[1])
    expect_length(dicom$getAsciiFields("Version"), 1)
    expect_output(print(dicom), "GROUP")
    
    image <- readDicomDirectory(file.path(path,"6157_fl3D_t1_sag"))$image
    expect_equal(image$getDimensions(), c(2,224,256))
    expect_equal(image$getVoxelDimensions(), rep(1,3L))
    expect_equal(round(image$getOrigin()), c(19,95,135))
    
    reportr::setOutputLevel(Verbose)
    expect_output(image <- readDicomDirectory(file.path(path,"6157_fl3D_t1_sag"), method="divest", interactive=FALSE)$image, "Found 2 DICOM")
    expect_equal(image$getDimensions(), c(2,224,256))
    expect_equal(image$getVoxelDimensions(), rep(1,3L))
    
    unlink(path, recursive=TRUE)
})
