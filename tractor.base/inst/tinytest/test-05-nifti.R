reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

expect_flag <- function (object, ...) {
    object
    expect_stdout(reportr::reportFlags(), ...)
}

# NIfTI-1 read/write
path <- system.file("extdata", "nifti", "maskedb0_lia.nii.gz", package="tractor.base")
fileNames <- identifyImageFileNames(path)
image <- readImageFile(path)
unreorderedImage <- readImageFile(path, reorder=FALSE)

expect_inherits(fileNames, "list")
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
expect_true(file.size(writePath$imageFile) < file.size(path))
rereadImage <- readImageFile(writePath$fileStem)
expect_equal(image[50,59,33], rereadImage[50,59,33], tolerance=0.1)

writePath <- writeImageFile(image, tempfile(), "NIFTI_GZ")
rereadImage <- readImageFile(writePath$fileStem)
expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), check.attributes=FALSE)

# NIfTI-2 read
path <- system.file("extdata", "nifti2", "avg152T1_LR_nifti2.nii.gz", package="tractor.base")
image <- readImageFile(path)
expect_equal(image$getDimensions(), c(91,109,91))
expect_equal(image$getVoxelDimensions(), rep(2,3L))
expect_equal(image$getOrigin(), c(46,64,37))

# ANALYZE read
path <- system.file("extdata", "analyze", "maskedb0.img.gz", package="tractor.base")
expect_flag(image <- readImageFile(path), "Image orientation for ANALYZE format is inconsistently interpreted")

expect_equal(image$getDimensions(), c(96,96,60))
expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
expect_equal(image$getOrigin(), c(49,39,23))
expect_equal(image[50,59,33], 264)
expect_equal(RNifti:::orientation(image$getXform()), "LAS")
