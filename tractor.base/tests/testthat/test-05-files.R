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
    expect_equal(xformToOrientation(image$getXform()), "LAS")
    
    expect_false(unreorderedImage$isReordered())
    expect_flag(unreorderedImage[50,59,33], "no consistent meaning")
    expect_equal(xformToOrientation(unreorderedImage$getXform()), "LIA")
    
    writePath <- writeImageFile(image, tempfile(), "NIFTI_GZ")
    rereadImage <- readImageFile(writePath$fileStem)
    # The next line may seem like a no-op, but getXform() gives the effective
    # xform after reordering, while setXform() sets the "stored" xform, usually
    # taken from the source file
    image$setXform(image$getXform())
    expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), tol=1e-4)
})

test_that("we can read a NIfTI-2 file", {
    path <- system.file("extdata", "nifti2", "avg152T1_LR_nifti2.nii.gz", package="tractor.base")
    image <- readImageFile(path)
    expect_equal(image$getDimensions(), c(91,109,91))
    expect_equal(image$getVoxelDimensions(), rep(2,3L))
    expect_equal(image$getOrigin(), c(46,64,37))
})

test_that("we can read and write ANALYZE files", {
    path <- system.file("extdata", "analyze", "maskedb0.img.gz", package="tractor.base")
    image <- readImageFile(path)
    
    expect_equal(image$getDimensions(), c(96,96,60))
    expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
    expect_equal(image$getOrigin(), c(49,39,23))
    expect_equal(image[50,59,33], 264)
    expect_equal(xformToOrientation(image$getXform()), "LAS")
    
    writePath <- writeImageFile(image, tempfile(), "ANALYZE")
    rereadImage <- readImageFile(writePath$fileStem)
    expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), tol=1e-4)
})

test_that("we can read and write MGH files", {
    path <- system.file("extdata", "mgh", "maskedb0.mgz", package="tractor.base")
    image <- readImageFile(path)
    
    expect_equal(image$getDimensions(), c(96,96,60))
    expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
    expect_equal(round(image$getOrigin()), c(50,39,23))
    expect_equal(image[50,59,33], 264)
    expect_equal(xformToOrientation(image$getXform()), "LAS")
    
    writePath <- writeImageFile(image, tempfile(), "MGH_GZ")
    rereadImage <- readImageFile(writePath$fileStem)
    expect_equal(image$setSource(NULL)$serialise(), rereadImage$setSource(NULL)$serialise(), tol=1e-4)
})
