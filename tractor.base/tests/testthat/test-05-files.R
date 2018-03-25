context("Files")

reportr::setOutputLevel(Warning)

test_that("we can read and write NIfTI-1 files", {
    path <- system.file("extdata", "nifti", "maskedb0_lia.nii.gz", package="tractor.base")
    fileNames <- identifyImageFileNames(path)
    image <- readImageFile(path)
    unreorderedImage <- readImageFile(path, reorder=FALSE)
    writePath <- tempfile()
    
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
})
