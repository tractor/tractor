reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

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
