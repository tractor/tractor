reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)
Sys.setenv(TRACTOR_COMMANDLINE="")

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

# The CRAN version of the ore package can't handle connections, which means we
# can't read MRtrix files, so only try if TRACTOR_HOME is set (so that we're
# hopefully using TractoR's version of ore)
if (Sys.getenv("TRACTOR_HOME") != "")
{
	mrtrixPath <- system.file("extdata", "mrtrix", package="tractor.base")
	image <- readImageFile(file.path(mrtrixPath, "maskedb0.mif.gz"))
	
	expect_equal(image$getDimensions(), c(96,96,60))
	expect_equal(image$getVoxelDimensions(), rep(2.5,3L))
	expect_equal(round(image$getOrigin()), c(50,39,23))
	expect_equal(image[50,59,33], 264)
	
	image <- readImageFile(file.path(mrtrixPath, "maskedb0_lia.mif.gz"), reorder=FALSE)
	expect_equal(RNifti:::orientation(image$getXform()), "LIA")
}
