context("Manipulation")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

test_that("we can manipulate images", {
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    image$setSource(NULL)
    
    expect_equal((image+image)[50,59,33], 528)
    expect_equal(max(image), 2503)
    expect_equal(round(sqrt(image)[50,59,33]), 16)
    
    expect_equal(image$summarise(), asMriImage(image$getData(),image)$summarise())
    expect_equal(dim(extractMriImage(image,3,33)), c(96,96))
    expect_equal(dim(trimMriImage(image,clearance=4)), c(64,83,60))
    expect_equal(dim(mergeMriImages(image,image)), c(96,96,120))
})
    