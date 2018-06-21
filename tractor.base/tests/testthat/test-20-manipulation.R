context("Manipulation")

reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

test_that("we can manipulate images", {
    image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
    image$setSource(NULL)
    
    image$tags <- list(key="value")
    expect_true(image$hasTags("key"))
    expect_equal(image$nTags(), 1L)
    
    expect_equal((image+image)[50,59,33], 528)
    expect_equal(max(image), 2503)
    expect_equal(round(sqrt(image)[50,59,33]), 16)
    
    expect_equal(image$summarise(), asMriImage(image$getData(),image)$summarise())
    expect_equal(dim(extractMriImage(image,3,33)), c(96,96))
    expect_equal(dim(trimMriImage(image,clearance=4)), c(64,83,60))
    expect_equal(dim(mergeMriImages(image,image)), c(96,96,120))
    
    copy <- image$copy()
    boxes <- readImageFile(system.file("extdata", "nifti", "boxes.nii.gz", package="tractor.base"), sparse=TRUE)
    copy$setOrigin(c(1,1,1))
    expect_equal(copy$getOrigin(), c(1,1,1))
    expect_equal(copy$threshold(300)[50,59,33], 0)
    expect_equal(copy[54,53,33], 372)
    expect_equal(copy$mask(boxes)[54,53,33], 0)
    expect_equal(copy[50,49,33], 422)
    expect_equal(copy$binarise()[50,49,33], 1)
    expect_equal(copy$fill(7)[50,49,33], 7)
    
    copy[boxes] <- image[boxes]
    copy[54,53,] <- 1
    expect_equal(copy[54,53,33], 1)
    expect_equal(copy[50,49,33], 422)
})
    