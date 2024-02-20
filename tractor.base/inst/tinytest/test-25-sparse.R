reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

imagePath <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
boxesPath <- system.file("extdata", "nifti", "boxes.nii.gz", package="tractor.base")
image <- readImageFile(imagePath)
boxes <- readImageFile(boxesPath, sparse=TRUE)
locs <- boxes$getNonzeroIndices()

expect_true(boxes$isSparse())
expect_equal(nrow(locs), 1072)
expect_equal(1-boxes$getSparseness(), nrow(locs)/prod(dim(boxes)))
expect_equal(round(mean(image[boxes])), 476)

data <- boxes$getData()
array <- as.array(boxes)
expect_true(is(data, "SparseArray"))
expect_equal(boxes[locs], data$getData())
expect_equal(locs, data$getCoordinates())
expect_equal(data$apply(1:2,max), apply(array,1:2,max))

expect_equal(data[40,40,28], 1L)
data[40,40,28] <- 0L
expect_equal(data[40,40,28], 0L)
expect_equal(length(data$getData()), nrow(locs)-1)
data[locs] <- 0L
expect_equal(dim(data$getCoordinates()), c(0L,3L))
expect_equal(data$getData(), integer(0))
