reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
altPath1 <- file.path(threadSafeTempFile(), "test1")
altPath2 <- file.path(threadSafeTempFile(), "test2")

files <- imageFiles(path)
expect_true(files$present())
info <- files$formats()[[1]]
expect_equal(info$format, "nifti_gz")
expect_equal(unname(info$headerFile), path)

expect_false(imageFiles(altPath1)$present())
files$copy(altPath1)
expect_true(imageFiles(altPath1)$present())
metadata <- readImageFile(altPath1, metadataOnly=TRUE)
expect_equal(metadata$getDimensions(), c(96,96,60))

