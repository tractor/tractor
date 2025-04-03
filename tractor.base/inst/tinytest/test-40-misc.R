reportr::setOutputLevel(Warning)
options(reportrStderrLevel=reportr::OL$Fatal)

# Geometric functions
expect_equal(drop(angleBetweenVectors(c(1,0),c(0,1))), pi/2)

info <- createNeighbourhoodInfo(width=3)
expect_equal(info$dim, 3)
expect_equivalent(as.vector(table(info$vectors)), rep(27L,3))
expect_equal(range(info$innerProducts,na.rm=TRUE), c(-1,1))

# File functions
path <- system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base")
temp <- threadSafeTempFile()

expect_false(imageFileExists(temp))
copyImageFiles(path, temp)
expect_true(imageFileExists(temp))
removeImageFiles(temp)
expect_false(imageFileExists(temp))

# Windows doesn't properly support symlinking
if (tolower(Sys.info()[["sysname"]]) != "windows")
{
    symlinkImageFiles(path, temp)
    expect_true(imageFileExists(temp))
}

# String and path functions
expect_equal(implode(1:3,", "), "1, 2, 3")
expect_equal(implode(1:3,", "," and "), "1, 2 and 3")
expect_equal(implode(1:3,", ",ranges=TRUE), "1-3")

expect_equal(pluralise("test",3), "test")
expect_equal(pluralise("test",3:4), "tests")
expect_equal(pluralise("test",n=3), "tests")
expect_equal(pluralise("ox",n=3,plural="oxen"), "oxen")

# Lambda functions
image <- readImageFile(system.file("extdata", "nifti", "maskedb0.nii.gz", package="tractor.base"))
value <- image[50,59,33]

expect_equal(sapply(33, fi(image[50,59,i])), value)

image$map(fx(x^2))
expect_equal(image[50,59,33], value^2)
image$map(fxy(x+y), image)
expect_equal(image[50,59,33], 2 * (value^2))
image$map(fxyz(x*y+z), 3, 1)
expect_equal(image[50,59,33], 6 * (value^2) + 1)
