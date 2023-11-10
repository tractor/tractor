reportr::setOutputLevel(Info)
options(reportrStderrLevel=reportr::OL$Fatal)

# Fake session path (including tractor subdirectory to reduce repetition)
fsp <- normalizePath(tempdir())
fstp <- file.path(fsp, "tractor")
dir.create(fstp, recursive=TRUE)

resolvePath <- function (path, ...) {
	path <- gsub("FSP", fsp, path, perl=TRUE)
	structure(tractor.base::resolvePath(path, ...), names=NULL)
}

wd <- getwd()
on.exit(setwd(wd))

# Unambiguous image name
expect_equal(resolvePath("FSP@fa"), file.path(fstp,"diffusion","dti_FA"))

# Indexed image name
expect_equal(resolvePath("FSP@eigenvalue",index=2), file.path(fstp,"diffusion","dti_eigval2"))

# Directory path
expect_equal(resolvePath("FSP@diffusion/"), file.path(fstp,"diffusion"))

# Ambiguous image name, requiring subdirectory to be specified
expect_equal(resolvePath("FSP@diffusion/data"), file.path(fstp,"diffusion","data"))
expect_equal(resolvePath("FSP@functional/data"), file.path(fstp,"functional","data"))

# Non-image path, relying on fallback
expect_equal(resolvePath("FSP@diffusion/directions.txt",index=2), file.path(fstp,"diffusion","directions.txt"))

# Shorthand for when the session directory is the current directory
setwd(fsp)
expect_equal(resolvePath("@MD"), file.path(fstp,"diffusion","dti_MD"))

# Using an explicit suffix
expect_equal(resolvePath("@FA.mif"), tractor.base::ensureFileSuffix(file.path(fstp,"diffusion","dti_FA"),"mif"))
