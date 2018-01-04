#@args package name
#@nohistory TRUE

runExperiment <- function ()
{
    requireArguments("package name")
    
    library(Arguments[1], character.only=TRUE)
    
    testthat::test_dir(file.path(Arguments[1],"tests","testthat"), stop_on_failure=TRUE)
}
