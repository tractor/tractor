#@args package name
#@nohistory TRUE

runExperiment <- function ()
{
    requireArguments("package name")
    
    library(Arguments[1], character.only=TRUE)
    
    results <- tinytest::test_all(Arguments[1])
    
    if (tinytest::any_fail(results))
    {
        print(results)
        report(OL$Error, "One or more tests failed")
    }
}
