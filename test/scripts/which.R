#@args image file, operator, value

runExperiment <- function ()
{
    requireArguments("image file", "operator", "value")
    
    useArrayIndices <- getWithDefault("ArrayIndices", TRUE)
    
    image <- newMriImageFromFile(Arguments[1])
    operator <- get(switch(Arguments[2], eq="==", gt=">", lt="<", ge=">=", le="<="))
    value <- as.numeric(Arguments[3])
    
    print(which(operator(image$getData(), value), arr.ind=useArrayIndices))
}
