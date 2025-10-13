#@desc Identify connected components in an image (i.e., regions of contiguously nonzero voxels), and create a new image replacing each voxel value with its component number. Component numbers will be sorted by size if the SortBySize option is true. Diagonal neighbours are only considered if IncludeDiagonal is true.
#@args image file
#@group Image processing

library(mmand)

runExperiment <- function ()
{
    includeDiagonal <- getConfigVariable("IncludeDiagonal", TRUE)
    sortBySize <- getConfigVariable("SortBySize", FALSE)
    
    requireArguments("image file")
    
    image <- readImageFile(Arguments[1])
    fileStem <- basename(image$getSource())
    kernel <- shapeKernel(3, min(3,image$getDimensionality()), type=ifelse(includeDiagonal,"box","diamond"))
    image$map(mmand::components, kernel=kernel)
    report(OL$Info, "There are #{max(image,na.rm=TRUE)} connected component(s) in the image")
    
    if (sortBySize)
    {
        sizes <- table(image$getData())
        report(OL$Info, "Largest component contains #{max(sizes)} voxel(s)")
        ranks <- length(sizes) - rank(sizes, na.last="keep", ties.method="random") + 1
        image$map(function(x) ranks[x])
    }
    
    writeImageFile(image, paste(fileStem,"components",sep="_"))
    
    invisible(NULL)
}
