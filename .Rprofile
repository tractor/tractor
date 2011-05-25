loadTractor <- function ()
{
    require(reportr)
    require(methods)

    files <- list.files("tractor.base/R", full.names=TRUE)
    for (file in files)
        source(file)
    .onLoad(NULL, NULL)

    files <- list.files("tractor.utils/R", full.names=TRUE)
    for (file in files)
        source(file)

    files <- list.files("tractor.session/R", full.names=TRUE)
    for (file in files)
        source(file)
    .onLoad(NULL, NULL)

    files <- list.files("tractor.nt/R", full.names=TRUE)
    for (file in files)
        source(file)
}
