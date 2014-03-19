#@args MD5 checksum file
#@nohistory TRUE

runExperiment <- function ()
{
    table <- read.table(Arguments[1], stringsAsFactors=FALSE)
    checksums <- tools::md5sum(table[,2])
    mismatches <- table[checksums!=table[,1], 2]
    
    if (length(mismatches) > 0)
    {
        cat(paste("Mismatches:\n  ", implode(mismatches,"\n  "), "\n", sep=""))
        report(OL$Error, "Mismatches found")
    }
}
