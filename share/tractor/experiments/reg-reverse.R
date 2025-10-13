#@args registration file
#@desc Create a version of the specified registration that is logically reversed, with the source and target images swapped.
#@group Registration

library(tractor.reg)

runExperiment <- function ()
{
    requireArguments("registration file")
    reg <- readRegistration(Arguments[1])
    reversed <- reverseRegistration(reg)
    fileStem <- ensureFileSuffix(basename(Arguments[1]), NULL, strip=c("xfmb","Rdata"))
    reversed$serialise(paste(fileStem, "reversed", sep="_"))
}
