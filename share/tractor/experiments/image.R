#@desc Manipulate images within the file system, ensuring that all relevant files are handled, including auxiliary files. This script uses subcommands, so the first argument specifies exactly which operation will be performed. Available operations are "copy", "move" (or "rename"), "remove" (or "delete") and "link" - the latter for creating symlinks if the file system supports them. Standard Unix short forms of each command are also acceptable. Symlinks will use relative paths to the original file unless RelativeLinks:false is given. Existing files will be overwritten by default.
#@args subcommand, image(s), [directory]

# To ensure that '@' shorthand is expanded properly
library(tractor.session)

runExperiment <- function ()
{
    requireArguments("subcommand", "image(s)")
    
    auxiliaries <- getConfigVariable("Auxiliaries", "dirs,lut,tags", multiple=TRUE)
    overwrite <- getConfigVariable("Overwrite", TRUE)
    relativeLinks <- getConfigVariable("RelativeLinks", TRUE)
    
    subcommand <- match.arg(Arguments[1], c("cp","copy","mv","move","rename","rm","remove","delete","ln","link"))
    
    if (subcommand %in% c("rm","remove","delete"))
        removeImageFiles(Arguments[-1], auxiliaries=auxiliaries)
    else if (nArguments() < 3)
        report(OL$Error, "All operations except deletion require two or more arguments")
    else
    {
        # Directory, existing or new (the latter indicated by a trailing '/' in the last argument)
        if (isTRUE(file.info(Arguments[nArguments()])$isdir) || Arguments[nArguments()] %~% "/$")
        {
            from <- resolvePath(Arguments[c(-1L,-nArguments())])
            directory <- resolvePath(Arguments[nArguments()])
            if (!file.exists(directory))
                dir.create(directory)
            to <- file.path(directory, basename(from))
        }
        else if (nArguments() > 3)
            report(OL$Error, "Exactly one source and target file name should be given, unless the target is a directory")
        else
        {
            # Exactly three arguments, including the subcommand
            from <- resolvePath(Arguments[2])
            to <- resolvePath(Arguments[3])
        }
        
        if (subcommand %in% c("ln","link"))
            symlinkImageFiles(from, to, overwrite=overwrite, relative=relativeLinks, auxiliaries=auxiliaries)
        else
        {
            deleteOriginals <- (subcommand %in% c("mv","move","rename"))
            copyImageFiles(from, to, overwrite=overwrite, deleteOriginals=deleteOriginals, auxiliaries=auxiliaries)
        }
    }
}
