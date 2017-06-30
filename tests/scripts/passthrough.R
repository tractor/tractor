#@args command name, arguments

runExperiment <- function ()
{
    execute(Arguments[1], Arguments[-1])
}
