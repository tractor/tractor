tractorHome <- Sys.getenv("TRACTOR_HOME")

if (tractorHome == "") {
	message("TRACTOR_HOME environment variable not set - TractoR will not be available")
} else {
	.libPaths(c(.libPaths(), file.path(tractorHome,"lib","R")))
	
	tractor <<- function (exptName = "console", args = NULL, configFiles = NULL, outputLevel = reportr::OL$Warning, ...) {
		tractor.utils::callExperiment(exptName, args, configFiles, outputLevel, ...)
	}
	
	if (interactive()) {
		message(paste("TractoR at", tractorHome, "is configured for use - tractor() loads all packages"))
	}
}

rm(tractorHome)
