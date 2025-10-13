.tractorHome <- Sys.getenv("TRACTOR_HOME")

if (.tractorHome == "") {
	message("TRACTOR_HOME environment variable not set - TractoR will not be available")
} else {
	tractor <<- function (exptName = "console", args = NULL, configFiles = NULL, outputLevel = reportr::OL$Warning, ...) {
		.libPaths(c(file.path(.tractorHome,"lib","R"), .libPaths()))
		tractor.utils::callExperiment(exptName, args, configFiles, outputLevel, ...)
	}
	
	if (interactive()) {
		message(paste("TractoR is available at", .tractorHome, "- tractor() loads all packages"))
	}
}
