.tractorHome <- Sys.getenv("TRACTOR_HOME")

if (.tractorHome == "") {
	message("TRACTOR_HOME environment variable not set - TractoR will not be available")
} else if (!dir.exists(.tractorHome)) {
	message("TRACTOR_HOME is set but its target does not exist - TractoR will not be available")
} else {
	.libraries <- list(tractor=file.path(.tractorHome,"lib","R"), existing=.libPaths())
	.libPaths(c(.libraries$existing, .libraries$tractor))
	tractor <<- function (exptName = "console", args = NULL, configFiles = NULL, outputLevel = reportr::OL$Warning, ...) {
		# TractoR's library path should have priority during the call to callExperiment() only
		.libPaths(c(.libraries$tractor, .libraries$existing))
		on.exit(.libPaths(c(.libraries$existing, .libraries$tractor)))
		tractor.utils::callExperiment(exptName, args, configFiles, outputLevel, ...)
	}
	
	if (interactive()) {
		message(paste("TractoR is available at", .tractorHome, "- tractor() loads all packages"))
	}
}
