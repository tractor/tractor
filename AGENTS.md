This project is called TractoR (for Tractography with R). Its purpose is provide medical image processing tools with a particular focus on magnetic resonance imaging and tractography, based on the R statistical language. It consists of several core R packages (`tractor.base`, `tractor.reg`, `tractor.track`, etc.), and vendors all of its dependency packages. Many of the dependencies are first-party, sometimes having been hived off from the TractoR codebase because they have more general applicability, and all are on CRAN.

The central repository is <https://github.com/tractor/tractor.git>. Of the core packages, only `tractor.base` is currently on CRAN.

## Structure and organisation

The core R packages sit at the top level, with vendored dependencies as git submodules within `lib/`. All packages are installed into the `lib/R/` subdirectory, which is a local package library and takes precedence over the system library for TractoR's purposes.

The `bin/` directory contains binaries and scripts intended to be on the user's `PATH`. `tractor` is the primary command-line interface for common tasks and when users are unwilling or unable to use R directly. It takes the name of an "experiment script" as an argument, in the form of a subcommand, which specifies a file with a .R extension and containing a `runExperiment()` function that takes no direct arguments but may be passed "configuration variables" from the command line. The latter are retrieved by the `getConfigVariable()` function, which allows defaults and expected types to be specified. `plough` is a wrapper that supports parallelisation of multiple `tractor` runs; `furrow` allows third-party commands to be run with some of TractoR's support infrastructure applied as preprocessing for convenience. The agricultural theme has stuck as a bit of fun.

Unlike `tractor` and `plough`, which are `bash` scripts, `furrow` is a top-level R script, run through the `lr` binary vended by the `littler` package. A `tractor` native binary is compiled from the source in `src/` and installed into `libexec/` -- this is a thin C wrapper around R's main loop, with some small added niceties like coloured error and warning output.

The set of experiment scripts shipped with TractoR is in `share/tractor/experiments/`; additional ones can be added by users by placing suitable R files on the filesystem or in packages. There are standard locations for these, which can be extended with the `TRACTOR_PATH` and `TRACTOR_PACKAGES` environment variables.

R function documentation generally uses `roxygen2`, although some core packages haven't yet fully adopted this. There is man-page documentation for the top-level binaries in `share/man/` and HTML documentation in `share/doc/`. The latter is an offline snapshot of the package's static website, which is at <https://tractor-mri.org.uk>. Other subdirectories of `share/tractor/` contain standard-space images, defaults and reference material.

The top-level Makefile handles installation (`make install`), uninstallation (`make uninstall`), unit testing (`make utest`) and integration testing (`make test`).

## Testing

Several (but not yet all) of the core packages have unit tests written against the `tinytest` package, which is not vendored but installed from CRAN if needed (e.g., via `make utest`). These generally focus on checking important invariances, testing for regressions against old bugs and ensuring package functions behave as documented. Non-visible package internals don't need to remain stable from release to release, so stability isn't worth testing unless it's critical.

There are also higher-level integration-type tests within `tests/`, which are small shell scripts with pre-stored output. Many of these leverage the minimal dataset within `tests/data/session/`, and/or other files under `tests/data/`. A few utilise auxiliary experiment scripts included with the `tractor.utils` package. Output from a previous run can be removed with `make clean`, otherwise only newly updated integration test scripts will be re-run. 

Modest changes that touch only one package can generally just be checked against that package's own unit tests (where there are some), using `tinytest` functions. A full run-through of the integration tests is only necessary after larger, more structural changes that might affect data interpretation or impact cross-package functionality.

## Conventions

TractoR is designed to work with MRI data sets, each consisting of a series of magnetic resonance images, potentially including structural, diffusion-weighted and functional images. The package stores all images and other files within a managed file hierarchy called a "session directory", or just "session", whose layout is specified within `share/tractor/session/default/`. Users can override the default layout per-session using "map files". Session paths or objects are frequently passed as arguments to package functions and experiment scripts, to save the user from having to specify paths to lots of individual files.

TractoR’s preferred file format for images is the NIfTI-1 format, although NIfTI-2, MRtrix .mif files, the legacy Analyze format and Freesurfer’s MGH/MGZ format are also supported. TractoR can also read from DICOM files, but not write to them.

Tractography streamlines are stored in TrackVis .trk format, although MRtrix .tck format is supported for reading.

Serialised R objects are usually stored in files with an .Rdata extension.

R-like 1-based indexing is used by experiment scripts where needed.

## Coding style

R functions and variables are in lower camel case; reference class names use upper camel case. In C++, types use upper camel case while free functions, methods and variables use lower camel case. In both languages opening braces go on their own lines, but braces aren't used for single statements. *Whitespace lines are indented to match the surrounding code or comment.*

The minimum target R version is currently 3.6.2, and C++ targets C++11 but must remain compatible with C++20. These lower bounds will be loosened at or before the next major release (TractoR v4.0.0), likely to R 4.2.0 and C++17.

The vendored packages (including the first-party ones) have their own styles and do not necessarily conform with TractoR style.
