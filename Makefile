install: install-base install-fsl

install-base:
	R CMD INSTALL tractor.base

install-fsl:
	R CMD INSTALL tractor.fsl