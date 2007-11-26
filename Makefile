all:
	@echo 'Nothing to compile - run "make install" to install packages'

install-base:
	R CMD INSTALL tractor.base

install-fsl:
	R CMD INSTALL tractor.fsl

install-nt:
	R CMD INSTALL tractor.nt

install: install-base install-fsl install-nt
