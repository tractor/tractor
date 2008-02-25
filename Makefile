INSTALL=/usr/bin/install
R=/usr/bin/R
BIN_PREFIX=/usr/local

all:
	@echo 'Nothing to compile - run "make install" to install packages'

install-base:
	$(R) CMD INSTALL tractor.base

install-fsl:
	$(R) CMD INSTALL tractor.fsl

install-camino:
	$(R) CMD INSTALL tractor.camino

install-nt:
	$(R) CMD INSTALL tractor.nt

install: install-base install-fsl install-camino install-nt

install-extras:
	@mkdir -p ~/.tractor
	@$(INSTALL) -Cv -m 0644 etc/experiments/*.R ~/.tractor
	@[ -d $(BIN_PREFIX)/bin ] || mkdir -p $(BIN_PREFIX)/bin
	@$(INSTALL) -Cv bin/tractor $(BIN_PREFIX)/bin

extras: install-extras

