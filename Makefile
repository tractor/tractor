INSTALL=/usr/bin/install
R=/usr/bin/R
TRUE=/usr/bin/true

all:
	@echo 'Nothing to compile - run "make install" to install packages'

install-base:
	$(R) CMD INSTALL tractor.base

install-fsl:
	$(R) CMD INSTALL tractor.fsl

install-nt:
	$(R) CMD INSTALL tractor.nt

install: install-base install-fsl install-nt

install-extras:
	@ mkdir -p ~/.tractor
	@ $(INSTALL) -Cv -m 0644 etc/experiments/*.R ~/.tractor
	@ [ -d ~/bin2 ] && $(INSTALL) -Cv bin/tractor ~/bin || $(TRUE)

extras: install-extras

