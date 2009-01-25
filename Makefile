INSTALL=/usr/bin/install
R=R
PREFIX=/usr/local

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

install-session:
	$(R) CMD INSTALL tractor.session

install: install-base install-fsl install-camino install-nt

install-extras:
	@read -p "The install-extras target is deprecated - are you sure? [yn] " ans && [ $$ans = "y" ]
	
	@mkdir -p ~/.tractor
	@$(INSTALL) -Cv -m 0644 etc/experiments/*.R ~/.tractor
	
	@mkdir -p $(PREFIX)/bin $(PREFIX)/share/man/man1
	@$(INSTALL) -Cv bin/tractor $(PREFIX)/bin
	@$(INSTALL) -Cv -m 0644 man/man1/tractor.1 $(PREFIX)/share/man/man1

extras: install-extras

