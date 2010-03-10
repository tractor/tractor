INSTALL=/usr/bin/install
R=R
PREFIX=/usr/local

all:
	@echo 'Nothing to compile - run "make install" to install packages'

install-base:
	$(R) CMD INSTALL tractor.base

install-fsl:
	@echo 'The fsl package is deprecated - run "make install-session" instead'

install-camino:
	@echo 'The camino package is deprecated - run "make install-session" instead'

install-session:
	$(R) CMD INSTALL tractor.session

install-nt:
	$(R) CMD INSTALL tractor.nt

install: install-base install-session install-nt

uninstall:
	$(R) CMD REMOVE tractor.nt tractor.session tractor.base

uninstall-deprecated:
	$(R) CMD REMOVE tractor.camino tractor.fsl

clean:
	@cd tests && $(MAKE) clean

test:
	@cd tests && $(MAKE) run-tests
