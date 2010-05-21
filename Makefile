R=R

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

install-native:
	cd lib && $(R) CMD INSTALL multicore

install-all: install install-native

uninstall:
	$(R) CMD REMOVE tractor.nt tractor.session tractor.base

uninstall-native:
	$(R) CMD REMOVE multicore

uninstall-all: uninstall uninstall-native

uninstall-deprecated:
	$(R) CMD REMOVE tractor.camino tractor.fsl

clean:
	@cd tests && $(MAKE) clean

test:
	@cd tests && $(MAKE) run-tests
