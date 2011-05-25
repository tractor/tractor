R=R
INSTALL=bin/tractor_Rinstall

all:
	@echo 'Run "make install" to install packages'

install-libs:
	@$(INSTALL) lib/reportr

install-base:
	@$(INSTALL) tractor.base

install-utils:
	@$(INSTALL) tractor.utils

install-session:
	@$(INSTALL) tractor.session

install-nt:
	@$(INSTALL) tractor.nt

install: install-libs install-base install-utils install-session install-nt

install-native:
	@$(INSTALL) tractor.native
	@$(INSTALL) lib/multicore

install-all: install install-native

uninstall:
	$(R) CMD REMOVE tractor.nt tractor.session tractor.utils tractor.base reportr

uninstall-native:
	$(R) CMD REMOVE tractor.native multicore

uninstall-all: uninstall uninstall-native

clean:
	@cd tests && $(MAKE) clean

test:
	@cd tests && $(MAKE) run-tests

dtest:
	@cd tests && $(MAKE) debug-tests
