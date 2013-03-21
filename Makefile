R=R
INSTALL=bin/tractor_Rinstall

default: build post-build-info

post-build-info:
	@echo 'Run "make install" to install packages'

build:
	@cd src && $(MAKE) R=$(R)

post-install-info:
	@echo
	@echo "Installation complete. You may wish to add the following lines"
	@echo "to your ~/.bashrc file (or equivalent for other shells):"
	@echo
	@echo "  export TRACTOR_HOME=`pwd`"
	@echo '  export PATH=$${TRACTOR_HOME}/bin:$${PATH}'
	@echo '  export MANPATH=$${TRACTOR_HOME}/man:$${MANPATH}'
	@echo
	@echo "The ~/.bashrc file can be created if it does not already exist."

install-libs:
	@$(INSTALL) lib/reportr
	@$(INSTALL) lib/mmand
	@$(INSTALL) lib/multicore

install-base:
	@$(INSTALL) tractor.base

install-utils:
	@$(INSTALL) tractor.utils

install-session:
	@$(INSTALL) tractor.session

install-nt:
	@$(INSTALL) tractor.nt

install-native:
	@$(INSTALL) tractor.native

install-reg:
	@$(INSTALL) lib/bitops
	@$(INSTALL) lib/oro.nifti
	@$(INSTALL) lib/RNiftyReg
	@$(INSTALL) tractor.reg

install: build install-libs install-base install-utils install-session install-nt install-native install-reg post-install-info

install-all: install

uninstall:
	$(R) CMD REMOVE tractor.reg tractor.native tractor.nt tractor.session tractor.utils tractor.base

uninstall-all: uninstall
	$(R) CMD REMOVE RNiftyReg oro.nifti bitops multicore mmand reportr

clean:
	@cd tests && $(MAKE) clean

distclean: clean
	@rm -f bin/exec/tractor

test:
	@cd tests && $(MAKE) run-tests

dtest:
	@cd tests && $(MAKE) debug-tests
