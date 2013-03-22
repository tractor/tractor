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
	@$(INSTALL) lib/reportr lib/multicore

install-base:
	@$(INSTALL) tractor.base

install-utils:
	@$(INSTALL) tractor.utils

install-reg:
	@$(INSTALL) lib/bitops lib/oro.nifti lib/RNiftyReg tractor.reg

install-session:
	@$(INSTALL) lib/mmand tractor.session

install-nt:
	@$(INSTALL) tractor.nt

install-native:
	@$(INSTALL) tractor.native

install: build
	@rm -f install.log
	@$(MAKE) install-libs install-base install-utils install-reg install-session install-nt install-native post-install-info

install-all: install

uninstall:
	$(R) CMD REMOVE tractor.native tractor.nt tractor.session tractor.reg tractor.utils tractor.base

uninstall-all: uninstall
	$(R) CMD REMOVE RNiftyReg oro.nifti bitops multicore mmand reportr

clean:
	@cd tests && $(MAKE) clean

distclean: clean
	@rm -f bin/exec/tractor src/build.log install.log
	@rm -f tractor.native/config.log tractor.native/config.status tractor.native/src/Makevars tractor.native/src/config.h

test:
	@cd tests && $(MAKE) run-tests

dtest:
	@cd tests && $(MAKE) debug-tests
