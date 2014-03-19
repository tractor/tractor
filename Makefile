SHELL=/bin/bash
export SHELL

R=R
ECHO=/bin/echo
ECHO_N=/bin/echo -n
GIT=git
MD5=md5
INSTALL=bin/tractor_Rinstall

default: build post-build-info

post-build-info:
	@$(ECHO) 'Run "make install" to install packages'

build:
	@cd src && $(MAKE) R=$(R)

post-install-info:
	@$(ECHO)
	@$(ECHO) "Installation complete. You may wish to add the following lines"
	@$(ECHO) "to your ~/.bashrc file (or equivalent for other shells):"
	@$(ECHO)
	@$(ECHO) "  export TRACTOR_HOME=`pwd`"
	@$(ECHO) '  export PATH=$${TRACTOR_HOME}/bin:$${PATH}'
	@$(ECHO) '  export MANPATH=$${TRACTOR_HOME}/man:$${MANPATH}'
	@$(ECHO)
	@$(ECHO) "The ~/.bashrc file can be created if it does not already exist."

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

install-track:
	@$(INSTALL) tractor.track

install-graph:
	@$(INSTALL) -k lib/igraph
	@$(INSTALL) tractor.graph

install: build
	@rm -f install.log
	@$(MAKE) install-libs install-base install-utils install-reg install-session install-nt install-track install-graph post-install-info

install-local:
	@mkdir -p lib/R
	@$(MAKE) install

install-all: install

uninstall:
	$(R) CMD REMOVE tractor.graph tractor.track tractor.nt tractor.session tractor.reg tractor.utils tractor.base

uninstall-local:
	@rm -rf lib/R

uninstall-all: uninstall
	$(R) CMD REMOVE RNiftyReg oro.nifti bitops multicore mmand reportr

clean:
	@cd tests && $(MAKE) clean

distclean: clean
	@rm -f bin/exec/tractor src/build.log install.log
	@rm -f tractor.track/config.log tractor.track/config.status tractor.track/src/Makevars tractor.track/src/config.h

test:
	@cd tests && $(MAKE) run-tests

dtest:
	@cd tests && $(MAKE) debug-tests

create-md5:
	@$(GIT) ls-files | grep -v -e '^lib/' -e '^etc/md5.txt' -e '\.git' | xargs $(MD5) -r >etc/md5.txt

check-md5:
	@mkdir -p tmp
	@$(ECHO_N) "Checking MD5 checksums... "
	@bin/tractor -q -z -i tests/scripts/check-md5 etc/md5.txt >tmp/md5.txt && $(ECHO) "OK" || ( $(ECHO) "FAIL"; sed '$$ d' tmp/md5.txt )
	@rm -f tmp/md5.txt
