SHELL=/bin/bash
export SHELL

R=R
ECHO=/bin/echo
ECHO_N=/bin/echo -n
GIT=git
MD5=md5
FETCH=bin/tractor_pkgget -m
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

do-install-libs:
	@mkdir -p tmp
	@rm -f tmp/*
	@$(FETCH) Rcpp 0.11.5
	@$(FETCH) RcppArmadillo 0.4.650.1.1
	@$(FETCH) bitops 1.0-6
	@$(FETCH) oro.nifti 0.4.3
	@$(FETCH) corpcor 1.6.7
	@$(FETCH) igraph 0.7.1
	@$(INSTALL) tmp/Rcpp_* tmp/RcppArmadillo_* tmp/bitops_* tmp/oro.nifti_* tmp/corpcor_* tmp/igraph_*
	@rm -f tmp/*

install-libs:
	@current_version=`cat VERSION`; \
	[ -f lib/.VERSION ] && installed_version=`cat lib/.VERSION` || installed_version=0; \
	[ $${current_version} = $${installed_version} ] || ( $(MAKE) do-install-libs && cp VERSION lib/.VERSION )

install-base:
	@$(INSTALL) lib/reportr tractor.base

install-utils:
	@$(INSTALL) tractor.utils

install-reg:
	@$(INSTALL) lib/RNiftyReg tractor.reg

install-session:
	@$(INSTALL) -k lib/mmand
	@$(INSTALL) tractor.session

install-nt:
	@$(INSTALL) tractor.nt

install-track:
	@$(INSTALL) tractor.track

install-graph:
	@$(INSTALL) tractor.graph

install: build
	@rm -f install.log
	@$(MAKE) install-libs install-base install-utils install-reg install-session install-nt install-track install-graph post-install-info

install-local: install

install-global:
	@$(MAKE) install INSTALL="$(INSTALL) -g"

install-all: install

uninstall:
	@rm -rf lib/R

uninstall-local: uninstall

uninstall-global:
	$(R) CMD REMOVE tractor.graph tractor.track tractor.nt tractor.session tractor.reg tractor.utils tractor.base

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
