BASH=/bin/bash
ECHO=/bin/echo
ECHO_N=/bin/echo -n
DIFF=diff -b

COLUMNS=40
TRACTOR=../bin/tractor -q -z -i
TRACTOR_HOME=..
TRACTOR_FILETYPE=NIFTI_GZ
TRACTOR_PATH=scripts
export COLUMNS TRACTOR TRACTOR_HOME TRACTOR_FILETYPE TRACTOR_PATH

all: run-tests

clean:
	@rm -f `find . -name '*.out' -print`
	@rm -rf tmp/* data/session-12dir/tractor/fdt.track/* data/session-12dir/tractor/objects/*

fullclean: clean
	@rm -f `find . -name '*.save' -print`

run-tests:
	@$(MAKE) `find . -name '*.sh' -print | sort | sed 's/\.sh$$/\.out/'` || exit 1
	@echo "All tests completed"

create-tests:
	@$(MAKE) `find . -name '*.sh' -print | sort | sed 's/\.sh$$/\.save/'` || exit 1

%.out: %.sh
	@$(ECHO_N) "`grep '#@desc' $< | sed 's/#@desc //'`... "
	@$(BASH) $< >$@ 2>&1 || ( $(ECHO) ""; cat $@; exit 1 )
	@$(DIFF) $*.save $@ && $(ECHO) "OK" || ( rm -f $@; exit 1 )

%.save: %.sh
	@$(ECHO_N) "Updating test output for $@... "
	@$(BASH) $< >$@ 2>&1 || ( $(ECHO) ""; cat $@; exit 1 )
	@$(ECHO) "done"
