SHELL=/bin/bash
export SHELL

BASH=/bin/bash
ECHO=/bin/echo
ECHO_N=/bin/echo -n
DIFF=diff -b
TIME=time -p

# This TRACTOR_PATH is a bit of a kludge, pointing to the same directory from two different starting points (one for finding scripts, the other for workflows)
COLUMNS=40
TRACTOR=../bin/tractor -q -z -i -v0
PLOUGH=../bin/plough
FURROW=../bin/furrow -z
TRACTOR_HOME=..
TRACTOR_PATH=scripts:../../../scripts
export COLUMNS TRACTOR PLOUGH FURROW TRACTOR_HOME TRACTOR_FILETYPE TRACTOR_PATH

# Tests are run three levels down
TRACTOR_TEST_DATA=../../../data
export TRACTOR_TEST_DATA

all: run-tests

clean:
	@rm -f `find . -name '*.out' -print`
	@rm -rf tmp/*

fullclean: clean
	@rm -f `find . -name '*.save' -print`

run-tests:
	@mkdir -p tmp
	@$(MAKE) `find . -name '0*.sh' -print | sort | sed 's/\.sh$$/\.out/'` || exit 1
	@rm -rf tmp/*
	@echo "All tests completed"

debug-tests:
	@$(MAKE) run-tests TRACTOR="$(TRACTOR) -d"

create-tests:
	@$(MAKE) `find . -name '0*.sh' -print | sort | sed 's/\.sh$$/\.save/'` || exit 1

%.out: %.sh
	@$(ECHO_N) "`grep '#@desc' $< | sed 's/#@desc //'`... "
	@mkdir -p tmp/$*
	@test `grep -c "#@data" $<` -eq 0 || cp -R `grep '#@data' $< | sed 's/#@data //'` tmp/$*/
	@if test -f $*.skip && $(BASH) $*.skip 2>&1; then \
	 	$(ECHO) "SKIPPED"; \
	 else \
	 	$(TIME) ( TRACTOR="$(TRACTOR) -w tmp/$*" $(BASH) $< >$@ 2>&1 ) >tmp/$*/.time 2>&1 || ( $(ECHO) "FAIL"; cat $@; rm -f $@; exit 1 ); \
		real=`grep '^real' tmp/$*/.time | sed 's/real *//'`; $(DIFF) $*.save $@ >tmp/$*/.diff 2>&1 && ( $(ECHO) "OK ($$real s)" ) || ( $(ECHO) "FAIL"; cat tmp/$*/.diff; rm -f $@; exit 1 ); \
	 fi
	@rm -rf tmp/$*

%.save: %.sh
	@$(ECHO_N) "Updating test output for $@... "
	@mkdir -p tmp/$*
	@test `grep -c "#@data" $<` -eq 0 || cp -R `grep '#@data' $< | sed 's/#@data //'` tmp/$*/
	@TRACTOR="$(TRACTOR) -w tmp/$*" $(BASH) $< >$@ 2>&1 || ( $(ECHO) ""; cat $@; exit 1 )
	@$(ECHO) "done"
	@rm -rf tmp/$*
