SHELL=/bin/bash
export SHELL

BASH=/bin/bash
ECHO=/bin/echo
ECHO_N=/bin/echo -n
DIFF=diff -b
TIME=time -p
TEE=tee

DEBUG=0
COLUMNS=40
TRACTOR_HOME=$(PWD)/..
TRACTOR_PATH=$(TRACTOR_HOME)/tests/scripts
PATH+=$(TRACTOR_HOME)/bin
TRACTOR=$(TRACTOR_HOME)/bin/tractor -q -z -i -v0
PLOUGH=$(TRACTOR_HOME)/bin/plough -q -z -i -v0
FURROW=$(TRACTOR_HOME)/bin/furrow -z
export COLUMNS TRACTOR_HOME TRACTOR_PATH PATH TRACTOR PLOUGH FURROW

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
	@$(MAKE) run-tests TRACTOR="$(TRACTOR) -d" DEBUG=1

create-tests:
	@$(MAKE) `find . -name '0*.sh' -print | sort | sed 's/\.sh$$/\.save/'` || exit 1

%.out: %.sh
	@$(ECHO_N) "`grep '#@desc' $< | sed 's/#@desc //'`... "
	@mkdir -p tmp/$*
	@test `grep -c "#@data" $<` -eq 0 || cp -R `grep '#@data' $< | sed 's/#@data //'` tmp/$*/
	@if test -f $*.skip && $(BASH) $*.skip 2>&1; then \
	 	$(ECHO) "SKIPPED"; \
	 elif test $(DEBUG) -eq 1; then \
	 	$(ECHO) "$<"; \
	 	cd tmp/$* && $(BASH) "../../../$<" 2>&1 | $(TEE) "../../../$@"; \
	 else \
	 	$(TIME) ( cd tmp/$* && $(BASH) "../../../$<" >"../../../$@" 2>&1 ) >tmp/$*/.time 2>&1 || ( $(ECHO) "FAIL"; cat $@; rm -f $@; exit 1 ); \
		real=`grep '^real' tmp/$*/.time | sed 's/real *//'`; $(DIFF) $*.save $@ >tmp/$*/.diff 2>&1 && ( $(ECHO) "OK ($$real s)" ) || ( $(ECHO) "FAIL"; cat tmp/$*/.diff; rm -f $@; exit 1 ); \
	 fi
	@rm -rf tmp/$*

%.save: %.sh
	@$(ECHO_N) "Updating test output for $@... "
	@mkdir -p tmp/$*
	@test `grep -c "#@data" $<` -eq 0 || cp -R `grep '#@data' $< | sed 's/#@data //'` tmp/$*/
	@cd tmp/$* && $(BASH) "../../../$<" >"../../../$@" 2>&1 || ( $(ECHO) ""; cat $@; exit 1 )
	@$(ECHO) "done"
	@rm -rf tmp/$*
