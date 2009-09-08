BASH=/bin/bash
ECHO=/bin/echo

COLUMNS=40
TRACTOR=../bin/tractor -q -z -i
TRACTOR_HOME=..
TRACTOR_FILETYPE=NIFTI_GZ
TRACTOR_PATH=scripts
export COLUMNS TRACTOR TRACTOR_HOME TRACTOR_FILETYPE TRACTOR_PATH

all: run-tests

clean:
	@rm -f `find . -name '*.out' -print`
	@rm -f `find . -name '*.Rdata' -print`
	@rm -rf tmp/* data/session-12dir/tractor/fdt.track

fullclean: clean
	@rm -f `find . -name '*.save' -print`

run-tests:
	@for file in `find . -name '*.sh' -print | sort`; do \
		outfile=`echo $${file} | sed 's/\.sh$$/\.out/'`; \
		$(MAKE) $${outfile} || exit 1; \
	done
	@echo "All tests completed"

create-tests:
	@for file in `find . -name '*.sh' -print | sort`; do \
		savefile=`echo $${file} | sed 's/\.sh$$/\.save/'`; \
		$(MAKE) $${savefile} || exit 1; \
	done

%.out: %.sh
	@$(ECHO) "`grep '#@desc' $< | sed 's/#@desc //'`... \c"
	@$(BASH) $< >$@ 2>&1
	@diff $*.save $@ && $(ECHO) "OK"

%.save: %.sh
	@$(ECHO) "Updating test output for $@... \c"
	@$(BASH) $< >$@ 2>&1 || ( echo ""; cat $@; exit 1 )
	@$(ECHO) "done"
