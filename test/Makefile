BASH=/bin/bash
DIFF=diff
SED=sed
TRACTOR=../bin/tractor

diff-file=/tmp/tractor_test.diff

all: run-tests

clean:
	@rm *.out

run-tests:
	@for file in `ls -1 *.sh`; do \
		outfile=`echo $${file} | $(SED) 's/\.sh$$/\.out/'`; \
		$(MAKE) $${outfile} || exit 1; \
	done

create-tests:
	@for file in `ls -1 *.sh`; do \
		savefile=`echo $${file} | $(SED) 's/\.sh$$/\.save/'`; \
		$(MAKE) $${savefile} || exit 1; \
	done

%.out: %.sh
	@TRACTOR=$(TRACTOR) $(BASH) $< >$@ 2>&1
	@-$(DIFF) $*.save $@ >$(diff-file)
	@if [ -s $(diff-file) ]; then \
		cat $(diff-file); \
		rm $(diff-file); \
		exit 1; \
	else \
		rm $(diff-file) ; \
	fi

%.save: %.sh
	@TRACTOR=$(TRACTOR) $(BASH) $< >$@ 2>&1
