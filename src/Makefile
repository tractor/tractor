ECHO=/bin/echo
ECHO_N=/bin/echo -n

all: tractor.c tractor.h
	@$(ECHO_N) "Building tractor executable... "
	@mkdir -p ../bin/exec
	@CC=`$(R) CMD config CC`; CPPFLAGS=`$(R) CMD config --cppflags`; LDFLAGS=`$(R) CMD config --ldflags`; $$CC $$CPPFLAGS $$LDFLAGS tractor.c -o ../bin/exec/tractor >build.log 2>&1 && $(ECHO) "OK" || ( $(ECHO) "FAIL"; exit 0 )
