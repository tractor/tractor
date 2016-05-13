include ${R_HOME}/etc${R_ARCH}/Makeconf

all: ../bin/exec/tractor

## The following is not needed, but avoids PIC flags.
tractor.o: tractor.c tractor.h
	$(CC) $(ALL_CPPFLAGS) $(CFLAGS) -c tractor.c -o $@

## replace $(LIBR) $(LIBS) by $(STATIC_LIBR) if R was build with a static libR
../bin/exec/tractor: tractor.o
	@mkdir -p ../bin/exec
	$(MAIN_LINK) -o $@ tractor.o $(LIBR) $(LIBS) || $(MAIN_LINK) -o $@ tractor.o $(STATIC_LIBR)
