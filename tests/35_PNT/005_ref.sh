#@desc Checking that we can create a PNT reference tract
${TRACTOR} pnt-ref $TRACTOR_TEST_DATA/session 50 59 33 Streamlines:50 | grep -v rank-deficient
${TRACTOR} peek tract_ref | grep -v session | grep -v spacing | sed '/seed/s/\.[0-9]*//g'
