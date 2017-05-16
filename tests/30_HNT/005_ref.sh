#@desc Checking that we can create an HNT reference tract
${TRACTOR} hnt-ref $TRACTOR_TEST_DATA/session 50 59 33 Streamlines:50
${TRACTOR} peek tract_ref | grep -v session | sed 's/\.[0-9][0-9]*//g'
