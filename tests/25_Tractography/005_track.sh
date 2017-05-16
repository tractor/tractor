#@desc Checking that we can run tractography
${TRACTOR} track $TRACTOR_TEST_DATA/session 50 59 33 Streamlines:50 RequireMap:true
${TRACTOR} value tract 50 59 33
