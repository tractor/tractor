#@desc Checking CSD-based tractography
${TRACTOR} track $TRACTOR_TEST_DATA/session 50 59 33 PreferredModel:csd Streamlines:50 RequireMap:true
${TRACTOR} value tract 50 59 33
