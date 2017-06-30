#@desc Checking tensor-based tractography
${TRACTOR} track $TRACTOR_TEST_DATA/session 50 59 33 PreferredModel:dti Streamlines:50 RequireMap:true
${TRACTOR} value tract 50 59 33
