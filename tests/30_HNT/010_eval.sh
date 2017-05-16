#@desc Checking that we can run HNT with a standard reference tract
${TRACTOR} hnt-eval $TRACTOR_TEST_DATA/session TractName:genu Streamlines:50 ResultsName:hnt_results | grep -v unique
${TRACTOR} peek hnt_results
${TRACTOR} hnt-interpret ResultsName:hnt_results Mode:location
