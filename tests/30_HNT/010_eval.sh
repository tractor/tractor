#@desc Checking that we can run HNT with a standard reference tract
${TRACTOR} hnt-eval data/session TractName:genu Streamlines:50 ResultsName:tmp/hnt_results | grep -v unique
${TRACTOR} peek tmp/hnt_results
${TRACTOR} hnt-interpret ResultsName:tmp/hnt_results Mode:location
rm -f tmp/hnt_results.Rdata
