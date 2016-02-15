#@desc Checking that we can run HNT with a standard reference tract
${TRACTOR} hnt-eval TractName:genu SessionList:data/session Streamlines:50 ResultsName:tmp/hnt_results | grep -v unique
${TRACTOR} peek tmp/hnt_results
${TRACTOR} hnt-interpret TractName:genu SessionList:data/session ResultsName:tmp/hnt_results Mode:location
rm -f tmp/hnt_results.Rdata
