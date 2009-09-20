#@desc Checking we can run HNT with a standard reference tract
${TRACTOR} hnt-eval TractName:genu SessionList:data/session-12dir NumberOfSamples:50 ResultsName:tmp/hnt_results | grep -v unique
${TRACTOR} peek tmp/hnt_results
${TRACTOR} hnt-interpret TractName:genu SessionList:data/session-12dir ResultsName:tmp/hnt_results Mode:location
rm -f tmp/hnt_results.Rdata
