#@desc Checking streamline pruning
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} -v1 pnt-prune TractName:genu DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results SessionList:data/session-12dir NumberOfSamples:50 RandomSeed:1 | grep -v length
rm -f tmp/pnt_results.Rdata
rm -f genu_session1.*