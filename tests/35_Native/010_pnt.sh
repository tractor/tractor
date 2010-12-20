#@desc Checking that we can use the native tracker in PNT functions
${TRACTOR} pnt-data TractName:genu SessionList:data/session-12dir SeedPoint:49,58,14 PointType:R NumberOfSamples:50 DatasetName:tmp/pnt_test_data Tracker:tractor
rm -f tmp/pnt_test_data.txt
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} pnt-prune TractName:genu DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results SessionList:data/session-12dir NumberOfSamples:50 RandomSeed:1 Tracker:tractor | grep -v outside | grep -v rejected
rm -f tmp/pnt_results.Rdata
rm -f genu_session1.*
