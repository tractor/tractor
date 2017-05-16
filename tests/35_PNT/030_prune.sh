#@desc Checking streamline pruning
${TRACTOR} pnt-eval DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model ResultsName:pnt_results
${TRACTOR} pnt-prune $TRACTOR_TEST_DATA/session TractName:genu DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model ResultsName:pnt_results Streamlines:50 RandomSeed:1 | grep -v outside | grep -v rejected
