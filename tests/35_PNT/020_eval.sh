#@desc Checking that we can evaluate data against a model
${TRACTOR} pnt-eval DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model ResultsName:pnt_results
${TRACTOR} peek pnt_results
${TRACTOR} pnt-interpret $TRACTOR_TEST_DATA/session TractName:genu ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ResultsName:pnt_results Mode:location
${TRACTOR} pnt-interpret TractName:genu ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ResultsName:pnt_results Mode:ratio | awk '{ print substr($1,1,6) }'
