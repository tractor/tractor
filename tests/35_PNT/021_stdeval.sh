#@desc Checking that we can use pretrained models
export TRACTOR_REFTRACT_SET=miua2017
${TRACTOR} pnt-data $TRACTOR_TEST_DATA/session TractName:genu Streamlines:50 DatasetName:pnt_test_data
${TRACTOR} pnt-eval TractName:genu DatasetName:pnt_test_data ResultsName:pnt_results
${TRACTOR} peek pnt_results
${TRACTOR} pnt-interpret $TRACTOR_TEST_DATA/session TractName:genu DatasetName:pnt_test_data ResultsName:pnt_results Mode:location
