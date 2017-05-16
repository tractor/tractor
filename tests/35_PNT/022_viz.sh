#@desc Checking visualisation and projection into MNI space
${TRACTOR} pnt-eval DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ModelName:$TRACTOR_TEST_DATA/pnt/pnt_model ResultsName:pnt_results
${TRACTOR} pnt-viz $TRACTOR_TEST_DATA/session TractName:genu DatasetName:$TRACTOR_TEST_DATA/pnt/pnt_test_data ResultsName:pnt_results Streamlines:50
${TRACTOR} transform $TRACTOR_TEST_DATA/session genu.1 SourceSpace:diffusion TargetSpace:MNI
${TRACTOR} apply genu.1_MNI 'ifelse(a>=0.01*max(a),1L,0L)' ResultName:genu.1_thresholded
${TRACTOR} value genu.1_thresholded 100 168 83
