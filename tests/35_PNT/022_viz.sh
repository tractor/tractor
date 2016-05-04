#@desc Checking visualisation and projection into MNI space
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} pnt-viz data/session TractName:genu DatasetName:data/pnt/pnt_test_data ResultsName:tmp/pnt_results Streamlines:50
${TRACTOR} transform data/session genu.1 SourceSpace:diffusion TargetSpace:MNI
${TRACTOR} apply genu.1_MNI 'ifelse(a>=0.01*max(a),1L,0L)' ResultName:genu.1_thresholded
${TRACTOR} value genu.1_thresholded 100 168 83
rm -f tmp/pnt_results.Rdata
rm -f genu.1*
