#@desc Checking we can evaluate data against a model
${TRACTOR} pnt-eval DatasetName:tmp/pnt_test_data ModelName:tmp/pnt_train_data_model ResultsName:tmp/pnt_results
${TRACTOR} peek tmp/pnt_results
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session-12dir ModelName:tmp/pnt_train_data_model DatasetName:tmp/pnt_test_data ResultsName:tmp/pnt_results Mode:location
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session-12dir ModelName:tmp/pnt_train_data_model DatasetName:tmp/pnt_test_data ResultsName:tmp/pnt_results Mode:ratio | awk '{ print substr($1,1,6) }'
rm -f tmp/pnt_results.Rdata
