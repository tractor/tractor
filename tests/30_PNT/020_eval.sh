#@desc Checking that we can evaluate data against a model
${TRACTOR} pnt-eval TractName:genu DatasetName:tmp/pnt_train_data TestDatasetName:tmp/pnt_test_data ResultsName:tmp/pnt_results
${TRACTOR} peek tmp/pnt_results
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session-12dir DatasetName:tmp/pnt_train_data ResultsName:tmp/pnt_results Mode:location
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session-12dir DatasetName:tmp/pnt_train_data ResultsName:tmp/pnt_results Mode:ratio | awk '{ print substr($1,1,6) }'
rm -f tmp/pnt_results.Rdata
