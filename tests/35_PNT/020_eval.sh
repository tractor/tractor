#@desc Checking that we can evaluate data against a model
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} peek tmp/pnt_results
${TRACTOR} pnt-interpret data/session TractName:genu ModelName:data/pnt/pnt_model DatasetName:data/pnt/pnt_test_data ResultsName:tmp/pnt_results Mode:location
${TRACTOR} pnt-interpret TractName:genu ModelName:data/pnt/pnt_model DatasetName:data/pnt/pnt_test_data ResultsName:tmp/pnt_results Mode:ratio | awk '{ print substr($1,1,6) }'
rm -f tmp/pnt_results.Rdata
