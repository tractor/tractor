#@desc Checking evaluation against data without stored seed points
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_noseed_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} peek tmp/pnt_results
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session ModelName:data/pnt/pnt_model DatasetName:data/pnt/pnt_noseed_data ResultsName:tmp/pnt_results Mode:location
${TRACTOR} pnt-interpret TractName:genu SessionList:data/session ModelName:data/pnt/pnt_model DatasetName:data/pnt/pnt_noseed_data ResultsName:tmp/pnt_results Mode:ratio | awk '{ print substr($1,1,6) }'
rm -f tmp/pnt_results.Rdata
