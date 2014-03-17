#@desc Checking visualisation and group mapping
${TRACTOR} pnt-eval DatasetName:data/pnt/pnt_test_data ModelName:data/pnt/pnt_model ResultsName:tmp/pnt_results
${TRACTOR} pnt-viz TractName:genu ResultsName:tmp/pnt_results SessionList:data/session NumberOfSamples:50 CreateVolumes:true CreateImages:false
${TRACTOR} gmap genu_session SessionList:data/session TractName:genu ThresholdLevel:0.01 ThresholdRelativeTo:max
${TRACTOR} value genu_group_map 46 78 42
rm -f tmp/pnt_results.Rdata
rm -f genu_session1.*
rm -f genu_group_map.*
