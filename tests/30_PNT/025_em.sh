#@desc Checking "unsupervised" PNT
${TRACTOR} pnt-em TractName:genu DatasetName:data/pnt/pnt_train_data ResultsName:tmp/pnt_results
${TRACTOR} peek data/pnt/pnt_train_data_model
${TRACTOR} pnt-em TractName:genu DatasetName:data/pnt/pnt_train_data ResultsName:tmp/pnt_results AlphaPriorMean:1
${TRACTOR} peek data/pnt/pnt_train_data_model
rm -f data/pnt/pnt_train_data_model.Rdata
rm -f tmp/pnt_results.Rdata
