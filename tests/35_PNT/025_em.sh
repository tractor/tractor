#@desc Checking "unsupervised" PNT
${TRACTOR} pnt-em TractName:genu DatasetName:data/pnt/pnt_train_data ResultsName:tmp/pnt_results AsymmetricModel:false
${TRACTOR} peek data/pnt/pnt_train_data_model
${TRACTOR} pnt-em TractName:genu DatasetName:data/pnt/pnt_train_data ResultsName:tmp/pnt_results AlphaPriorMean:0 AsymmetricModel:false
${TRACTOR} peek data/pnt/pnt_train_data_model
rm -f data/pnt/pnt_train_data_model.Rdata
rm -f tmp/pnt_results.Rdata
