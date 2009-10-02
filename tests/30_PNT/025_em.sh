#@desc Checking "unsupervised" PNT
${TRACTOR} pnt-em TractName:genu DatasetName:tmp/pnt_train_data ResultsName:tmp/pnt_results
${TRACTOR} peek tmp/pnt_train_data_model
${TRACTOR} pnt-em TractName:genu DatasetName:tmp/pnt_train_data ResultsName:tmp/pnt_results AlphaPriorMean:1
${TRACTOR} peek tmp/pnt_train_data_model
