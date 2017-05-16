#@desc Checking "unsupervised" PNT
#@data data/pnt/pnt_train_data.txt
${TRACTOR} pnt-em TractName:genu DatasetName:pnt_train_data ResultsName:pnt_results AsymmetricModel:false
${TRACTOR} peek pnt_train_data_model
${TRACTOR} pnt-em TractName:genu DatasetName:pnt_train_data ResultsName:pnt_results AlphaPriorMean:0 AsymmetricModel:false
${TRACTOR} peek pnt_train_data_model
