#@desc Checking that we can train symmetric and asymmetric PNT models
#@data data/pnt/pnt_train_data.txt
${TRACTOR} pnt-train TractName:genu DatasetName:pnt_train_data AsymmetricModel:false
${TRACTOR} peek pnt_train_data_model
${TRACTOR} pnt-train TractName:genu DatasetName:pnt_train_data AsymmetricModel:true
${TRACTOR} peek pnt_train_data_model
