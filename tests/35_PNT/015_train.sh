#@desc Checking that we can train symmetric and asymmetric PNT models
${TRACTOR} pnt-train TractName:genu DatasetName:data/pnt/pnt_train_data AsymmetricModel:false
${TRACTOR} peek data/pnt/pnt_train_data_model
${TRACTOR} pnt-train TractName:genu DatasetName:data/pnt/pnt_train_data AsymmetricModel:true
${TRACTOR} peek data/pnt/pnt_train_data_model
rm -f data/pnt/pnt_train_data_model.Rdata
