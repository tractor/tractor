#@desc Checking we can train symmetric and asymmetric PNT models
${TRACTOR} pnt-data TractName:genu SessionList:data/session-12dir NumberOfSamples:50 DatasetName:tmp/pnt_train_data
${TRACTOR} pnt-train TractName:genu DatasetName:tmp/pnt_train_data
${TRACTOR} peek tmp/pnt_train_data_model
${TRACTOR} pnt-train TractName:genu DatasetName:tmp/pnt_train_data AsymmetricModel:true
${TRACTOR} peek tmp/pnt_train_data_model
