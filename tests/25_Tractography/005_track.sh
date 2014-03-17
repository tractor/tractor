#@desc Checking that we can run tractography
${TRACTOR} track data/session 50 59 33 PointType:R NumberOfSamples:50 TractName:tmp/tract CreateVolumes:true
${TRACTOR} value tmp/tract 50 59 33
rm -f tmp/tract*
