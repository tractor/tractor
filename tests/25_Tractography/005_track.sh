#@desc Checking that we can run FSL tractography
${TRACTOR} track data/session-12dir 49 58 14 PointType:R NumberOfSamples:50 TractName:tmp/tract CreateVolumes:true Force:true Tracker:fsl
${TRACTOR} value tmp/tract 49 58 14
rm -f tmp/tract*
