#@desc Checking that ROI-based tractography works
${TRACTOR} mkroi data/session-12dir 49 58 14 PointType:R Width:1 ROIName:tmp/region
${TRACTOR} mtrack data/session-12dir SeedMaskFile:tmp/region NumberOfSamples:50 TractName:tmp/tract CreateVolumes:true Tracker:fsl
${TRACTOR} value tmp/tract 49 58 14
rm -f tmp/tract* tmp/region.*
