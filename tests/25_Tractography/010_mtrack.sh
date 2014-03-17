#@desc Checking that ROI-based tractography works
${TRACTOR} mkroi data/session 50 59 33 PointType:R Width:1 ROIName:tmp/region
${TRACTOR} mtrack data/session SeedMaskFile:tmp/region NumberOfSamples:50 TractName:tmp/tract CreateVolumes:true
${TRACTOR} value tmp/tract 50 59 33
rm -f tmp/tract* tmp/region.*
