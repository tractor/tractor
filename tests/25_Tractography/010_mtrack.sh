#@desc Checking that ROI-based tractography works
${TRACTOR} mkroi data/session@FA 50 59 33 PointType:R Width:1 ROIName:tmp/region
${TRACTOR} track data/session tmp/region Streamlines:50x TractName:tmp/tract RequireMap:true
${TRACTOR} value tmp/tract 50 59 33
rm -f tmp/tract* tmp/region.*
