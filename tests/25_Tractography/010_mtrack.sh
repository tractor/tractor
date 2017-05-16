#@desc Checking that ROI-based tractography works
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 50 59 33 Width:1 ROIName:region
${TRACTOR} track $TRACTOR_TEST_DATA/session region Streamlines:50x RequireMap:true
${TRACTOR} value tract 50 59 33
