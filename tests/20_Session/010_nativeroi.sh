#@desc Checking that we can create native space ROIs
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 50 59 33 Width:3 ROIName:region
${TRACTOR} which region eq 1
