#@desc Checking that we can extract the mean FA in a region
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 50 59 33 Width:3 ROIName:region
${TRACTOR} mean $TRACTOR_TEST_DATA/session@FA region AveragingMode:binary
${TRACTOR} mean $TRACTOR_TEST_DATA/session@FA AveragingMode:binary ThresholdLevel:0.2
