#@desc Checking that we can extract the mean FA in a region
${TRACTOR} mkroi data/session@FA 50 59 33 PointType:R Width:3 ROIName:tmp/region
${TRACTOR} mean data/session@FA tmp/region AveragingMode:binary
${TRACTOR} mean data/session@FA AveragingMode:binary ThresholdLevel:0.2
rm -f tmp/region.*
