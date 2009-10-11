#@desc Checking that we can extract the mean FA in a region
${TRACTOR} mkroi data/session-12dir 49 58 14 PointType:R Width:3 ROIName:tmp/region
${TRACTOR} mean tmp/region data/session-12dir Metric:FA AveragingMode:binary
rm -f tmp/region.*
