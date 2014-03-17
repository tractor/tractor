#@desc Checking that we can extract the mean FA in a region
${TRACTOR} mkroi data/session 50 59 33 PointType:R Width:3 ROIName:tmp/region
${TRACTOR} mean tmp/region data/session Metric:FA AveragingMode:binary
rm -f tmp/region.*
