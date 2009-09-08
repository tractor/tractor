#@desc Checking that we can create native space ROIs
${TRACTOR} mkroi data/session-12dir 49 58 14 PointType:R Width:3 ROIName:tmp/region
${TRACTOR} which tmp/region eq 1
rm -f tmp/region.*
