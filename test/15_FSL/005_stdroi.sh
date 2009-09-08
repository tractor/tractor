#@desc Checking that standard space registration works (FSL required)
${TRACTOR} mkroi data/session-12dir 0,18,16 PointType:mm CentreInMNISpace:true Width:3 ROIName:tmp/region
${TRACTOR} which tmp/region eq 1
rm -f tmp/region.*
