#@desc Checking we can create candidate tracts for PNT
${TRACTOR} pnt-data TractName:genu SessionList:data/session-12dir SeedPoint:49,58,14 PointType:R NumberOfSamples:50 DatasetName:tmp/pnt_test_data
rm -f tmp/pnt_test_data.txt
