#@desc Checking that we can create candidate tracts for PNT
${TRACTOR} pnt-data TractName:genu SessionList:data/session SeedPoint:50,59,33 PointType:R NumberOfSamples:50 DatasetName:tmp/pnt_test_data
rm -f tmp/pnt_test_data.txt
