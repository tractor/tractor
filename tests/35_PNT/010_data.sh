#@desc Checking that we can create candidate tracts for PNT
${TRACTOR} pnt-data data/session TractName:genu Streamlines:50 DatasetName:tmp/pnt_test_data
rm -f tmp/pnt_test_data.txt
