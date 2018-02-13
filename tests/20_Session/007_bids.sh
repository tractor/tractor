#@desc Checking BIDS compatibility
${TRACTOR} status $TRACTOR_TEST_DATA/bids/sub-01 | grep -v bids
