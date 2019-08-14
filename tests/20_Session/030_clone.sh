#@desc Checking that we can perform a shallow clone of a session
${TRACTOR} clone $TRACTOR_TEST_DATA/session newsession Map:true
${FURROW} cat newsession/tractor/map.yaml
