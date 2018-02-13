#@desc Checking that furrow works

# Note that furrow has no -w option, so it is run from the tests directory
# This is OK because it does not create any files
${FURROW} -s data/session basename @FA
