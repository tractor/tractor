#@desc Checking that we can build graphs from functional data
${TRACTOR} graph-build $TRACTOR_TEST_DATA/session Type:functional
${TRACTOR} peek graph
${TRACTOR} graph-build $TRACTOR_TEST_DATA/session Type:functional UseShrinkage:true
${TRACTOR} peek graph
