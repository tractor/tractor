#@desc Checking that we can build a graph from streamlines
${TRACTOR} graph-build $TRACTOR_TEST_DATA/session TractName:$TRACTOR_TEST_DATA/streamlines/wm2gm
${TRACTOR} peek graph
