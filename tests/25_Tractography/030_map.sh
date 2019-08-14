#@desc Checking that we can map streamline end-points
${TRACTOR} trkmap $TRACTOR_TEST_DATA/streamlines/wm2gm $TRACTOR_TEST_DATA/session@FA Scope:ends
${TRACTOR} value wm2gm_ends 51 42 39
