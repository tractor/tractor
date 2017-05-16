#@desc Checking tractography between parcellated regions
${TRACTOR} track $TRACTOR_TEST_DATA/session precentral_gyrus_left TargetRegions:precentral_gyrus_right Streamlines:10 RequirePaths:true
