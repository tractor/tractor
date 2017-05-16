#@desc Checking that tractography with waypoint and termination masks works
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 50 59 33 Width:1 ROIName:seedregion
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 42 66 32 Width:3 ROIName:wpregion1
${TRACTOR} mkroi $TRACTOR_TEST_DATA/session@FA 58 66 33 Width:3 ROIName:wpregion2
${TRACTOR} track $TRACTOR_TEST_DATA/session seedregion TargetRegions:wpregion1,wpregion2 TerminateAtTargets:true MinTargetHits:2 Streamlines:50 RequireMap:true
