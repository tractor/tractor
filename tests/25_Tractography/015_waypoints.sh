#@desc Checking that tractography with waypoint and termination masks works
${TRACTOR} mkroi data/session@FA 50 59 33 Width:1 ROIName:tmp/seedregion
${TRACTOR} mkroi data/session@FA 42 66 32 Width:3 ROIName:tmp/wpregion1
${TRACTOR} mkroi data/session@FA 58 66 33 Width:3 ROIName:tmp/wpregion2
${TRACTOR} track data/session tmp/seedregion TargetRegions:tmp/wpregion1,tmp/wpregion2 TerminateAtTargets:true MinTargetHits:2 Streamlines:50 TractName:tmp/tract RequireMap:true
rm -f tmp/tract* tmp/*region*
