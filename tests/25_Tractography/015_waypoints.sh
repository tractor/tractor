#@desc Checking that tractography with waypoint and termination masks works
${TRACTOR} mkroi data/session 50 59 33 PointType:R Width:1 ROIName:tmp/seedregion
${TRACTOR} mkroi data/session 42 66 32 PointType:R Width:3 ROIName:tmp/wpregion1
${TRACTOR} mkroi data/session 58 66 33 PointType:R Width:3 ROIName:tmp/wpregion2
${TRACTOR} mtrack data/session SeedMaskFile:tmp/seedregion WaypointMaskFiles:tmp/wpregion1,tmp/wpregion2 TerminationMaskFiles:tmp/wpregion1,tmp/wpregion2 NumberOfSamples:50 TractName:tmp/tract CreateVolumes:true
rm -f tmp/tract* tmp/*region*
