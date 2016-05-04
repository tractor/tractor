#@desc Checking tractography between parcellated regions
${TRACTOR} track data/session precentral_gyrus_left TargetRegions:precentral_gyrus_right Streamlines:10 TractName:tmp/tract RequirePaths:true
rm -f tmp/tract*
