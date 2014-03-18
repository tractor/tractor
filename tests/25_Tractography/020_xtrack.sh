#@desc Checking tractography between parcellated regions
${TRACTOR} xtrack data/session SeedRegions:precentral_gyrus_left TargetRegions:precentral_gyrus_right NumberOfSamples:1 TractName:tmp/tract
rm -f tmp/tract_streamlines.Rdata
