#@desc Checking that we can run tractography
${TRACTOR} track data/session 50 59 33 Streamlines:50 TractName:tmp/tract RequireMap:true
${TRACTOR} value tmp/tract 50 59 33
rm -f tmp/tract*
