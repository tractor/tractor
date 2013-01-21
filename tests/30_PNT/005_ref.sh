#@desc Checking that we can create a PNT reference tract
${TRACTOR} pnt-ref data/session-12dir 49 58 14 PointType:R NumberOfSamples:50 TractName:tmp/tract Tracker:fsl
${TRACTOR} peek tmp/tract_ref | grep -v session-12dir | grep -v spacing | sed '/seed/s/\.[0-9]*//g'
rm -f tmp/tract_ref.Rdata
