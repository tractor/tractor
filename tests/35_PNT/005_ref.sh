#@desc Checking that we can create a PNT reference tract
${TRACTOR} pnt-ref data/session 50 59 33 PointType:R NumberOfSamples:50 TractName:tmp/tract
${TRACTOR} peek tmp/tract_ref | grep -v session | grep -v spacing | sed '/seed/s/\.[0-9]*//g'
rm -f tmp/tract_ref.Rdata
