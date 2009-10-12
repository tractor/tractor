#@desc Checking that we can create an HNT reference tract
${TRACTOR} hnt-ref data/session-12dir 49 58 14 PointType:R NumberOfSamples:50 TractName:tmp/tract
${TRACTOR} peek tmp/tract_ref | grep -v session-12dir | sed 's/\.[0-9][0-9]//g'
rm -f tmp/tract_ref.Rdata
