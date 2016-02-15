#@desc Checking that we can create an HNT reference tract
${TRACTOR} hnt-ref data/session 50 59 33 PointType:R Streamlines:50 TractName:tmp/tract
${TRACTOR} peek tmp/tract_ref | grep -v session | sed 's/\.[0-9][0-9]*//g'
rm -f tmp/tract_ref.Rdata
