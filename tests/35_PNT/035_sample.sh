#@desc Checking that we can sample from a PNT model
export TRACTOR_REFTRACT_SET=miua2017
${TRACTOR} pnt-sample TractName:genu Samples:50
${TRACTOR} -v1 trkinfo genu | grep -v source
