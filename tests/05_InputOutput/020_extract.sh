#@desc Checking region extraction
${TRACTOR} extract data/session-12dir/tractor/diffusion/maskedb0 225 RegionName:tmp/region
${TRACTOR} which tmp/region eq 1
rm -f tmp/region.*
