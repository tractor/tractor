#@desc Checking that we can read and write graphs in CSV format
#@data data/graph/diffusion.Rdata
${TRACTOR} graph2csv diffusion
rm -f diffusion.Rdata
${TRACTOR} graph-props diffusion.csv
