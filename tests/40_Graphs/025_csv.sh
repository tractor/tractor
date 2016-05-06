#@desc Checking that we can read and write graphs in CSV format
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph2csv tmp/graph
rm -f tmp/graph.Rdata
${TRACTOR} graph-props tmp/graph.csv
rm -f tmp/graph.csv
