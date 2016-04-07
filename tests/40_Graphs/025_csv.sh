#@desc Checking that we can read and write graphs in CSV format
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph2csv GraphName:tmp/graph
${TRACTOR} graph-props tmp/graph.csv
rm -f tmp/graph.*
