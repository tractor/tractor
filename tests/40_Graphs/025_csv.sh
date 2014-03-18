#@desc Checking that we can export a graph to CSV format
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph2csv GraphName:tmp/graph
cat tmp/graph.csv
rm -f tmp/graph.*
