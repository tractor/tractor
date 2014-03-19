#@desc Checking graph decomposition
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph-reweight weightedFA GraphName:tmp/graph
${TRACTOR} graph-decompose GraphName:tmp/graph Method:modularity EdgeWeightThreshold:0.3
rm -f tmp/graph*
