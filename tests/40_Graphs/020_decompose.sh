#@desc Checking graph decomposition
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph-reweight tmp/graph weightedFA
${TRACTOR} graph-decompose tmp/graph Method:modularity EdgeWeightThreshold:0.3
rm -f graph_decomposed* tmp/graph*
