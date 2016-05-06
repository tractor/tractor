#@desc Checking that we can reweight a graph
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph-reweight tmp/graph weightedFA
${TRACTOR} graph-props tmp/graph EdgeWeightThreshold:0.3
echo "---"
${TRACTOR} graph-props tmp/graph Binarise:false
rm -f tmp/graph.Rdata
