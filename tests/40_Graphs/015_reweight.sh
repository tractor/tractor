#@desc Checking that we can reweight a graph
cp data/graph/diffusion.Rdata tmp/graph.Rdata
${TRACTOR} graph-reweight weightedFA GraphName:tmp/graph
${TRACTOR} graph-props GraphName:tmp/graph EdgeWeightThreshold:0.3
echo "---"
${TRACTOR} graph-props GraphName:tmp/graph Binarise:false
rm -f tmp/graph.Rdata
