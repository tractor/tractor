#@desc Checking that we can reweight a graph
#@data data/graph/diffusion.Rdata
${TRACTOR} graph-reweight diffusion weightedFA
${TRACTOR} graph-props diffusion EdgeWeightThreshold:0.3
echo "---"
${TRACTOR} graph-props diffusion Binarise:false
echo "---"
${TRACTOR} graph-props diffusion Binarise:false Normalise:true
echo "---"
${TRACTOR} graph-props diffusion Binarise:false WeightedClusteringCoefficient:barratt
