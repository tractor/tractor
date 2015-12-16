#include <Rcpp.h>

#include "lemon/smart_graph.h"
#include "lemon/bfs.h"
#include "lemon/dijkstra.h"
#include "lemon/bellman_ford.h"

using namespace Rcpp;
using namespace lemon;

typedef SmartDigraph::NodeMap<int> IndexMap;
typedef SmartDigraph::ArcMap<double> WeightMap;

struct RGraph
{
    SmartDigraph graph;
    IndexMap indices;
    WeightMap weights;
    bool directed;
    bool weighted;
    bool negativeWeights;
    
    RGraph ()
        : indices(graph), weights(graph), directed(false), weighted(false), negativeWeights(false) {}
};

template <class Algorithm>
void searchPaths (const RGraph &g, Algorithm &algorithm, NumericMatrix &result)
{
    for (SmartDigraph::NodeIt source(g.graph); source != INVALID; ++source)
    {
        algorithm.run(source);
        for (SmartDigraph::NodeIt target(g.graph); target != INVALID; ++target)
        {
            if (!algorithm.reached(target))
                result(g.indices[source], g.indices[target]) = R_PosInf;
            else
                result(g.indices[source], g.indices[target]) = algorithm.dist(target);
        }
    }
}

void createGraph (const int nVertices, const IntegerMatrix &edges, const NumericVector &weights, const bool directed, RGraph &g)
{
    g.directed = directed;
    double nonzeroValue = NA_REAL;
    
    for (int i=0; i<weights.size(); i++)
    {
        const double weight = weights[i];
        if (!ISNA(weight) && weight != 0.0)
        {
            if (weight < 0.0)
                g.negativeWeights = true;
            
            // The graph is classified as weighted if there is more than one unique nonzero weight
            if (ISNA(nonzeroValue))
                nonzeroValue = weight;
            else if (weight != nonzeroValue)
                g.weighted = true;
            
            // No need to keep looking
            if (g.weighted && g.negativeWeights)
                break;
        }
    }
    
    std::vector<SmartDigraph::Node> nodes(nVertices);
    for (int i=0; i<nVertices; i++)
    {
        SmartDigraph::Node node = g.graph.addNode();
        nodes[i] = node;
        g.indices[node] = i;
    }
    
    for (int i=0; i<edges.nrow(); i++)
    {
        const double weight = weights[i];
        if (ISNA(weight) || weight == 0.0)
            continue;
        
        SmartDigraph::Arc arc = g.graph.addArc(nodes[edges(i,0)-1], nodes[edges(i,1)-1]);
        if (g.weighted)
            g.weights[arc] = weight;
        
        if (!g.directed)
        {
            SmartDigraph::Arc arc = g.graph.addArc(nodes[edges(i,1)-1], nodes[edges(i,0)-1]);
            if (g.weighted)
                g.weights[arc] = weight;
        }
    }
}

RcppExport SEXP shortestPaths (SEXP _nVertices, SEXP _edges, SEXP _weights, SEXP _directed)
{
BEGIN_RCPP
    const int nVertices = as<int>(_nVertices);
    RGraph g;
    createGraph(nVertices, IntegerMatrix(_edges), NumericVector(_weights), as<bool>(_directed), g);
    
    NumericMatrix result(nVertices, nVertices);
    if (!g.weighted)
    {
        Bfs<SmartDigraph> bfs(g.graph);
        searchPaths(g, bfs, result);
    }
    else if (!g.negativeWeights)
    {
        Dijkstra<SmartDigraph,WeightMap> dijkstra(g.graph, g.weights);
        searchPaths(g, dijkstra, result);
    }
    else
    {
        BellmanFord<SmartDigraph,WeightMap> bellmanFord(g.graph, g.weights);
        searchPaths(g, bellmanFord, result);
    }
    
    return result;
END_RCPP
}
