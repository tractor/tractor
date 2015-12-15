#include <Rcpp.h>

#include "lemon/smart_graph.h"
#include "lemon/bfs.h"
#include "lemon/dijkstra.h"
#include "lemon/bellman_ford.h"

using namespace Rcpp;
using namespace lemon;

template <class GraphType>
typename GraphType::Arc addEdge (GraphType &graph, const typename GraphType::Node &source, const typename GraphType::Node &target)
{
    return GraphType::Arc(INVALID);
}

template <>
SmartDigraph::Arc addEdge (SmartDigraph &graph, const SmartDigraph::Node &source, const SmartDigraph::Node &target)
{
    return graph.addArc(source, target);
}

template <>
SmartGraph::Arc addEdge (SmartGraph &graph, const SmartGraph::Node &source, const SmartGraph::Node &target)
{
    SmartGraph::Edge edge = graph.addEdge(source, target);
    return graph.direct(edge, true);
}

template <class GraphType, class Algorithm>
void searchPaths (const GraphType &graph, Algorithm &algorithm, std::map<typename GraphType::Node, int> &indices, NumericMatrix &result)
{
    for (typename GraphType::NodeIt source(graph); source != INVALID; ++source)
    {
        algorithm.run(source);
        for (typename GraphType::NodeIt target(graph); target != INVALID; ++target)
        {
            if (!algorithm.reached(target))
                result(indices[source], indices[target]) = R_PosInf;
            else
                result(indices[source], indices[target]) = algorithm.dist(target);
        }
    }
}

template <class GraphType>
NumericMatrix findShortestPaths (const int nVertices, const IntegerMatrix &edges, const NumericVector &weights)
{
    bool weighted = false, negativeWeights = false;
    double nonzeroValue = NA_REAL;
    
    for (int i=0; i<weights.size(); i++)
    {
        const double weight = weights[i];
        if (!ISNA(weight) && weight != 0.0)
        {
            if (weight < 0.0)
                negativeWeights = true;
            
            // The graph is classified as weighted if there is more than one unique nonzero weight
            if (ISNA(nonzeroValue))
                nonzeroValue = weight;
            else if (weight != nonzeroValue)
                weighted = true;
            
            // No need to keep looking
            if (weighted && negativeWeights)
                break;
        }
    }
    
    std::vector<typename GraphType::Node> nodes(nVertices);
    std::map<typename GraphType::Node, int> indices;
    GraphType graph;
    typedef typename GraphType::template ArcMap<double> WeightMap;
    WeightMap weightMap(graph);
    
    for (int i=0; i<nVertices; i++)
    {
        typename GraphType::Node node = graph.addNode();
        nodes[i] = node;
        indices[node] = i;
    }
    
    for (int i=0; i<edges.nrow(); i++)
    {
        const double weight = weights[i];
        if (ISNA(weight) || weight == 0.0)
            continue;
        
        typename GraphType::Arc arc = addEdge(graph, nodes[edges(i,0)-1], nodes[edges(i,1)-1]);
        if (weighted)
            weightMap[arc] = weight;
    }
    
    NumericMatrix result(nVertices, nVertices);
    if (!weighted)
    {
        Bfs<GraphType> bfs(graph);
        searchPaths(graph, bfs, indices, result);
    }
    else if (!negativeWeights)
    {
        Dijkstra<GraphType,WeightMap> dijkstra(graph, weightMap);
        searchPaths(graph, dijkstra, indices, result);
    }
    else
    {
        BellmanFord<GraphType,WeightMap> bellmanFord(graph, weightMap);
        searchPaths(graph, bellmanFord, indices, result);
    }
    
    return result;
}

RcppExport SEXP shortestPaths (SEXP _nVertices, SEXP _edges, SEXP _weights, SEXP _directed)
{
BEGIN_RCPP
    const bool directed = as<bool>(_directed);
    const int nVertices = as<int>(_nVertices);
    IntegerMatrix edges(_edges);
    NumericVector weights(_weights);
    
    NumericMatrix result;
    if (directed)
        result = findShortestPaths<SmartDigraph>(nVertices, edges, weights);
    else
        result = findShortestPaths<SmartGraph>(nVertices, edges, weights);
    
    return result;
END_RCPP
}
