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
    WeightMap distances;
    bool directed;
    bool weighted;
    bool negativeWeights;
    
    RGraph ()
        : indices(graph), weights(graph), distances(graph), directed(false), weighted(false), negativeWeights(false) {}
};

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
        {
            g.weights[arc] = weight;
            g.distances[arc] = 1.0 / weight;
        }
        
        if (!g.directed)
        {
            SmartDigraph::Arc arc = g.graph.addArc(nodes[edges(i,1)-1], nodes[edges(i,0)-1]);
            if (g.weighted)
            {
                g.weights[arc] = weight;
                g.distances[arc] = 1.0 / weight;
            }
        }
    }
}

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
        Dijkstra<SmartDigraph,WeightMap> dijkstra(g.graph, g.distances);
        searchPaths(g, dijkstra, result);
    }
    else
    {
        BellmanFord<SmartDigraph,WeightMap> bellmanFord(g.graph, g.distances);
        searchPaths(g, bellmanFord, result);
    }
    
    return result;
END_RCPP
}

RcppExport SEXP neighbourhoods (SEXP _nVertices, SEXP _edges, SEXP _weights, SEXP _directed, SEXP _vertices, SEXP _type)
{
BEGIN_RCPP
    IntegerVector vertices(_vertices);
    const std::string type = as<std::string>(_type);
    
    RGraph g;
    createGraph(as<int>(_nVertices), IntegerMatrix(_edges), NumericVector(_weights), as<bool>(_directed), g);
    
    List result(vertices.length());
    for (int i=0; i<vertices.length(); i++)
    {
        if (ISNA(vertices[i]))
        {
            result[i] = IntegerVector();
            continue;
        }
        
        SmartDigraph::Node targetNode;
        for (SmartDigraph::NodeIt node(g.graph); node != INVALID; ++node)
        {
            if (g.indices[node] == vertices[i] - 1)
                targetNode = node;
        }
        
        std::set<int> neighbours;
        if (type == "all" || type == "in")
        {
            for (SmartDigraph::InArcIt arc(g.graph,targetNode); arc != INVALID; ++arc)
                neighbours.insert(g.indices[g.graph.source(arc)] + 1);
        }
        if (type == "all" || type == "out")
        {
            for (SmartDigraph::OutArcIt arc(g.graph,targetNode); arc != INVALID; ++arc)
                neighbours.insert(g.indices[g.graph.target(arc)] + 1);
        }
        
        // Remove the target node itself, if present (open neighbourhood)
        neighbours.erase(vertices[i]);
        result[i] = wrap(neighbours);
    }
    
    return result;
END_RCPP
}

RcppExport SEXP clusteringCoefficients (SEXP _nVertices, SEXP _edges, SEXP _weights, SEXP _directed, SEXP _method)
{
BEGIN_RCPP
    const int nVertices = as<int>(_nVertices);
    const std::string method = as<std::string>(_method);
    
    RGraph g;
    createGraph(nVertices, IntegerMatrix(_edges), NumericVector(_weights), as<bool>(_directed), g);
    
    ArcLookUp<SmartDigraph> arcs(g.graph);
    NumericVector result(nVertices);
    for (SmartDigraph::NodeIt node(g.graph); node != INVALID; ++node)
    {
        // Find all neighbours of the current node
        std::set<SmartDigraph::Node> neighbours;
        for (SmartDigraph::InArcIt arc(g.graph,node); arc != INVALID; ++arc)
            neighbours.insert(g.graph.source(arc));
        for (SmartDigraph::OutArcIt arc(g.graph,node); arc != INVALID; ++arc)
            neighbours.insert(g.graph.target(arc));
        neighbours.erase(node);
        
        int nNeighbours = neighbours.size();
        if (nNeighbours < 2)
        {
            // By definition, the clustering coefficient is zero when there are no possible triangles
            result[g.indices[node]] = 0.0;
        }
        else
        {
            // Calculate the number of triangles around the node
            double strength = 0.0;
            double triangles = 0.0;
            for (std::set<SmartDigraph::Node>::const_iterator neighbour1=neighbours.begin(); neighbour1 != neighbours.end(); neighbour1++)
            {
                strength += g.weights[arcs(node,*neighbour1)];
                for (std::set<SmartDigraph::Node>::const_iterator neighbour2=neighbours.begin(); neighbour2 != neighbours.end(); neighbour2++)
                {
                    if (*neighbour1 == *neighbour2)
                        continue;
                    else if (!g.weighted && !g.directed)
                        triangles += static_cast<double>(arcs(*neighbour1,*neighbour2) != INVALID);
                    else if (!g.weighted && g.directed)
                        triangles += static_cast<double>(arcs(*neighbour1,*neighbour2) != INVALID) + static_cast<double>(arcs(*neighbour2,*neighbour1) != INVALID);
                    else if (g.weighted && g.directed)
                        throw std::domain_error("Clustering coefficient is not implemented for directed, weighted graphs");
                    else if (method == "onnela")
                    {
                        // Onnela method: weighted triangle value is the geometric mean of the weights of the edges
                        SmartDigraph::Arc arc = arcs(*neighbour1, *neighbour2);
                        if (arc != INVALID)
                            triangles += R_pow(g.weights[arc] * g.weights[arcs(node,*neighbour1)] * g.weights[arcs(node,*neighbour2)], 1.0/3.0);
                    }
                    else
                    {
                        // Barrat method: proportion of node strength associated with links to neighbours in triangles
                        triangles += static_cast<double>(arcs(*neighbour1,*neighbour2) != INVALID) * (g.weights[arcs(node,*neighbour1)] + g.weights[arcs(node,*neighbour2)]) / 2.0;
                    }
                }
            }
            
            if (g.weighted && method == "barrat")
                result[g.indices[node]] = triangles / (strength * static_cast<double>(nNeighbours - 1));
            else
                result[g.indices[node]] = (g.directed ? 0.5 : 1.0) * triangles / static_cast<double>(nNeighbours * (nNeighbours - 1));
        }
    }
    
    return result;
END_RCPP
}
