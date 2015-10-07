#include <RcppEigen.h>

#include "RCallback.h"

void RCallbackDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    currentIndex = 0;
    currentStart = 0;
    nTotalPoints = 0;
    for (const_iterator it=begin; it!=end; it++)
        nTotalPoints += it->nPoints();
    
    points.resize(nTotalPoints, Eigen::NoChange);
    startIndices.resize(count);
    seedIndices.resize(count);
}

void RCallbackDataSink::put (const Streamline &data)
{
    Eigen::ArrayX3f currentPoints;
    size_t seedIndex = data.concatenatePoints(currentPoints);
    if (currentPoints.rows() != 0)
    {
        startIndices(currentIndex) = currentStart;
        seedIndices(currentIndex) = currentStart + seedIndex;
        points.block(currentStart,0,currentPoints.rows(),3) = currentPoints;
        currentIndex++;
        currentStart += currentPoints.rows();
    }
}

void RCallbackDataSink::finish ()
{
    Rcpp::NumericMatrix pointsR(points.rows(), 3);
    Rcpp::IntegerVector startIndicesR(startIndices.size());
    Rcpp::IntegerVector seedIndicesR(seedIndices.size());
    for (size_t i=0; i<points.rows(); i++)
    {
        pointsR(i,0) = points(i,0) + 1.0;
        pointsR(i,1) = points(i,1) + 1.0;
        pointsR(i,2) = points(i,2) + 1.0;
        startIndicesR[i] = static_cast<int>(startIndices[i]) + 1;
        seedIndicesR[i] = static_cast<int>(seedIndices[i]) + 1;
    }
    function(pointsR, startIndicesR, seedIndicesR);
}

void ProfileMatrixDataSink::put (const Streamline &data)
{
    const std::set<int> &labels = data.getLabels();
    for (std::set<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
    {
        if (counts.count(*it) == 0)
            counts[*it] = 1;
        else
            counts[*it]++;
    }
}

void ProfileMatrixDataSink::done ()
{
    std::vector<int> labels;
    std::vector<size_t> labelCounts;
    
    for (std::map<int,size_t>::const_iterator it=counts.begin(); it!=counts.end(); it++)
    {
        labels.push_back(it->first);
        labelCounts.push_back(it->second);
    }
    
    SEXP labelsR = Rcpp::wrap(labels);
    SEXP labelCountsR = Rcpp::wrap(labelCounts);
    
    function(labelsR, labelCountsR);
}
