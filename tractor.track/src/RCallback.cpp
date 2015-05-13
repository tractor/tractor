#include <RcppArmadillo.h>

#include "RCallback.h"

void RCallbackDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    currentIndex = 0;
    currentStart = 0;
    nTotalPoints = 0;
    for (const_iterator it=begin; it!=end; it++)
        nTotalPoints += it->nPoints();
    
    points.set_size(nTotalPoints, 3);
    startIndices.set_size(count);
    seedIndices.set_size(count);
}

void RCallbackDataSink::put (const Streamline &data)
{
    arma::fmat currentPoints;
    size_t seedIndex = data.concatenatePoints(currentPoints);
    if (!currentPoints.is_empty())
    {
        startIndices(currentIndex) = currentStart;
        seedIndices(currentIndex) = currentStart + seedIndex;
        points(arma::span(currentStart,currentStart+currentPoints.n_rows-1), arma::span::all) = currentPoints;
        currentIndex++;
        currentStart += currentPoints.n_rows;
    }
}

void RCallbackDataSink::finish ()
{
    SEXP pointsR = Rcpp::wrap(points + 1.0);
    SEXP startIndicesR = Rcpp::wrap(startIndices + 1);
    SEXP seedIndicesR = Rcpp::wrap(seedIndices + 1);
    function(pointsR, startIndicesR, seedIndicesR);
}

void ProfileMatrixDataSink::put (const Streamline &data)
{
    const std::set<int> &labels = data.getLabels();
    for (std::set<int>::iterator it=labels.begin(); it!=labels.end(); it++)
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
