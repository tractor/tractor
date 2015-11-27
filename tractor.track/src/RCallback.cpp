#include <RcppEigen.h>

#include "RCallback.h"

void RCallbackDataSink::put (const Streamline &data)
{
    Eigen::ArrayX3f points;
    size_t seedIndex = data.concatenatePoints(points);
    
    Rcpp::NumericMatrix pointsR(points.rows(), 3);
    int seedIndexR = static_cast<int>(seedIndex) + 1;
    
    const Streamline::PointType pointType = data.getPointType();
    
    for (size_t i=0; i<points.rows(); i++)
    {
        pointsR(i,0) = points(i,0) + (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        pointsR(i,1) = points(i,1) + (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        pointsR(i,2) = points(i,2) + (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
    }
    
    const std::string unit = (pointType == Streamline::VoxelPointType ? "vox" : "mm");
    
    function(pointsR, seedIndexR, data.getVoxelDimensions(), unit);
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
