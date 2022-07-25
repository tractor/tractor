#include <Rcpp.h>

#include "RCallback.h"

void RCallbackDataSink::put (const Streamline &data)
{
    const std::vector<ImageSpace::Point> points = data.getPoints();
    
    Rcpp::NumericMatrix pointsR(points.size(), 3);
    int seedIndexR = static_cast<int>(data.getSeedIndex()) + 1;
    
    const PointType pointType = data.getPointType();
    
    for (size_t i=0; i<points.size(); i++)
    {
        pointsR(i,0) = points[i][0] + (pointType == PointType::Voxel ? 1.0 : 0.0);
        pointsR(i,1) = points[i][1] + (pointType == PointType::Voxel ? 1.0 : 0.0);
        pointsR(i,2) = points[i][2] + (pointType == PointType::Voxel ? 1.0 : 0.0);
    }
    
    const std::string unit = (pointType == PointType::Voxel ? "vox" : "mm");
    
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
