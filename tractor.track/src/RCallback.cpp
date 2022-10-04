#include <Rcpp.h>

#include "RCallback.h"

using namespace Rcpp;

void RListDataSink::put (const Streamline &data)
{
    if (!data.hasImageSpace())
        throw std::runtime_error("Streamline has no image space information");
    
    const std::vector<ImageSpace::Point> points = data.getPoints();
    
    NumericMatrix pointsR(points.size(), 3);
    const int seedIndexR = static_cast<int>(data.getSeedIndex()) + 1;
    
    const PointType pointType = data.getPointType();
    
    for (size_t i=0; i<points.size(); i++)
    {
        pointsR(i,0) = points[i][0] + (pointType == PointType::Voxel ? 1.0 : 0.0);
        pointsR(i,1) = points[i][1] + (pointType == PointType::Voxel ? 1.0 : 0.0);
        pointsR(i,2) = points[i][2] + (pointType == PointType::Voxel ? 1.0 : 0.0);
    }
    
    const std::string unit = (pointType == PointType::Voxel ? "vox" : "mm");
    
    Language call(constructor, _["line"]=pointsR, _["seedIndex"]=seedIndexR, _["voxelDims"]=data.imageSpace()->pixdim, _["coordUnit"]=unit);
    list[currentStreamline] = call.eval();
    currentStreamline++;
}

void LabelProfileDataSink::put (const Streamline &data)
{
    const std::set<int> &labels = data.getLabels();
    for (auto it=labels.cbegin(); it!=labels.cend(); it++)
    {
        if (counts.count(*it) == 0)
            counts[*it] = 1;
        else
            counts[*it]++;
    }
}

void LabelProfileDataSink::done ()
{
    std::vector<std::string> labels;
    std::vector<int> labelCounts;
    
    for (auto it=counts.cbegin(); it!=counts.cend(); it++)
    {
        if (dictionary.count(it->first) == 1)
            labels.push_back(dictionary[it->first]);
        else
            labels.push_back(std::to_string(it->first));
        
        if (it->second > std::numeric_limits<int>::max())
            Rf_warning("Streamline hit count for label %s is too large", labels.back().c_str());
        labelCounts.push_back(static_cast<int>(it->second));
    }
    
    profile = labelCounts;
    profile.attr("names") = labels;
}
