#include "RCallback.h"

using namespace Rcpp;

Rcpp::Reference RListDataSource::getElement (const size_t n)
{
    RObject object = list[n];
    if (!object.inherits("Streamline"))
        throw std::runtime_error("List element " + std::to_string(currentStreamline+1) + " is not a Streamline object");
    return Reference(object);
}

RListDataSource::RListDataSource (SEXP list)
    : list(list)
{
    totalStreamlines = static_cast<size_t>(this->list.size());
    if (totalStreamlines > 0)
    {
        Rcpp::Reference first = getElement(0);
        ImageSpace *space = new ImageSpace(as<ImageSpace::DimVector>(first.field("spaceDims")), as<ImageSpace::PixdimVector>(first.field("voxelDims")));
        this->space.reset(space);
    }
}

void RListDataSource::get (Streamline &data)
{
    Reference streamline = getElement(currentStreamline);
    
    NumericMatrix line = streamline.field("line");
    const int seedIndex = as<int>(streamline.field("seedIndex")) - 1;
    const PointType pointType = as<std::string>(streamline.field("coordUnit")) == "vox" ? PointType::Voxel : PointType::Scaled;
    
    ImageSpace::Point point;
    std::vector<ImageSpace::Point> leftPoints, rightPoints;
    for (int i=seedIndex; i>=0; i--)
    {
        point[0] = line(i,0) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[1] = line(i,1) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[2] = line(i,2) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        leftPoints.push_back(point);
    }
    for (int i=seedIndex; i<line.nrow(); i++)
    {
        point[0] = line(i,0) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[1] = line(i,1) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[2] = line(i,2) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        rightPoints.push_back(point);
    }
    
    data = Streamline(leftPoints, rightPoints, pointType, space, false);
    currentStreamline++;
}

void RListDataSink::setup (const size_t &count)
{
    totalStreamlines += count;
    List tempList = list;
    list = List(totalStreamlines);
    std::copy(tempList.begin(), tempList.end(), list.begin());
}

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
    
    // FIXME: Strictly, the R class expects PointType::Scaled or PointType::Voxel only
    const std::string unit = (pointType == PointType::Voxel ? "vox" : "mm");
    
    Language call(constructor, _["line"]=pointsR, _["seedIndex"]=seedIndexR, _["spaceDims"]=data.imageSpace()->dim, _["voxelDims"]=data.imageSpace()->pixdim, _["coordUnit"]=unit);
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
