#include <Rcpp.h>

#include "Image.h"
#include "Streamline.h"
#include "VisitationMap.h"

inline void checkAndSetPoint (Image<bool,3> &visited, Image<double,3> &values, const ImageSpace::Point &point)
{
    // The code below would suffice, but this function is called a lot, so
    // we try to reduce duplicating work between the two images by flattening
    // separately once
    // if (!visited.at(point)) { visited.at(point) = true; values.at(point) += 1.0; }
    static ImageRaster<3>::ArrayIndex loc;
    static size_t index;
    
    for (int i=0; i<3; i++)
        loc[i] = static_cast<size_t>(round(point[i]));
    
    values.imageRaster().flattenIndex(loc, index);
    if (!visited[index])
    {
        visited[index] = true;
        values[index] += 1.0;
    }
}

void VisitationMapDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
}

void VisitationMapDataSink::put (const Streamline &data)
{
    Image<bool,3> visited(values.dim(), false);
    
    const std::vector<ImageSpace::Point> &leftPoints = data.getLeftPoints();
    const std::vector<ImageSpace::Point> &rightPoints = data.getRightPoints();
    
    switch (scope)
    {
        case MappingScope::All:
        for (size_t i=0; i<leftPoints.size(); i++)
            checkAndSetPoint(visited, values, leftPoints[i]);
        for (size_t i=0; i<rightPoints.size(); i++)
            checkAndSetPoint(visited, values, rightPoints[i]);
        break;
        
        case MappingScope::Seed:
        if (leftPoints.size() > 0)
            checkAndSetPoint(visited, values, leftPoints[0]);
        else if (rightPoints.size() > 0)
            checkAndSetPoint(visited, values, rightPoints[0]);
        break;
        
        case MappingScope::Ends:
        if (leftPoints.size() > 0)
        {
            size_t i = leftPoints.size() - 1;
            checkAndSetPoint(visited, values, leftPoints[i]);
        }
        if (rightPoints.size() > 0)
        {
            size_t i = rightPoints.size() - 1;
            checkAndSetPoint(visited, values, rightPoints[i]);
        }
        break;
    }
}

void VisitationMapDataSink::done ()
{
    if (normalise)
        std::transform(values.begin(), values.end(), values.begin(), [this](const double &x) { return x / static_cast<double>(totalStreamlines); });
}

void VisitationMapDataSink::writeToNifti (const std::string &fileName, ImageSpace *space)
{
    if (space != nullptr)
        values.setImageSpace(space, true);
    values.toNifti(DT_FLOAT64).toFile(fileName);
}
