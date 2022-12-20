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
        if (!leftPoints.empty())
            checkAndSetPoint(visited, values, leftPoints.front());
        else if (!rightPoints.empty())
            checkAndSetPoint(visited, values, rightPoints.front());
        break;
        
        case MappingScope::Ends:
        if (!leftPoints.empty())
            checkAndSetPoint(visited, values, leftPoints.back());
        if (!rightPoints.empty())
            checkAndSetPoint(visited, values, rightPoints.back());
        break;
    }
}

void VisitationMapDataSink::done ()
{
    if (normalise)
    {
        std::transform(values.begin(), values.end(), values.begin(), [this](const double &x) {
            return x / static_cast<double>(totalStreamlines);
        });
    }
}
