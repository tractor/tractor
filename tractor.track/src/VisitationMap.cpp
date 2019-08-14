#include <RcppEigen.h>

#include "RNifti.h"
#include "Streamline.h"
#include "VisitationMap.h"

struct divider
{
    double divisor;
    
    divider (double divisor)
        : divisor(divisor) {}
    
    double operator() (double x) { return x/divisor; }
};

inline void checkAndSetPoint (Array<bool> &visited, Array<double> &values, const Space<3>::Point &point)
{
    static std::vector<int> loc(3);
    size_t index;
    
    for (int i=0; i<3; i++)
        loc[i] = static_cast<int>(round(point[i]));
    
    values.flattenIndex(loc, index);
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
    Array<bool> visited(values.getDimensions(), false);
    
    const std::vector<Space<3>::Point> &leftPoints = data.getLeftPoints();
    const std::vector<Space<3>::Point> &rightPoints = data.getRightPoints();
    
    switch (scope)
    {
        case FullMappingScope:
        for (size_t i=0; i<leftPoints.size(); i++)
            checkAndSetPoint(visited, values, leftPoints[i]);
        for (size_t i=0; i<rightPoints.size(); i++)
            checkAndSetPoint(visited, values, rightPoints[i]);
        break;
        
        case SeedMappingScope:
        if (leftPoints.size() > 0)
            checkAndSetPoint(visited, values, leftPoints[0]);
        else if (rightPoints.size() > 0)
            checkAndSetPoint(visited, values, rightPoints[0]);
        break;
        
        case EndsMappingScope:
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
        std::transform(values.begin(), values.end(), values.begin(), divider(static_cast<double>(totalStreamlines)));
}

void VisitationMapDataSink::writeToNifti (const RNifti::NiftiImage &reference, const std::string &fileName) const
{
    RNifti::NiftiImage image = reference;
    image.replaceData(values.getData(), DT_FLOAT64);
    image.toFile(fileName);
}
