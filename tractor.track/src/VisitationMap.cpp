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

void VisitationMapDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
}

void VisitationMapDataSink::put (const Streamline &data)
{
    Array<bool> visited(values.getDimensions(), false);
    
    const std::vector<Space<3>::Point> &leftPoints = data.getLeftPoints();
    const std::vector<Space<3>::Point> &rightPoints = data.getRightPoints();
    
    std::vector<int> currentLoc(3);
    for (size_t i=0; i<leftPoints.size(); i++)
    {
        for (int j=0; j<3; j++)
            currentLoc[j] = static_cast<int>(round(leftPoints[i][j]));
        size_t index;
        values.flattenIndex(currentLoc, index);
        if (!visited[index])
        {
            visited[index] = true;
            values[index] += 1.0;
        }
    }
    for (size_t i=0; i<rightPoints.size(); i++)
    {
        for (int j=0; j<3; j++)
            currentLoc[j] = static_cast<int>(round(rightPoints[i][j]));
        size_t index;
        values.flattenIndex(currentLoc, index);
        if (!visited[index])
        {
            visited[index] = true;
            values[index] += 1.0;
        }
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
