#include <RcppArmadillo.h>

#include "NiftiImage.h"
#include "Streamline.h"
#include "VisitationMap.h"

struct divider
{
    double divisor;
    
    divider (double divisor)
        : divisor(divisor) {}
    
    double operator() (double x) { return x/divisor; }
};

struct conditionalIncrementer
{
    double operator() (const double x, const bool y) { return (y ? x+1.0 : x); }
};

void VisitationMapDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
}

void VisitationMapDataSink::put (const Streamline &data)
{
    if (data.hasVisitationMap())
    {
        Array<bool> *visited = data.getVisitationMap();
        std::transform(values.begin(), values.end(), visited->begin(), values.begin(), conditionalIncrementer());
    }
    else
    {
        Array<bool> visited(values.getDimensions(), 0);
    
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
                visited[index] = true;
        }
        for (size_t i=0; i<rightPoints.size(); i++)
        {
            for (int j=0; j<3; j++)
                currentLoc[j] = static_cast<int>(round(rightPoints[i][j]));
            size_t index;
            values.flattenIndex(currentLoc, index);
            if (!visited[index])
                visited[index] = true;
        }
    
        // Step through the two arrays, adding them elementwise
        std::transform(values.begin(), values.end(), visited.begin(), values.begin(), conditionalIncrementer());
    }
}

void VisitationMapDataSink::done ()
{
    if (normalise)
        std::transform(values.begin(), values.end(), values.begin(), divider(static_cast<double>(totalStreamlines)));
}

void VisitationMapDataSink::writeToNifti (const NiftiImage &reference, const std::string &fileName) const
{
    NiftiImage image = reference;
    image.setData(values);
    image.writeToFile(fileName);
}
