#include <RcppEigen.h>

#include "Streamline.h"

double Streamline::getLength (const std::vector<Space<3>::Point> &points) const
{
    const size_t nPoints = points.size();
    
    if (points.size() < 2)
        return 0.0;
    else if (fixedSpacing)
    {
        if (pointType == VoxelPointType)
            return ((points[1] - points[0]) * voxelDims).matrix().norm() * (nPoints - 1);
        else
            return (points[1] - points[0]).matrix().norm() * (nPoints - 1);
    }
    else
    {
        double length = 0.0;
        if (pointType == VoxelPointType)
        {
            for (size_t i=1; i<nPoints; i++)
                length += ((points[i] - points[i-1]) * voxelDims).matrix().norm();
        }
        else
        {
            for (size_t i=1; i<nPoints; i++)
                length += (points[i] - points[i-1]).matrix().norm();
        }
        return length;
    }
}

void Streamline::trim (std::vector<Space<3>::Point> &points, const double maxLength)
{
    const size_t nPoints = points.size();
    
    if (fixedSpacing)
    {
        double step;
        if (pointType == VoxelPointType)
            step = ((points[1] - points[0]) * voxelDims).matrix().norm();
        else
            step = (points[1] - points[0]).matrix().norm();
        
        const int maxSteps = static_cast<int>(floor(maxLength / step));
        if (nPoints > (maxSteps + 1))
            points.erase(points.begin() + maxSteps + 1, points.end());
    }
    else
    {
        double length = 0.0;
        for (size_t i=1; i<nPoints; i++)
        {
            if (pointType == VoxelPointType)
                length += ((points[i] - points[i-1]) * voxelDims).matrix().norm();
            else
                length += (points[i] - points[i-1]).matrix().norm();
            
            if (length > maxLength)
            {
                points.erase(points.begin() + i, points.end());
                break;
            }
        }
    }
}

size_t Streamline::concatenatePoints (Eigen::ArrayX3f &points) const
{
    int nPoints = this->nPoints();
    if (nPoints < 1)
    {
        points.resize(0, 0);
        return 0;
    }
    else
        points.resize(nPoints, 3);
    
    size_t index = 0;
    if (leftPoints.size() > 1)
    {
        for (std::vector<Space<3>::Point>::const_reverse_iterator it=leftPoints.rbegin(); it!=leftPoints.rend()-1; it++)
        {
            points.row(index) = *it;
            index++;
        }
    }
    
    if (rightPoints.size() > 0)
    {
        for (std::vector<Space<3>::Point>::const_iterator it=rightPoints.begin(); it!=rightPoints.end(); it++)
        {
            points.row(index) = *it;
            index++;
        }
    }
    else
    {
        // The left side can't also have length zero, so grab the seed from there
        points.row(index) = leftPoints[0];
    }
    
    return getSeedIndex();
}
