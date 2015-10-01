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
            return (points[1] - points[0]).cwiseProduct(voxelDims).norm() * (nPoints - 1);
        else
            return (points[1] - points[0]).norm() * (nPoints - 1);
    }
    else
    {
        double length = 0.0;
        if (pointType == VoxelPointType)
        {
            for (size_t i=1; i<nPoints; i++)
                length += (points[i] - points[i-1]).cwiseProduct(voxelDims).norm();
        }
        else
        {
            for (size_t i=1; i<nPoints; i++)
                length += (points[i] - points[i-1]).norm();
        }
        return length;
    }
}

size_t Streamline::concatenatePoints (Eigen::MatrixX3f &points) const
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
    
    return std::max(static_cast<size_t>(leftPoints.size())-1, size_t(0));
}
