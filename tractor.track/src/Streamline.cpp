#include <RcppArmadillo.h>

#include "Streamline.h"

double Streamline::getLength (const std::vector<Space<3>::Point> &points) const
{
    const size_t nPoints = points.size();
    
    if (points.size() < 2)
        return 0.0;
    else if (fixedSpacing)
    {
        if (pointType == VoxelPointType)
            return arma::norm((points[1] - points[0]) % voxelDims, 2) * (nPoints - 1);
        else
            return arma::norm(points[1] - points[0], 2) * (nPoints - 1);
    }
    else
    {
        double length = 0.0;
        if (pointType == VoxelPointType)
        {
            for (size_t i=1; i<nPoints; i++)
                length += arma::norm((points[i] - points[i-1]) % voxelDims, 2);
        }
        else
        {
            for (size_t i=1; i<nPoints; i++)
                length += arma::norm(points[i] - points[i-1], 2);
        }
        return length;
    }
}

size_t Streamline::concatenatePoints (arma::fmat &points) const
{
    int nPoints = this->nPoints();
    if (nPoints < 1)
    {
        points.reset();
        return 0;
    }
    else
        points.set_size(nPoints, 3);
    
    size_t index = 0;
    if (leftPoints.size() > 1)
    {
        for (std::vector<Space<3>::Point>::const_reverse_iterator it=leftPoints.rbegin(); it!=leftPoints.rend()-1; it++)
        {
            points.row(index) = it->t();
            index++;
        }
    }
    
    if (rightPoints.size() > 0)
    {
        for (std::vector<Space<3>::Point>::const_iterator it=rightPoints.begin(); it!=rightPoints.end(); it++)
        {
            points.row(index) = it->t();
            index++;
        }
    }
    else
    {
        // The left side can't also have length zero, so grab the seed from there
        points.row(index) = leftPoints[0].t();
    }
    
    return std::max(static_cast<size_t>(leftPoints.size())-1, size_t(0));
}
