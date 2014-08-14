#include <RcppArmadillo.h>

#include "Streamline.h"

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
    
    return std::max(static_cast<int>(leftPoints.size())-1, 0);
}
