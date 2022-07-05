#include <Rcpp.h>

#include "Streamline.h"

double Streamline::getLength (const std::vector<ImageSpace::Point> &points) const
{
    const size_t nPoints = points.size();
    
    if (points.size() < 2)
        return 0.0;
    else
    {
        double length = 0.0;
        for (size_t i=1; i<nPoints; i++)
        {
            ImageSpace::Vector step = ImageSpace::step(points[i-1], points[i]);
            if (pointType == ImageSpace::VoxelPointType)
            {
                for (int j=0; j<3; j++)
                    step[j] *= voxelDims[j];
            }
            
            // If the spacing is consistent we only need to measure one step,
            // so we can abandon the loop and return
            if (fixedSpacing)
                return static_cast<double>(ImageSpace::norm(step)) * (nPoints - 1);
            else
                length += static_cast<double>(ImageSpace::norm(step));
        }
        
        return length;
    }
}

void Streamline::trim (std::vector<ImageSpace::Point> &points, const double maxLength)
{
    const size_t nPoints = points.size();
    
    double length = 0.0;
    for (size_t i=1; i<nPoints; i++)
    {
        ImageSpace::Vector step = ImageSpace::step(points[i-1], points[i]);
        if (pointType == ImageSpace::VoxelPointType)
        {
            for (int j=0; j<3; j++)
                step[j] *= voxelDims[j];
        }
        
        if (fixedSpacing)
        {
            const int maxSteps = static_cast<int>(floor(maxLength / ImageSpace::norm(step)));
            if (nPoints > (maxSteps + 1))
                points.erase(points.begin() + maxSteps + 1, points.end());
            break;      // regardless, because this is a one-off operation
        }
        else
        {
            length += static_cast<double>(ImageSpace::norm(step));
            if (length > maxLength)
            {
                points.erase(points.begin() + i, points.end());
                break;
            }
        }
    }
}

std::vector<ImageSpace::Point> Streamline::getPoints () const
{
    std::vector<ImageSpace::Point> result;
    
    const size_t nPoints = this->nPoints();
    if (nPoints < 1)
        return result;
    else
        result.resize(nPoints);
    
    size_t index = 0;
    if (leftPoints.size() > 1)
    {
        for (auto it=leftPoints.crbegin(); it!=leftPoints.crend()-1; it++)
        {
            result[index] = *it;
            index++;
        }
    }
    
    if (rightPoints.size() > 0)
    {
        for (auto it=rightPoints.cbegin(); it!=rightPoints.cend(); it++)
        {
            result[index] = *it;
            index++;
        }
    }
    else
    {
        // The left side can't also have length zero, so grab the seed from there
        result[index] = leftPoints[0];
    }
    
    return result;
}
