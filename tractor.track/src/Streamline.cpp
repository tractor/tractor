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
            if (pointType == PointType::Voxel)
            {
                if (!this->hasImageSpace())
                    throw std::runtime_error("Streamline has no image space information");
                for (int j=0; j<3; j++)
                    step[j] *= space->pixdim[j];
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
        if (pointType == PointType::Voxel)
        {
            if (!this->hasImageSpace())
                throw std::runtime_error("Streamline has no image space information");
            for (int j=0; j<3; j++)
                step[j] *= space->pixdim[j];
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

bool StreamlineLabeller::process (Streamline &data)
{
    // The new labels replace any old ones
    data.clearLabels();
    
    const PointType pointType = data.getPointType();
    
    for (const ImageSpace::Point &point : data.getLeftPoints())
    {
        const int &value = labelMap.at(point, pointType);
        if (value > 0)
            data.addLabel(value);
    }
    
    for (const ImageSpace::Point &point : data.getRightPoints())
    {
        const int &value = labelMap.at(point, pointType);
        if (value > 0)
            data.addLabel(value);
    }
    
    return true;
}

void StreamlineLabelMatcher::process (const std::set<int> &hits, const size_t &index)
{
    bool isMatch = (combine == CombineOperation::And);
    for (size_t i=0; i<labels.size(); i++)
    {
        if (combine == CombineOperation::None)
        {
            if (hits.count(labels[i]) == 1)
                matches[i].push_back(index);
        }
        else if (combine == CombineOperation::And)
        {
            isMatch = isMatch && hits.count(labels[i]) == 1;
            if (!isMatch)
                break;
        }
        else if (combine == CombineOperation::Or)
        {
            isMatch = isMatch || hits.count(labels[i]) == 1;
            if (isMatch)
                break;
        }
    }
    
    // The second test here is unnecessary, because isMatch will never be true
    // when no combination is performed, but it makes this explicit
    if (isMatch && combine != CombineOperation::None)
        matches[0].push_back(index);
}
