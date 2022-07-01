#include <Rcpp.h>

#include "Filter.h"
#include "Image.h"

// Note that this function will modify its first argument, as long as it is
// passed by reference
template <typename ElementType>
static ElementType & getNthElement (std::vector<ElementType> &vec, size_t n)
{
    std::nth_element(vec.begin(), vec.begin()+n, vec.end());
    return *(vec.begin() + n);
}

// This is a many-to-one filter, so it rejects every streamline except the last
// The final streamline passed to it is replaced with the median
bool MedianStreamlineFilter::process (Streamline &data)
{
    // Cache the streamline
    cache[current] = data;
    current++;
    
    // For every streamline except the last, we simply cache the data and drop
    // the element from the pipeline's working set
    if (current < count)
        return false;
    
    // Lengths are in steps here
    std::vector<size_t> leftLengths(count), rightLengths(count);
    const ImageSpace::PointType pointType = cache[0].getPointType();
    
    // First pass: find lengths
    for (size_t i=0; i<count; i++)
    {
        leftLengths[i] = cache[i].getLeftPoints().size();
        rightLengths[i] = cache[i].getRightPoints().size();
        
        if (cache[i].getPointType() != pointType)
            throw std::runtime_error("Point types do not match across streamlines, so median will make no sense");
    }
    
    const size_t lengthIndex = static_cast<size_t>(floor((count-1) * quantile));
    const size_t leftLength = getNthElement(leftLengths, lengthIndex);
    const size_t rightLength = getNthElement(rightLengths, lengthIndex);
    
    // Second pass: left points
    std::vector<ImageSpace::Point> leftPoints(leftLength);
    for (size_t j=0; j<leftLength; j++)
    {
        std::vector<float> x, y, z;
        
        for (size_t i=0; i<count; i++)
        {
            // Skip over this streamline if it is too short
            if (leftLengths[i] > j)
            {
                const ImageSpace::Point point = cache[i].getLeftPoints()[j];
                x.push_back(point[0]);
                y.push_back(point[1]);
                z.push_back(point[2]);
            }
        }
        
        const size_t medianIndex = static_cast<size_t>(round(x.size() / 2.0));
        leftPoints[j][0] = getNthElement(x, medianIndex);
        leftPoints[j][1] = getNthElement(y, medianIndex);
        leftPoints[j][2] = getNthElement(z, medianIndex);
    }
    
    // Third pass: right points
    std::vector<ImageSpace::Point> rightPoints(rightLength);
    for (size_t j=0; j<rightLength; j++)
    {
        std::vector<float> x, y, z;
        
        for (size_t i=0; i<count; i++)
        {
            // Skip over this streamline if it is too short
            if (rightLengths[i] > j)
            {
                const ImageSpace::Point point = cache[i].getRightPoints()[j];
                x.push_back(point[0]);
                y.push_back(point[1]);
                z.push_back(point[2]);
            }
        }
        
        const size_t medianIndex = static_cast<size_t>(round(x.size() / 2.0));
        rightPoints[j][0] = getNthElement(x, medianIndex);
        rightPoints[j][1] = getNthElement(y, medianIndex);
        rightPoints[j][2] = getNthElement(z, medianIndex);
    }
    
    // Replace the source data with the calculated median
    // Fixed spacing won't be preserved
    data = Streamline(leftPoints, rightPoints, pointType, data.imageSpace().pixdim, false);
}
