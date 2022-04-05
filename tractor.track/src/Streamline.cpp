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

void StreamlineLabelList::read (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open((fileStem + ".trkl").c_str(), std::ios::binary);
    fileStream.seekg(0);
    if (binaryStream.readString(8).compare("TRKLABEL") != 0)
        throw std::runtime_error("Track label file does not seem to have a valid magic number");
    
    const int version = binaryStream.readValue<int32_t>();
    binaryStream.setEndianness(version > 0xffff ? "swapped" : "native");
    
    const int nStreamlines = binaryStream.readValue<int32_t>();
    const int nLabels = binaryStream.readValue<int32_t>();
    fileStream.seekg(32);
    
    for (int i=0; i<nLabels; i++)
    {
        binaryStream.readValue<int32_t>();
        binaryStream.readString();
    }
    
    labelList.clear();
    for (int j=0; j<nStreamlines; j++)
    {
        offsetList.push_back(static_cast<size_t>(binaryStream.readValue<uint64_t>()));
        const int currentCount = binaryStream.readValue<int32_t>();
        std::set<int> currentLabels;
        for (int i=0; i<currentCount; i++)
            currentLabels.insert(binaryStream.readValue<int32_t>());
        labelList.push_back(currentLabels);
    }
}

const std::vector<int> StreamlineLabelList::find (const std::vector<int> &labels)
{
    std::vector<int> indices;
    for (int i=0; i<labelList.size(); i++)
    {
        bool allPresent = true;
        for (std::vector<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
        {
            if (labelList[i].count(*it) == 0)
                allPresent = false;
        }
        
        if (allPresent)
            indices.push_back(i);
    }
    
    return indices;
}
