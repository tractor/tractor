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
