#ifndef _STREAMLINE_H_
#define _STREAMLINE_H_

#include <RcppEigen.h>

#include "Space.h"
#include "DataSource.h"

class Streamline
{
public:
    enum PointType { VoxelPointType, WorldPointType };
    
private:
    // A list of points along the streamline; the path is considered
    // piecewise linear in between. Not a matrix since size isn't known
    // in advance. 
    std::vector<Space<3>::Point> leftPoints;
    std::vector<Space<3>::Point> rightPoints;
    
    // Are points stored in voxel or world (typically mm) terms?
    Streamline::PointType pointType;
    
    // Voxel dimensions, needed for converting between voxel and world point types
    Eigen::ArrayXf voxelDims;
    
    // A set of integer labels associated with the streamline, indicating, for
    // example, the anatomical regions that the streamline passes through
    std::set<int> labels;
    
protected:
    // A boolean value indicating whether or not the points are equally spaced
    // (in real-world terms)
    bool fixedSpacing;
    
    double getLength (const std::vector<Space<3>::Point> &points) const;
    
public:
    Streamline () {}
    Streamline (const std::vector<Space<3>::Point> &leftPoints, const std::vector<Space<3>::Point> &rightPoints, const Streamline::PointType pointType, const Eigen::VectorXf &voxelDims, const bool fixedSpacing)
        : leftPoints(leftPoints), rightPoints(rightPoints), pointType(pointType), voxelDims(voxelDims), fixedSpacing(fixedSpacing) {}
    
    size_t nPoints () const { return std::max(static_cast<size_t>(leftPoints.size()+rightPoints.size())-1, size_t(0)); }
    size_t getSeedIndex () const { return std::max(static_cast<size_t>(leftPoints.size())-1, size_t(0)); }
    
    const std::vector<Space<3>::Point> & getLeftPoints () const { return leftPoints; }
    const std::vector<Space<3>::Point> & getRightPoints () const { return rightPoints; }
    Streamline::PointType getPointType () const { return pointType; }
    bool usesFixedSpacing () const { return fixedSpacing; }
    
    const Eigen::ArrayXf & getVoxelDimensions () const { return voxelDims; }
    
    double getLeftLength () const  { return getLength(leftPoints); }
    double getRightLength () const { return getLength(rightPoints); }
    
    int nLabels () const                            { return static_cast<int>(labels.size()); }
    bool addLabel (const int label)                 { return labels.insert(label).second; }
    bool removeLabel (const int label)              { return (labels.erase(label) == 1); }
    const std::set<int> & getLabels () const        { return labels; }
    void setLabels (const std::set<int> &labels)    { this->labels = labels; }
    void clearLabels ()                             { labels.clear(); }
    
    size_t concatenatePoints (Eigen::ArrayX3f &points) const;
};

class StreamlineMatrixDataSink : public DataSink<Streamline>
{
private:
    Eigen::ArrayX3f points;
    Eigen::Array<unsigned int,Eigen::Dynamic,1> startIndices, seedIndices;
    size_t currentIndex, currentStart, nTotalPoints;
    
public:
    void setup (const size_type &count, const_iterator begin, const_iterator end)
    {
        currentIndex = 0;
        currentStart = 0;
        nTotalPoints = 0;
        for (const_iterator it=begin; it!=end; it++)
            nTotalPoints += it->nPoints();
        
        points.resize(nTotalPoints, 3);
        startIndices.resize(count);
        seedIndices.resize(count);
    }
    
    void put (const Streamline &data)
    {
        Eigen::ArrayX3f currentPoints;
        size_t seedIndex = data.concatenatePoints(currentPoints);
        if (currentPoints.rows() != 0)
        {
            startIndices(currentIndex) = currentStart;
            seedIndices(currentIndex) = currentStart + seedIndex;
            points.block(currentStart,0,currentPoints.rows(),3) = currentPoints;
            currentIndex++;
            currentStart += currentPoints.rows();
        }
    }
    
    const Eigen::ArrayX3f & getPoints () { return points; }
    const Eigen::Array<unsigned int,Eigen::Dynamic,1> & getStartIndices () { return startIndices; }
    const Eigen::Array<unsigned int,Eigen::Dynamic,1> & getSeedIndices () { return seedIndices; }
};

#endif
