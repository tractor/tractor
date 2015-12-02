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
    bool hasLabel (const int label)                 { return (labels.count(label) == 1); }
    const std::set<int> & getLabels () const        { return labels; }
    void setLabels (const std::set<int> &labels)    { this->labels = labels; }
    void clearLabels ()                             { labels.clear(); }
    
    size_t concatenatePoints (Eigen::ArrayX3f &points) const;
};

class StreamlineLengthsDataSink : public DataSink<Streamline>
{
private:
    std::vector<double> lengths;
    
public:
    void put (const Streamline &data)
    {
        lengths.push_back(data.getLeftLength() + data.getRightLength());
    }
    
    const std::vector<double> & getLengths () { return lengths; }
};

#endif
