#ifndef _STREAMLINE_H_
#define _STREAMLINE_H_

#include "Space.h"

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
    
    // The index (row number) of the seed point for the streamline
    int seed;
    
    // Are points stored in voxel or world (typically mm) terms?
    Streamline::PointType pointType;
    
    // A boolean value indicating whether or not the points are equally spaced
    // (in real-world terms)
    bool fixedSpacing;
    
    // A set of integer labels associated with the streamline, indicating, for
    // example, the anatomical regions that the streamline terminates in
    std::set<int> labels;
    
public:
    Streamline () {}
    Streamline (const std::vector<Space<3>::Point> &leftPoints, const std::vector<Space<3>::Point> &rightPoints, const int seed, const Streamline::PointType pointType, const bool fixedSpacing)
        : leftPoints(leftPoints), rightPoints(rightPoints), seed(seed), pointType(pointType), fixedSpacing(fixedSpacing) {}
    
    int nPoints () const { return static_cast<int>(leftPoints.size() + rightPoints.size()); }
    int getSeed () const { return seed; }
    
    int nLabels () const                { return static_cast<int>(labels.size()); }
    bool addLabel (const int label)     { return labels.insert(label).second; }
    bool removeLabel (const int label)  { return (labels.erase(label) == 1); }
    void clearLabels ()                 { labels.clear(); }
};

#endif
