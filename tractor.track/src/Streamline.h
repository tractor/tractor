#ifndef _STREAMLINE_H_
#define _STREAMLINE_H_

#include <RcppArmadillo.h>

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
    // int seed;
    
    // Are points stored in voxel or world (typically mm) terms?
    Streamline::PointType pointType;
    
    // A boolean value indicating whether or not the points are equally spaced
    // (in real-world terms)
    bool fixedSpacing;
    
    // A set of integer labels associated with the streamline, indicating, for
    // example, the anatomical regions that the streamline passes through
    std::set<int> labels;
    
public:
    Streamline () {}
    Streamline (const std::vector<Space<3>::Point> &leftPoints, const std::vector<Space<3>::Point> &rightPoints, const Streamline::PointType pointType, const bool fixedSpacing)
        : leftPoints(leftPoints), rightPoints(rightPoints), pointType(pointType), fixedSpacing(fixedSpacing) {}
    
    int nPoints () const { return static_cast<int>(leftPoints.size() + rightPoints.size() - 1); }
    // int getSeed () const { return seed; }
    
    int nLabels () const                { return static_cast<int>(labels.size()); }
    bool addLabel (const int label)     { return labels.insert(label).second; }
    bool removeLabel (const int label)  { return (labels.erase(label) == 1); }
    void clearLabels ()                 { labels.clear(); }
    
    size_t concatenatePoints (arma::fmat &points)
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
            for (std::vector<Space<3>::Point>::reverse_iterator it=leftPoints.rbegin(); it!=leftPoints.rend()-1; it++)
            {
                points.row(index) = it->t();
                index++;
            }
        }
        
        if (rightPoints.size() > 0)
        {
            for (std::vector<Space<3>::Point>::iterator it=rightPoints.begin(); it!=rightPoints.end(); it++)
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
};

#endif
