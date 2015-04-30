#ifndef _STREAMLINE_H_
#define _STREAMLINE_H_

#include <RcppArmadillo.h>

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
    arma::fvec voxelDims
    
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
    Streamline (const std::vector<Space<3>::Point> &leftPoints, const std::vector<Space<3>::Point> &rightPoints, const Streamline::PointType pointType, const arma::fvec &voxelDims, const bool fixedSpacing)
        : leftPoints(leftPoints), rightPoints(rightPoints), pointType(pointType), voxelDims(voxelDims), fixedSpacing(fixedSpacing) {}
    
    size_t nPoints () const { return std::max(static_cast<size_t>(leftPoints.size()+rightPoints.size())-1, size_t(0)); }
    
    const std::vector<Space<3>::Point> & getLeftPoints() const { return leftPoints; }
    const std::vector<Space<3>::Point> & getRightPoints() const { return rightPoints; }
    
    double getLeftLength () const { return getLength(leftPoints); }
    double getRightLength () const { return getLength(rightPoints); }
    
    int nLabels () const                { return static_cast<int>(labels.size()); }
    bool addLabel (const int label)     { return labels.insert(label).second; }
    bool removeLabel (const int label)  { return (labels.erase(label) == 1); }
    void clearLabels ()                 { labels.clear(); }
    
    size_t concatenatePoints (arma::fmat &points) const;
};

class StreamlineMatrixDataSink : public DataSink<Streamline>
{
private:
    arma::fmat points;
    arma::uvec startIndices, seedIndices;
    size_t currentIndex, currentStart, nTotalPoints;
    
public:
    void setup (const size_type &count, const_iterator begin, const_iterator end)
    {
        currentIndex = 0;
        currentStart = 0;
        nTotalPoints = 0;
        for (const_iterator it=begin; it!=end; it++)
            nTotalPoints += it->nPoints();
        
        points.set_size(nTotalPoints, 3);
        startIndices.set_size(count);
        seedIndices.set_size(count);
    }
    
    void put (const Streamline &data)
    {
        arma::fmat currentPoints;
        size_t seedIndex = data.concatenatePoints(currentPoints);
        if (!currentPoints.empty())
        {
            startIndices(currentIndex) = currentStart;
            seedIndices(currentIndex) = currentStart + seedIndex;
            points(arma::span(currentStart,currentStart+currentPoints.n_rows-1), arma::span::all) = currentPoints;
            currentIndex++;
            currentStart += currentPoints.n_rows;
        }
    }
    
    const arma::fmat & getPoints () { return points; }
    const arma::uvec & getStartIndices () { return startIndices; }
    const arma::uvec & getSeedIndices () { return seedIndices; }
};

#endif
