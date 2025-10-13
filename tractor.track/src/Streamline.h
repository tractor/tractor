#ifndef _STREAMLINE_H_
#define _STREAMLINE_H_

#include "Image.h"
#include "DataSource.h"
#include "BinaryStream.h"

#include <Rcpp.h>

class Streamline : public ImageSpaceEmbedded
{
public:
    enum struct TerminationReason { Unknown, Bounds, Mask, OneWay, Target, NoData, Loop, Curvature };
    
private:
    // A list of points along the streamline; the path is considered
    // piecewise linear in between. Not a matrix since size isn't known
    // in advance. 
    std::vector<ImageSpace::Point> leftPoints;
    std::vector<ImageSpace::Point> rightPoints;
    
    // Are points stored in voxel or world (typically mm) terms?
    PointType pointType;
    
    // A set of integer labels associated with the streamline, indicating, for
    // example, the anatomical regions that the streamline passes through
    std::set<int> labels;
    
    // Reasons for termination on each side
    TerminationReason leftTerminationReason = TerminationReason::Unknown, rightTerminationReason = TerminationReason::Unknown;
    
protected:
    // A boolean value indicating whether or not the points are equally spaced
    // (in real-world terms)
    bool fixedSpacing;
    
    double getLength (const std::vector<ImageSpace::Point> &points) const;
    void trim (std::vector<ImageSpace::Point> &points, const double maxLength);
    
public:
    Streamline () {}
    
    Streamline (const ImageSpaceEmbedded &object) { space = object.imageSpace(); }
    
    Streamline (const std::vector<ImageSpace::Point> &leftPoints, const std::vector<ImageSpace::Point> &rightPoints, const PointType pointType, const bool fixedSpacing)
        : leftPoints(leftPoints), rightPoints(rightPoints), pointType(pointType), fixedSpacing(fixedSpacing) {}
    
    // Update points without affecting the space
    void setPoints (const std::vector<ImageSpace::Point> &leftPoints, const std::vector<ImageSpace::Point> &rightPoints, const PointType pointType, const bool fixedSpacing)
    {
        this->leftPoints = leftPoints;
        this->rightPoints = rightPoints;
        this->pointType = pointType;
        this->fixedSpacing = fixedSpacing;
        
        // Invalidate any existing labels and termination reasons
        labels.clear();
        leftTerminationReason = TerminationReason::Unknown;
        rightTerminationReason = TerminationReason::Unknown;
    }
    
    size_t nPoints () const { return std::max(static_cast<size_t>(leftPoints.size()+rightPoints.size())-1, size_t(0)); }
    size_t getSeedIndex () const { return std::max(static_cast<size_t>(leftPoints.size())-1, size_t(0)); }
    
    const std::vector<ImageSpace::Point> & getLeftPoints () const { return leftPoints; }
    const std::vector<ImageSpace::Point> & getRightPoints () const { return rightPoints; }
    std::vector<ImageSpace::Point> getPoints () const;
    PointType getPointType () const { return pointType; }
    bool usesFixedSpacing () const { return fixedSpacing; }
    
    double getLeftLength () const  { return getLength(leftPoints); }
    double getRightLength () const { return getLength(rightPoints); }
    
    void trimLeft (const double maxLength)  { trim(leftPoints,maxLength); }
    void trimRight (const double maxLength) { trim(rightPoints,maxLength); }
    
    int nLabels () const                            { return static_cast<int>(labels.size()); }
    bool addLabel (const int label)                 { return labels.insert(label).second; }
    bool removeLabel (const int label)              { return (labels.erase(label) == 1); }
    bool hasLabel (const int label) const           { return (labels.count(label) == 1); }
    const std::set<int> & getLabels () const        { return labels; }
    void setLabels (const std::set<int> &labels)    { this->labels = labels; }
    void clearLabels ()                             { labels.clear(); }
    
    TerminationReason getLeftTerminationReason () const     { return leftTerminationReason; }
    TerminationReason getRightTerminationReason () const    { return rightTerminationReason; }
    void setTerminationReasons (const TerminationReason left, const TerminationReason right)
    {
        leftTerminationReason = left;
        rightTerminationReason = right;
    }
};

// This manipulator replaces any existing labels with hits within an image
class StreamlineLabeller : public DataManipulator<Streamline>
{
private:
    Image<int,3> labelMap;
    
public:
    StreamlineLabeller (const Image<int,3> &labelMap)
        : labelMap(labelMap) {}
    
    bool process (Streamline &data) override;
};

class StreamlineTruncator : public DataManipulator<Streamline>
{
private:
    double maxLeftLength, maxRightLength;
    
public:
    StreamlineTruncator (const double maxLeftLength, const double maxRightLength)
        : maxLeftLength(maxLeftLength), maxRightLength(maxRightLength) {}
    
    bool process (Streamline &data) override
    {
        data.trimLeft(maxLeftLength);
        data.trimRight(maxRightLength);
        
        // Truncating streamlines may invalidate labels, so drop them
        data.clearLabels();
        
        return true;
    }
};

class StreamlineLabelMatcher : public DataSink<Streamline>
{
public:
    enum struct CombineOperation { None, And, Or };
    
private:
    std::vector<int> labels;
    CombineOperation combine;
    // If the combine operation is None, this will be one vector per label;
    // otherwise it will be one for all streamlines labelled as needed
    std::vector<std::vector<size_t>> matches;
    size_t currentStreamline = 0;
    
    // Checks whether the specified set of label hits matches the requirements,
    // and stores the associated index in the list(s) of matches if so
    void process (const std::set<int> &hits, const size_t &index);
    
public:
    // Delete the default constructor
    StreamlineLabelMatcher () = delete;
    
    StreamlineLabelMatcher (const std::vector<int> &labels, const CombineOperation combine)
        : labels(labels), combine(combine)
    {
        matches.resize(combine == CombineOperation::None ? labels.size() : 1);
    }
    
    void put (const Streamline &data) override
    {
        process(data.getLabels(), currentStreamline);
        currentStreamline++;
    }
    
    void put (const std::vector<std::set<int>> &hits)
    {
        for (size_t i=0; i<hits.size(); i++)
            process(hits[i], i);
    }
    
    const std::vector<std::vector<size_t>> & getMatches () const { return matches; }
};

class StreamlineLengthsDataSink : public DataSink<Streamline>
{
private:
    std::vector<double> lengths;
    
public:
    void put (const Streamline &data) override
    {
        lengths.push_back(data.getLeftLength() + data.getRightLength());
    }
    
    const std::vector<double> & getLengths () { return lengths; }
};

#endif
