#ifndef _FILTER_H_
#define _FILTER_H_

#include "DataSource.h"
#include "Streamline.h"

class LengthFilter : public DataManipulator<Streamline>
{
private:
    double minLength, maxLength;
    
public:
    LengthFilter (const double minLength, const double maxLength = 0.0)
        : minLength(minLength), maxLength(maxLength) {}
    
    bool process (Streamline &data)
    {
        const double length = data.getLeftLength() + data.getRightLength();
        if (minLength > 0.0 && length < minLength)
            return false;
        if (maxLength > 0.0 && length > maxLength)
            return false;
        return true;
    }
};

class LabelFilter : public DataManipulator<Streamline>
{
private:
    std::vector<int> labels;
    
public:
    LabelFilter (const std::vector<int> &labels)
        : labels(labels) {}
    
    bool process (Streamline &data)
    {
        for (std::vector<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
        {
            if (!data.hasLabel(*it))
                return false;
        }
        return true;
    }
};

class LabelCountFilter : public DataManipulator<Streamline>
{
private:
    int minCount, maxCount;
    
public:
    LabelCountFilter (const int minCount, const int maxCount = 0)
        : minCount(minCount), maxCount(maxCount) {}
    
    bool process (Streamline &data)
    {
        const int count = data.nLabels();
        if (minCount > 0 && count < minCount)
            return false;
        if (maxCount > 0 && count > maxCount)
            return false;
        return true;
    }
};

class IndexFilter : public DataManipulator<Streamline>
{
private:
    std::vector<int> indices;
    int loc, counter;
    
public:
    IndexFilter (const std::vector<int> &indices)
        : indices(indices), loc(0), counter(0)
    {
        std::sort(this->indices.begin(), this->indices.end());
    }
    
    bool process (Streamline &data)
    {
        bool result = false;
        if (loc < indices.size() && counter == indices[loc])
        {
            result = true;
            loc++;
        }
        counter++;
        return result;
    }
};

#endif
