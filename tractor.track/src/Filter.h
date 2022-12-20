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

class MedianStreamlineFilter : public DataManipulator<Streamline>
{
private:
    double quantile;
    size_t count = 0, current = 0;
    std::vector<Streamline> cache;
    
public:
    MedianStreamlineFilter (const double quantile = 0.99)
        : quantile(quantile) {}
    
    void setup (const size_t &count)
    {
        this->count = count;
        this->current = 0;
        cache.resize(count);
    }
    
    bool process (Streamline &data);
};

#endif
