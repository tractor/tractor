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

#endif
