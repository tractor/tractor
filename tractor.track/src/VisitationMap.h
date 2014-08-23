#ifndef _VISITATION_MAP_H_
#define _VISITATION_MAP_H_

#include "DataSource.h"
#include "Streamline.h"
#include "Array.h"

class VisitationMapDataSink : public DataSink<Streamline>
{
private:
    Array<double> values;
    bool normalise;
    size_t totalStreamlines;
    
    // Hide default constructor
    VisitationMapDataSink () {}
    
public:
    VisitationMapDataSink (const std::vector<int> &dims, const bool normalise = false)
        : normalise(normalise), totalStreamlines(0)
    {
        values = Array<double>(dims, 0.0);
    }
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
    
    const Array<double> & getArray () const { return values; }
    void writeToNifti (const NiftiImage &reference, const std::string &fileName) const;
};

#endif
