#ifndef _VISITATION_MAP_H_
#define _VISITATION_MAP_H_

#include "DataSource.h"
#include "Streamline.h"
#include "Array.h"

class VisitationMapDataSink : public DataSink<Streamline>
{
public:
    enum MappingScope { FullMappingScope, SeedMappingScope, EndsMappingScope };
    
private:
    Image<double> values;
    MappingScope scope;
    bool normalise;
    size_t totalStreamlines;
    
    // Hide default constructor
    VisitationMapDataSink () {}
    
public:
    VisitationMapDataSink (const std::vector<int> &dims, const MappingScope scope = FullMappingScope, const bool normalise = false)
        : scope(scope), normalise(normalise), totalStreamlines(0)
    {
        values = Image<double>(dims, 0.0);
    }
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
    
    const Image<double> & getArray () const { return values; }
    void writeToNifti (const RNifti::NiftiImage &reference, const std::string &fileName) const;
};

#endif
