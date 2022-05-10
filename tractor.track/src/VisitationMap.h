#ifndef _VISITATION_MAP_H_
#define _VISITATION_MAP_H_

#include "DataSource.h"
#include "Streamline.h"
#include "Image.h"

class VisitationMapDataSink : public DataSink<Streamline>
{
public:
    enum MappingScope { FullMappingScope, SeedMappingScope, EndsMappingScope };
    
private:
    Image<double,3> values;
    MappingScope scope;
    bool normalise;
    size_t totalStreamlines = 0;
    
    // Hide default constructor
    VisitationMapDataSink () {}
    
public:
    VisitationMapDataSink (const Image<double,3>::ArrayIndex &dims, const MappingScope scope = FullMappingScope, const bool normalise = false)
        : scope(scope), normalise(normalise)
    {
        values = Image<double,3>(dims, 0.0);
    }
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
    
    const Image<double,3> & getImage () const { return values; }
    void writeToNifti (const RNifti::NiftiImage &reference, const std::string &fileName) const;
};

#endif
