#ifndef _VISITATION_MAP_H_
#define _VISITATION_MAP_H_

#include "DataSource.h"
#include "Streamline.h"
#include "Image.h"

class VisitationMapDataSink : public DataSink<Streamline>
{
public:
    enum struct MappingScope { All, Seed, Ends };
    
private:
    Image<double,3> values;
    MappingScope scope;
    bool normalise;
    size_t totalStreamlines = 0;
    
public:
    // Delete the default constructor
    VisitationMapDataSink () = delete;
    
    VisitationMapDataSink (const ImageRaster<3> &raster, const MappingScope scope = MappingScope::All, const bool normalise = false)
        : scope(scope), normalise(normalise)
    {
        this->values = Image<double,3>(raster, 0.0);
    }
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
    
    const Image<double,3> & getImage () const { return values; }
    void writeToNifti (const std::string &fileName, ImageSpace *space = nullptr);
};

#endif
