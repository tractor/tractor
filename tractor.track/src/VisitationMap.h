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
    std::string path;
    Image<double,3> values;
    MappingScope scope;
    bool normalise;
    size_t totalStreamlines = 0;
    
public:
    // Delete the default constructor
    VisitationMapDataSink () = delete;
    
    VisitationMapDataSink (const std::string &path, ImageSpace *space, const MappingScope scope = MappingScope::All, const bool normalise = false)
        : path(path), scope(scope), normalise(normalise)
    {
        this->values = Image<double,3>(space->dim, 0.0);
        this->values.setImageSpace(space, true);
    }
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
    
    const Image<double,3> & getImage () const { return values; }
};

#endif
