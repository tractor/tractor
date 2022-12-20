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
    
    explicit VisitationMapDataSink (ImageSpace *space, const MappingScope scope = MappingScope::All, const bool normalise = false)
        : scope(scope), normalise(normalise)
    {
        this->values = Image<double,3>(space->dim, 0.0);
        this->values.setImageSpace(space, true);
    }
    
    void setup (const size_t &count) override
    {
        totalStreamlines += count;
    }
    
    void put (const Streamline &data) override;
    void done () override;
    
    const Image<double,3> & getImage () const { return values; }
};

#endif
