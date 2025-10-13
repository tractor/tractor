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
    Image<double,3> *values = nullptr;
    MappingScope scope;
    bool normalise;
    size_t totalStreamlines = 0;
    
public:
    // Delete the default constructor
    VisitationMapDataSink () = delete;
    
    explicit VisitationMapDataSink (Image<double,3> * const values, const MappingScope scope = MappingScope::All, const bool normalise = false)
        : values(values), scope(scope), normalise(normalise)
    {
        this->values->fill(0.0);
    }
    
    void setup (const size_t &count) override
    {
        totalStreamlines += count;
    }
    
    void put (const Streamline &data) override;
    void done () override;
};

#endif
