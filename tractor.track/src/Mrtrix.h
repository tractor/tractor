#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include <RcppEigen.h>

#include "Grid.h"
#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class MrtrixDataSource : public StreamlineFileSource
{
protected:
    std::string datatype;
    
    void readStreamline (Streamline &data);
    
public:
    using StreamlineFileSource::StreamlineFileSource;
    
    void setup ();
    void get (Streamline &data) { readStreamline(data); }
};

#endif
