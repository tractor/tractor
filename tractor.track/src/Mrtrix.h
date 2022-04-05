#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include <Rcpp.h>

#include "Streamline.h"

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
