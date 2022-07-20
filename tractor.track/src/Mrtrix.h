#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include "FileAdapters.h"

class MrtrixSourceFileAdapter : public SourceFileAdapter
{
protected:
    std::string datatype;
    
public:
    using SourceFileAdapter::SourceFileAdapter;
    
    void open (StreamlineFileMetadata &metadata);
    void read (Streamline &data);
};

#endif
