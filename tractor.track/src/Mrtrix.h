#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include "FileAdapters.h"

class MrtrixSourceFileAdapter : public SourceFileAdapter
{
protected:
    std::string datatype;
    
public:
    using SourceFileAdapter::SourceFileAdapter;
    
    void open (StreamlineFileMetadata &metadata) override;
    void read (Streamline &data) override;
};

#endif
