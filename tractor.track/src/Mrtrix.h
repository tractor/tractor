#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include "Files.h"

class MrtrixSourceFileAdapter : public SourceFileAdapter
{
protected:
    std::string datatype;
    size_t dataOffset_;
    
public:
    using SourceFileAdapter::SourceFileAdapter;
    
    void open ();
    size_t dataOffset () { return dataOffset_; }
    void read (Streamline &data);
};

#endif
