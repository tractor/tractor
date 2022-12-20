#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include "Image.h"
#include "FileAdapters.h"

class TrackvisSourceFileAdapter : public SourceFileAdapter
{
protected:
    int nScalars, nProperties, seedProperty;
    ImageSpace *space = nullptr;
    
public:
    using SourceFileAdapter::SourceFileAdapter;
    
    void open (StreamlineFileMetadata &metadata) override;
    void read (Streamline &data) override;
    void skip (const size_t n = 1) override;
};

class TrackvisSinkFileAdapter : public SinkFileAdapter
{
public:
    using SinkFileAdapter::SinkFileAdapter;
    
    size_t open (const bool append) override;
    size_t capacity () { return static_cast<size_t>(std::numeric_limits<int32_t>::max()); }
    size_t write (const Streamline &data) override;
    void close (const StreamlineFileMetadata &metadata) override;
};

#endif
