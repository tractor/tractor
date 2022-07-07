#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include "Files.h"

class TrackvisSourceFileAdapter : public SourceFileAdapter
{
protected:
    int nScalars, seedProperty;
    
public:
    using SourceFileAdapter::SourceFileAdapter;
    
    void open ();
    size_t dataOffset () { return 1000; }
    void read (Streamline &data);
    void skip (const size_t n = 1);
    void close ();
};

class TrackvisSinkFileAdapter : public SinkFileAdapter
{
public:
    using SinkFileAdapter::SinkFileAdapter;
    
    size_t open (const bool append);
    constexpr size_t capacity () { return static_cast<size_t>(std::numeric_limits<int32_t>::max()); }
    size_t write (const Streamline &data);
    void close (const size_t &count);
};

#endif
