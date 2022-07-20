#ifndef _FILE_ADAPTERS_H_
#define _FILE_ADAPTERS_H_

#include "Image.h"
#include "Streamline.h"

struct StreamlineFileMetadata
{
    size_t count = 0;
    size_t dataOffset = 0;
    std::vector<std::string> properties;
    ImageSpace *space = nullptr;
    
    ~StreamlineFileMetadata ()
    {
        delete space;
    }
};

class SourceFileAdapter
{
protected:
    BinaryInputStream inputStream;
    std::string path;
    
public:
    SourceFileAdapter (const std::string &path)
        : path(path)
    {
        inputStream.attach(path);
    }
    
    virtual ~SourceFileAdapter () {}
    
    virtual void open (StreamlineFileMetadata &metadata) {}
    virtual void seek (const size_t offset)
    {
        inputStream->seekg(offset);
        if (!inputStream->good())
            throw std::runtime_error("Failed to seek to offset " + std::to_string(offset));
    }
    virtual void read (Streamline &data) {}
    virtual void skip (const size_t n = 1)
    {
        // Default implementation: read each streamline as normal, but ignore it
        Streamline ignored;
        for (size_t i=0; i<n; i++)
            read(ignored);
    }
    virtual void close () {}
};

class SinkFileAdapter
{
protected:
    BinaryOutputStream outputStream;
    std::string path;
    
public:
    SinkFileAdapter (const std::string &path)
        : path(path)
    {
        outputStream.attach(path);
    }
    
    virtual ~SinkFileAdapter () {}
    
    // Read header (if there is one) and prepare to read first streamline
    // Should return the current number of streamlines in the file (0 unless appending)
    virtual size_t open (const bool append) { return 0; }
    
    // The maximum number of streamlines that the format can store (0 means no limit)
    virtual size_t capacity () const { return 0; }
    
    // Write a streamline to file and return its offset within the file
    virtual size_t write (const Streamline &data) { return 0; }
    
    // Finalise the file with the specified metadata
    virtual void close (const StreamlineFileMetadata &metadata) {}
};

#endif
