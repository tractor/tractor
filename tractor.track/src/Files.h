#ifndef _FILES_H_
#define _FILES_H_

#include "Image.h"
#include "DataSource.h"
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

class StreamlineFileSource : public DataSource<Streamline>
{
private:
    // Prevent initialisation without a path
    StreamlineFileSource () {}
    
protected:
    size_t currentStreamline = 0, totalStreamlines = 0;
    SourceFileAdapter *source = nullptr;
    StreamlineFileMetadata *metadata = nullptr;
    
    bool haveLabels = false;
    std::vector<std::set<int>> labels;
    std::vector<size_t> offsets;
    std::map<int,std::string> dictionary;
    
public:
    StreamlineFileSource (SourceFileAdapter *source)
        : source(source)
    {
        if (source == nullptr)
            throw std::runtime_error("A valid source must be specified");
        
        metadata = new StreamlineFileMetadata;
        source->open(*metadata);
        totalStreamlines = metadata->count;
    }
    
    virtual ~StreamlineFileSource ()
    {
        delete metadata;
        delete source;
    }
    
    void attachLabelFile (const std::string &path);
    
    bool more () { return currentStreamline < totalStreamlines; }
    void get (Streamline &data)
    {
        source->read(data);
        if (haveLabels && labels.size() > currentStreamline)
            data.setLabels(labels[currentStreamline]);
        currentStreamline++;
    }
    void seek (const size_t n);
    bool seekable () { return true; }
    void done () { source->close(); }
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

class StreamlineFileSink : public DataSink<Streamline>
{
private:
    // Prevent initialisation without a path
    StreamlineFileSink () {}
    
protected:
    size_t currentStreamline = 0;
    SinkFileAdapter *sink = nullptr;
    StreamlineFileMetadata *metadata = nullptr;
    
    bool keepLabels = false;
    std::vector<std::set<int>> labels;
    std::vector<size_t> offsets;
    std::map<int,std::string> dictionary;
    
    void writeLabels (const std::string &path);
    
public:
    StreamlineFileSink (SinkFileAdapter *sink, const bool keepLabels = true, const bool append = false)
        : sink(sink), keepLabels(keepLabels)
    {
        if (sink == nullptr)
            throw std::runtime_error("A valid sink must be specified");
        
        metadata = new StreamlineFileMetadata;
        currentStreamline = sink->open(append);
    }
    
    std::map<int,std::string> & labelDictionary () { return dictionary; }
    
    void setup (const size_t &count)
    {
        const size_t capacity = sink->capacity();
        if (capacity > 0 && currentStreamline + count > capacity)
            throw std::runtime_error("Total streamline count will exceed the capacity of the output file format");
    }
    
    void put (const Streamline &data)
    {
        const size_t offset = sink->write(data);
        if (keepLabels)
        {
            labels.push_back(data.getLabels());
            offsets.push_back(offset);
        }
        currentStreamline++;
    }
    
    void done ()
    {
        metadata->count = currentStreamline;
        sink->close(*metadata);
    }
};

#endif
