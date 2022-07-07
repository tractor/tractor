#ifndef _FILES_H_
#define _FILES_H_

#include "Image.h"
#include "DataSource.h"
#include "Streamline.h"

class SourceFileAdapter
{
protected:
    BinaryInputStream inputStream;
    std::string path;
    
    size_t count;
    std::vector<std::string> properties;
    ImageSpace *space = nullptr;
    
public:
    SourceFileAdapter (const std::string &path)
        : path(path)
    {
        inputStream.attach(path);
    }
    
    virtual ~SourceFileAdapter ()
    {
        delete space;
    }
    
    size_t nStreamlines () { return count; }
    size_t nProperties () { return properties.size(); }
    std::vector<std::string> propertyNames () { return properties; }
    ImageSpace * imageSpace () { return space; }
    
    virtual void open () {}
    virtual size_t dataOffset () { return 0; }
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
    
    bool haveLabels = false;
    std::vector<std::set<int>> labels;
    std::vector<size_t> offsets;
    std::map<int,std::string> dictionary;
    
    bool fileExists (const std::string &path) const
    {
        return std::ifstream(path).good();
    }
    
    void readLabels (const std::string &path);
    
public:
    StreamlineFileSource (const std::string &fileStem, const bool readLabels = true)
    {
        if (fileExists(fileStem + ".trk"))
            source = new TrackvisSourceFileAdapter(fileStem + ".trk");
        else if (fileExists(fileStem + ".tck"))
            source = new MrtrixSourceFileAdapter(fileStem + ".tck");
        else
            throw std::runtime_error("Specified streamline source file does not exist");
        
        source->open();
        totalStreamlines = source->nStreamlines();
        
        if (readLabels && fileExists(fileStem + ".trkl"))
            readLabels(fileStem + ".trkl");
    }
    
    bool more () { return currentStreamline < totalStreamlines; }
    void get (Streamline &data)
    {
        source->read(data);
        if (haveLabels && labels.size() > currentStreamline)
            data.setLabels(labels[currentStreamline]);
        currentStreamline++;
    }
    void seek (const int n);
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
    virtual size_t open (const bool append) {}
    
    // The maximum number of streamlines that the format can store (0 means no limit)
    virtual constexpr size_t capacity () { return 0; }
    
    // Write a streamline to file and return its offset within the file
    virtual size_t write (const Streamline &data) { return 0; }
    
    // Finalise the file. The argument is the final streamline count
    virtual void close (const size_t &count) {}
};

class StreamlineFileSink : public DataSink<Streamline>
{
private:
    // Prevent initialisation without a path
    StreamlineFileSink () {}
    
protected:
    size_t currentStreamline = 0;
    std::string fileStem;
    SinkFileAdapter *sink = nullptr;
    
    bool needLabels = false;
    std::vector<std::set<int>> labels;
    std::vector<size_t> offsets;
    std::map<int,std::string> dictionary;
    
    void writeLabels (const std::string &path);
    
public:
    StreamlineFileSink (const std::string &fileStem, const bool writeLabels = true, const bool append = false)
        : fileStem(fileStem), needLabels(writeLabels)
    {
        sink = new TrackvisSinkFileAdapter(fileStem + ".trk");
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
        const offset = sink->write(data);
        if (needLabels)
        {
            labels.push_back(data.getLabels());
            offsets.push_back(offset);
        }
        currentStreamline++;
    }
    
    void done ()
    {
        sink->close(currentStreamline);
        writeLabels(fileStem + ".trkl");
    }
};

#endif
