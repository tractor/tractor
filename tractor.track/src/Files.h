#ifndef _FILES_H_
#define _FILES_H_

#include "DataSource.h"
#include "FileAdapters.h"
#include "Trackvis.h"
#include "Mrtrix.h"

class StreamlineFileSource : public DataSource<Streamline>
{
protected:
    size_t currentStreamline = 0, totalStreamlines = 0;
    SourceFileAdapter *source = nullptr;
    StreamlineFileMetadata *metadata = nullptr;
    
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
    // Prevent initialisation without a path
    StreamlineFileSource () = delete;
    
    StreamlineFileSource (const std::string &fileStem, const bool readLabels = true)
    {
        if (fileExists(fileStem + ".trk"))
            source = new TrackvisSourceFileAdapter(fileStem + ".trk");
        else if (fileExists(fileStem + ".tck"))
            source = new MrtrixSourceFileAdapter(fileStem + ".tck");
        else
            throw std::runtime_error("Specified streamline source file does not exist");
        
        metadata = new StreamlineFileMetadata;
        source->open(*metadata);
        totalStreamlines = metadata->count;
        
        if (readLabels && fileExists(fileStem + ".trkl"))
            this->readLabels(fileStem + ".trkl");
    }
    
    virtual ~StreamlineFileSource ()
    {
        delete metadata;
        delete source;
    }
    
    bool hasLabels () const { return haveLabels; }
    const std::vector<std::set<int>> & labelList () const { return labels; }
    
    bool hasImageSpace () const { return (metadata != nullptr && metadata->space != nullptr); }
    ImageSpace * imageSpace () const { return metadata == nullptr ? nullptr : metadata->space; }
    
    StreamlineFileMetadata * fileMetadata () const { return metadata; }
    
    std::string type () const override { return "file"; }
    
    void setup () override;
    size_t count () override { return totalStreamlines; }
    bool more () override { return currentStreamline < totalStreamlines; }
    void get (Streamline &data) override
    {
        source->read(data);
        if (haveLabels && labels.size() > currentStreamline)
            data.setLabels(labels[currentStreamline]);
        currentStreamline++;
    }
    void seek (const size_t n) override;
    bool seekable () override { return true; }
    void done () override { source->close(); }
};

class StreamlineFileSink : public DataSink<Streamline>
{
protected:
    size_t currentStreamline = 0;
    std::string fileStem;
    SinkFileAdapter *sink = nullptr;
    StreamlineFileMetadata *metadata = nullptr;
    
    bool keepLabels = false;
    std::vector<std::set<int>> labels;
    std::vector<size_t> offsets;
    std::map<int,std::string> dictionary;
    
    void writeLabels (const std::string &path);
    
public:
    // Prevent initialisation without a path
    StreamlineFileSink () = delete;
    
    StreamlineFileSink (const std::string &fileStem, const bool keepLabels = true, const bool append = false)
        : fileStem(fileStem), keepLabels(keepLabels)
    {
        sink = new TrackvisSinkFileAdapter(fileStem + ".trk");
        metadata = new StreamlineFileMetadata;
        currentStreamline = sink->open(append);
    }
    
    virtual ~StreamlineFileSink ()
    {
        delete metadata;
        delete sink;
    }
    
    StreamlineFileMetadata * fileMetadata () const { return metadata; }
    std::map<int,std::string> & labelDictionary () { return dictionary; }
    
    void setImageSpace (ImageSpace *space)
    {
        metadata->space = space;
        metadata->sharedSpace = true;
    }
    
    void setup (const size_t &count) override
    {
        const size_t capacity = sink->capacity();
        if (capacity > 0 && currentStreamline + count > capacity)
            throw std::runtime_error("Total streamline count will exceed the capacity of the output file format");
    }
    
    void put (const Streamline &data) override
    {
        const size_t offset = sink->write(data, metadata->space);
        if (keepLabels)
        {
            labels.push_back(data.getLabels());
            offsets.push_back(offset);
        }
        currentStreamline++;
    }
    
    void done () override
    {
        metadata->count = currentStreamline;
        sink->close(*metadata);
        if (keepLabels)
            writeLabels(fileStem + ".trkl");
    }
};

#endif
