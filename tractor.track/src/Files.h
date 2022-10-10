#ifndef _FILES_H_
#define _FILES_H_

#include "DataSource.h"
#include "FileAdapters.h"
#include "Trackvis.h"
#include "Mrtrix.h"

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
    std::vector<size_t> matchLabels (const std::vector<int> &values) const;
    
    bool hasImageSpace () const { return (metadata != nullptr && metadata->space != nullptr); }
    ImageSpace * imageSpace () const { return metadata == nullptr ? nullptr : metadata->space; }
    
    StreamlineFileMetadata * fileMetadata () const { return metadata; }
    
    std::string type () const { return "file"; }
    
    void setup ();
    size_t count () { return totalStreamlines; }
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

class StreamlineFileSink : public DataSink<Streamline>
{
private:
    // Prevent initialisation without a path
    StreamlineFileSink () {}
    
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
    StreamlineFileSink (const std::string &fileStem, const bool keepLabels = true, const bool append = false)
        : fileStem(fileStem), keepLabels(keepLabels)
    {
        sink = new TrackvisSinkFileAdapter(fileStem + ".trk");
        metadata = new StreamlineFileMetadata;
        currentStreamline = sink->open(append);
    }
    
    StreamlineFileMetadata * fileMetadata () const { return metadata; }
    std::map<int,std::string> & labelDictionary () { return dictionary; }
    
    void setImageSpace (ImageSpace *space)
    {
        delete metadata->space;
        metadata->space = space;
    }
    
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
        if (keepLabels)
            writeLabels(fileStem + ".trkl");
    }
};

#endif
