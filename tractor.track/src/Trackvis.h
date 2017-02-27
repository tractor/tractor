#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include <RcppEigen.h>

#include "Grid.h"
#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

// Base class for Trackvis readers: provides common functionality
class TrackvisDataSource : public Griddable3D, public DataSource<Streamline>
{
protected:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    int nScalars, nProperties, seedProperty;
    size_t totalStreamlines, currentStreamline;
    Grid<3> grid;
    
    TrackvisDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    TrackvisDataSource (const std::string &fileStem)
    {
        binaryStream.attach(&fileStream);
        attach(fileStem);
    }
    
    void readStreamline (Streamline &data);
    
public:
    virtual ~TrackvisDataSource ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    virtual void attach (const std::string &fileStem);
    Grid<3> getGrid3D () const { return grid; }
};

// Basic Trackvis reader: read all streamlines, including seed property
class BasicTrackvisDataSource : public TrackvisDataSource
{
public:
    BasicTrackvisDataSource (const std::string &fileStem)
        : TrackvisDataSource(fileStem) {}
    
    size_t nStreamlines () const { return totalStreamlines; }
    
    bool more () { return (currentStreamline < totalStreamlines); }
    void get (Streamline &data) { readStreamline(data); }
    void seek (const int n);
    bool seekable () { return true; }
};

class StreamlineLabelList
{
private:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    std::vector< std::set<int> > labelList;
    std::vector<size_t> offsetList;
    
public:
    StreamlineLabelList ()
    {
        binaryStream.attach(&fileStream);
    }
    
    StreamlineLabelList (const std::string &fileStem)
    {
        binaryStream.attach(&fileStream);
        read(fileStem);
    }
    
    void read (const std::string &fileStem);
    const std::vector<int> find (const std::vector<int> &labels);
    size_t size () { return labelList.size(); }
    const std::set<int> & getLabels (const int n) { return labelList[n]; }
    size_t getOffset (const int n) { return offsetList[n]; }
};

// Labelled Trackvis reader: also read auxiliary file containing label info
class LabelledTrackvisDataSource : public TrackvisDataSource
{
protected:
    std::ifstream auxFileStream;
    BinaryInputStream auxBinaryStream;
    StreamlineLabelList *labelList;
    bool externalLabelList;
    
public:
    LabelledTrackvisDataSource ()
    {
        auxBinaryStream.attach(&auxFileStream);
    }
    
    LabelledTrackvisDataSource (const std::string &fileStem, StreamlineLabelList *labelList = NULL)
        : labelList(labelList)
    {
        auxBinaryStream.attach(&auxFileStream);
        this->externalLabelList = (labelList != NULL);
        attach(fileStem);
    }
    
    ~LabelledTrackvisDataSource ()
    {
        auxBinaryStream.detach();
        if (auxFileStream.is_open())
            auxFileStream.close();
        if (!externalLabelList)
            delete labelList;
    }
    
    void attach (const std::string &fileStem);
    bool more () { return (currentStreamline < totalStreamlines); }
    void get (Streamline &data);
    void seek (const int n);
    bool seekable () { return true; }
};

// Median Trackvis reader: construct and return median streamline only
class MedianTrackvisDataSource : public TrackvisDataSource
{
protected:
    bool read;
    double quantile;
    
public:
    MedianTrackvisDataSource ()
        : read(false) {}
    
    MedianTrackvisDataSource (const std::string &fileStem, const double quantile = 0.99)
        : TrackvisDataSource(fileStem), quantile(quantile), read(false) {}
    
    bool more () { return (!read); }
    void get (Streamline &data);
};

class TrackvisDataSink : public DataSink<Streamline>
{
protected:
    std::ofstream fileStream;
    BinaryOutputStream binaryStream;
    size_t totalStreamlines;
    Grid<3> grid;
    
    TrackvisDataSink ()
    {
        binaryStream.attach(&fileStream);
        binaryStream.swapEndianness(false);
    }
    
    TrackvisDataSink (const std::string &fileStem, const Grid<3> &grid)
        : grid(grid)
    {
        binaryStream.attach(&fileStream);
        binaryStream.swapEndianness(false);
        attach(fileStem);
    }
    
    void writeStreamline (const Streamline &data);
    
public:
    static std::map<int,char> orientationCodeMap;
    
    static std::map<int,char> createOrientationCodeMap ()
    {
        std::map<int,char> map;
        map[NIFTI_L2R] = 'R';
        map[NIFTI_R2L] = 'L';
        map[NIFTI_P2A] = 'A';
        map[NIFTI_A2P] = 'P';
        map[NIFTI_I2S] = 'S';
        map[NIFTI_S2I] = 'I';
        return map;
    }
    
    virtual ~TrackvisDataSink ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    virtual void attach (const std::string &fileStem);
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void done ();
};

class BasicTrackvisDataSink : public TrackvisDataSink
{
public:
    BasicTrackvisDataSink (const std::string &fileStem, const Grid<3> &grid)
        : TrackvisDataSink(fileStem,grid) {}
    
    void put (const Streamline &data) { writeStreamline(data); }
};

class LabelledTrackvisDataSink : public TrackvisDataSink
{
protected:
    std::ofstream auxFileStream;
    BinaryOutputStream auxBinaryStream;
    std::map<int,std::string> labelDictionary;
    
public:
    LabelledTrackvisDataSink ()
    {
        auxBinaryStream.attach(&auxFileStream);
        auxBinaryStream.swapEndianness(false);
    }
    
    // Don't call base class constructor explicitly here
    LabelledTrackvisDataSink (const std::string &fileStem, const Grid<3> &grid, const std::map<int,std::string> labelDictionary)
        : labelDictionary(labelDictionary)
    {
        this->grid = grid;
        auxBinaryStream.attach(&auxFileStream);
        auxBinaryStream.swapEndianness(false);
        attach(fileStem);
    }
    
    ~LabelledTrackvisDataSink ()
    {
        auxBinaryStream.detach();
        if (auxFileStream.is_open())
            auxFileStream.close();
    }
    
    void attach (const std::string &fileStem);
    void put (const Streamline &data);
    void done ();
};

class MedianTrackvisDataSink : public TrackvisDataSink
{
protected:
    double quantile;
    Streamline median;
    
public:
    MedianTrackvisDataSink (const std::string &fileStem, const Grid<3> &grid, const double quantile = 0.99)
        : TrackvisDataSink(fileStem,grid), quantile(quantile) {}
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void done ();
};

#endif
