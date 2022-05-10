#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include <Rcpp.h>

#include "Image.h"
#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class TrackvisDataSource : public StreamlineFileSource
{
protected:
    int nScalars, nProperties, seedProperty;
    
    void readStreamline (Streamline &data);
    
public:
    using StreamlineFileSource::StreamlineFileSource;
    
    void setup ();
    void get (Streamline &data);
    void seek (const int n);
    bool seekable () { return true; }
};

class TrackvisDataSink : public ImageSpaceEmbedded, public DataSink<Streamline>
{
protected:
    BinaryOutputStream outputStream;
    size_t totalStreamlines;
    bool append = false;
    
    // TrackvisDataSink (const std::string &fileStem, const ImageSpace &grid, const bool append = false)
    //     : grid(grid), append(append)
    // {
    //     binaryStream.attach(&fileStream);
    //     attach(fileStem);
    // }
    
    void writeStreamline (const Streamline &data);
    
public:
    TrackvisDataSink (const std::string &fileStem, const bool append = false)
        : append(append)
    {
        attach(fileStem);
    }
    
    virtual void attach (const std::string &fileStem);
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void done ();
};

// class BasicTrackvisDataSink : public TrackvisDataSink
// {
// public:
//     using TrackvisDataSink::TrackvisDataSink;
//     
//     void put (const Streamline &data) { writeStreamline(data); }
//     void append (const Streamline &data)
//     {
//         writeStreamline(data);
//         totalStreamlines++;
//     }
// };
// 
// class LabelledTrackvisDataSink : public TrackvisDataSink
// {
// protected:
//     std::ofstream auxFileStream;
//     BinaryOutputStream auxBinaryStream;
//     std::map<int,std::string> labelDictionary;
//     
// public:
//     LabelledTrackvisDataSink ()
//     {
//         auxBinaryStream.attach(&auxFileStream);
//     }
//     
//     // Don't call base class constructor explicitly here
//     LabelledTrackvisDataSink (const std::string &fileStem, const ImageSpace &grid, const std::map<int,std::string> labelDictionary)
//         : labelDictionary(labelDictionary)
//     {
//         this->grid = grid;
//         auxBinaryStream.attach(&auxFileStream);
//         attach(fileStem);
//     }
//     
//     ~LabelledTrackvisDataSink ()
//     {
//         auxBinaryStream.detach();
//         if (auxFileStream.is_open())
//             auxFileStream.close();
//     }
//     
//     void attach (const std::string &fileStem);
//     void put (const Streamline &data);
//     void done ();
// };
// 
// class MedianTrackvisDataSink : public TrackvisDataSink
// {
// protected:
//     double quantile;
//     Streamline median;
//     
// public:
//     MedianTrackvisDataSink (const std::string &fileStem, const ImageSpace &grid, const double quantile = 0.99)
//         : TrackvisDataSink(fileStem,grid), quantile(quantile) {}
//     
//     void setup (const size_type &count, const_iterator begin, const_iterator end);
//     void done ();
// };

#endif
