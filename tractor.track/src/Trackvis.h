#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include <RcppEigen.h>

#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class TrackvisDataSource : public DataSource<Streamline>
{
protected:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    int nScalars, nProperties;
    size_t totalStreamlines, currentStreamline;
    Eigen::Array3f voxelDims;
    
public:
    TrackvisDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    TrackvisDataSource (const std::string &fileStem)
    {
        binaryStream.attach(&fileStream);
        attach(fileStem);
    }
    
    ~TrackvisDataSource ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    virtual void attach (const std::string &fileStem);
    bool more ();
    void get (Streamline &data);
};

class AugmentedTrackvisDataSource : public TrackvisDataSource
{
protected:
    std::ifstream auxFileStream;
    BinaryInputStream auxBinaryStream;
    
public:
    AugmentedTrackvisDataSource ()
    {
        auxBinaryStream.attach(&auxFileStream);
    }
    
    AugmentedTrackvisDataSource (const std::string &fileStem)
    {
        auxBinaryStream.attach(&auxFileStream);
        attach(fileStem);
    }
    
    ~AugmentedTrackvisDataSource ()
    {
        auxBinaryStream.detach();
        if (auxFileStream.is_open())
            auxFileStream.close();
    }
    
    void attach (const std::string &fileStem);
    bool more ();
    void get (Streamline &data);
};

class TrackvisDataSink : public DataSink<Streamline>
{
protected:
    std::ofstream fileStream;
    BinaryOutputStream binaryStream;
    size_t totalStreamlines;
    Eigen::Array3f voxelDims;
    
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
    
    TrackvisDataSink ()
    {
        binaryStream.attach(&fileStream);
        binaryStream.swapEndianness(false);
    }
    
    TrackvisDataSink (const std::string &fileStem, const NiftiImage &image)
    {
        binaryStream.attach(&fileStream);
        binaryStream.swapEndianness(false);
        attach(fileStem, image);
    }
    
    ~TrackvisDataSink ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    virtual void attach (const std::string &fileStem, const NiftiImage &image);
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
};

class AugmentedTrackvisDataSink : public TrackvisDataSink
{
protected:
    std::ofstream auxFileStream;
    BinaryOutputStream auxBinaryStream;
    std::map<int,std::string> labelDictionary;
    
public:
    AugmentedTrackvisDataSink ()
    {
        auxBinaryStream.attach(&auxFileStream);
        auxBinaryStream.swapEndianness(false);
    }
    
    AugmentedTrackvisDataSink (const std::string &fileStem, const NiftiImage &image)
    {
        auxBinaryStream.attach(&auxFileStream);
        auxBinaryStream.swapEndianness(false);
        attach(fileStem, image);
    }
    
    ~AugmentedTrackvisDataSink ()
    {
        auxBinaryStream.detach();
        if (auxFileStream.is_open())
            auxFileStream.close();
    }
    
    void attach (const std::string &fileStem, const NiftiImage &image);
};

#endif
