#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include <RcppArmadillo.h>

#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class TrackvisDataSource : public DataSource<Streamline>
{
private:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    int nScalars, nProperties;
    size_t totalStreamlines, currentStreamline;
    arma::fvec voxelDims;
    
public:
    TrackvisDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    TrackvisDataSource (const std::string &fileName)
    {
        binaryStream.attach(&fileStream);
        attach(fileName);
    }
    
    ~TrackvisDataSource ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    void attach (const std::string &fileName);
    bool more ();
    void get (Streamline &data);
};

class TrackvisDataSink : public DataSink<Streamline>
{
private:
    std::ofstream fileStream;
    BinaryOutputStream binaryStream;
    size_t totalStreamlines;
    arma::fvec voxelDims;
    
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
    
    TrackvisDataSink (const std::string &fileName, const NiftiImage &image)
    {
        binaryStream.attach(&fileStream);
        binaryStream.swapEndianness(false);
        attach(fileName, image);
    }
    
    ~TrackvisDataSink ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    void attach (const std::string &fileName, const NiftiImage &image);
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void done ();
};

#endif
