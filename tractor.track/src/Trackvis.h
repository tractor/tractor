#ifndef _TRACKVIS_H_
#define _TRACKVIS_H_

#include "Streamline.h"
#include "DataSource.h"

class TrackvisDataSource : public DataSource<Streamline>
{
private:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    int nScalars, nProperties;
    size_t totalStreamlines, currentStreamline;
    
public:
    TrackvisDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    ~TrackvisDataSource ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    void setup (const std::string &fileName);
    bool more ();
    void get (Streamline &data);
};

class TrackvisDataSink : public DataSink<Streamline>
{
};

#endif
