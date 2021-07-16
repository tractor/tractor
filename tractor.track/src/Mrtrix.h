#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include <RcppEigen.h>

#include "Grid.h"
#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class MrtrixDataSource : public DataSource<Streamline>
{
protected:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    size_t totalStreamlines, currentStreamline;
    std::string datatype;
    
    void readStreamline (Streamline &data);
    
public:
    MrtrixDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    MrtrixDataSource (const std::string &fileStem)
    {
        binaryStream.attach(&fileStream);
        attach(fileStem);
    }
    
    virtual ~MrtrixDataSource ()
    {
        binaryStream.detach();
        if (fileStream.is_open())
            fileStream.close();
    }
    
    void attach (const std::string &fileStem);
    
    size_t nStreamlines () const { return totalStreamlines; }
    
    bool more () { return (currentStreamline < totalStreamlines); }
    void get (Streamline &data) { readStreamline(data); }
};

#endif
