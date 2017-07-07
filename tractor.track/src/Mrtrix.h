#ifndef _MRTRIX_H_
#define _MRTRIX_H_

#include <RcppEigen.h>

#include "Grid.h"
#include "Streamline.h"
#include "DataSource.h"
#include "BinaryStream.h"

class MrtrixDataSource : public Griddable3D, public DataSource<Streamline>
{
protected:
    std::ifstream fileStream;
    BinaryInputStream binaryStream;
    size_t totalStreamlines, currentStreamline;
    Grid<3> grid;
    
    void readStreamline (Streamline &data);
    
public:
    MrtrixDataSource ()
    {
        binaryStream.attach(&fileStream);
    }
    
    MrtrixDataSource (const std::string &fileStem, const Grid<3> &grid)
        : grid(grid)
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
    Grid<3> getGrid3D () const { return grid; }
    
    size_t nStreamlines () const { return totalStreamlines; }
    
    bool more () { return (currentStreamline < totalStreamlines); }
    void get (Streamline &data) { readStreamline(data); }
};

#endif
