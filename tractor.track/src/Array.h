#ifndef _ARRAY_H_
#define _ARRAY_H_

#include "RcppArmadillo.h"

struct Neighbourhood
{
    long size;
    std::vector<int> widths;
    arma::Mat<int> locs;
    std::vector<long> offsets;
};

template <typename DataType> class Array
{
private:
    std::vector<DataType> data;
    std::vector<int> dims;
    int nDims;

public:
    Array () {}
    
    Array (const std::vector<DataType> &data, const std::vector<int> &dims)
        : data(data), dims(dims)
    {
        nDims = dims.size();
    }
    
    const DataType & at (long n) const { return data[n]; }
    
    long size () const { return data.size(); }
    
    const std::vector<int> & getDims () const { return dims; }
    
    int getNDims () const { return nDims; }
    
    Neighbourhood getNeighbourhood () const;
    
    Neighbourhood getNeighbourhood (const int width) const;
    
    Neighbourhood getNeighbourhood (const std::vector<int> &widths) const;
    
    void flattenIndex (const std::vector<int> &loc, long &result) const;
    
    void expandIndex (const long &loc, std::vector<int> &result) const;
};

#endif
