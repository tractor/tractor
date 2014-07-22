#ifndef _ARRAY_H_
#define _ARRAY_H_

#include "RcppArmadillo.h"

struct Neighbourhood
{
    size_t size;
    std::vector<int> widths;
    arma::Mat<int> locs;
    std::vector<ptrdiff_t> offsets;
};

template <typename DataType> class Array
{
protected:
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
    
    const bool empty () const { return (data.size() == 0); }
    
    const DataType & at (const size_t n) const { return data[n]; }
    
    const DataType & at (const std::vector<int> &loc) const
    {
        size_t n;
        flattenIndex(loc, n);
        return at(n);
    }
    
    size_t size () const { return data.size(); }
    
    const std::vector<int> & getDimensions () const { return dims; }
    
    int getDimensionality () const { return nDims; }
    
    Neighbourhood getNeighbourhood () const;
    
    Neighbourhood getNeighbourhood (const int width) const;
    
    Neighbourhood getNeighbourhood (const std::vector<int> &widths) const;
    
    void flattenIndex (const std::vector<int> &loc, size_t &result) const;
    
    void expandIndex (const size_t &loc, std::vector<int> &result) const;
};

#endif
