#ifndef _IMAGE_H_
#define _IMAGE_H_

#include "RNifti.h"

struct ImageSpace
{
    typedef RNifti::NiftiImage::Xform::Element Element;
    typedef RNifti::NiftiImage::Xform::Vector3 Point;
    typedef RNifti::NiftiImage::Xform::Vector3 Vector;
    typedef RNifti::NiftiImage::Xform::Matrix Transform;
    
    typedef std::array<RNifti::NiftiImage::dim_t,3> DimVector;
    typedef std::array<RNifti::NiftiImage::pixdim_t,3> PixdimVector;
    
    enum PointType { VoxelPointType, ScaledPointType, WorldPointType };
    
    DimVector dim;
    PixdimVector pixdim;
    Transform transform;
    
    static constexpr Vector zeroVector ()
    {
        return Vector(0);
    }
    
    static bool zeroVector (const Vector &vector)
    {
        const Element norm = vector[0]*vector[0] + vector[1]*vector[1] + vector[2]*vector[2];
        return (sqrt(norm) == 0.0);
    }
    
    static Vector sphericalToCartesian (const Vector &spherical)
    {
        Vector cartesian;
        cartesian[0] = spherical[0] * sin(spherical[1]) * cos(spherical[2]);
        cartesian[1] = spherical[0] * sin(spherical[1]) * sin(spherical[2]);
        cartesian[2] = spherical[0] * cos(spherical[1]);
        return cartesian;
    }
};

template <class ElementType, int Dimensionality>
class Image
{
public:
    typedef ElementType Element;
    typedef std::array<size_t,Dimensionality> ArrayIndex;
    
protected:
    std::vector<Element> data_;
    ArrayIndex dims, strides;
    
    void calculateStrides ()
    {
        // The first index always moves fastest (as in R)
        strides[0] = 1;
        for (size_t i=1; i<Dimensionality; i++)
            strides[i] = strides[i-1] * dims[i-1];
    }
    
    template <int N>
    struct Indexer
    {
        const Indexer<N-1> child;
        
        size_t flatten (const ArrayIndex &loc) const
        {
            return strides[N-1] * loc[N-1] + child.flatten(loc);
        }
    };
    
    template<>
    struct Indexer<1>
    {
        size_t flatten (const ArrayIndex &loc) const { return loc[0]; }
    }
    
    Indexer<Dimensionality> indexer;
    ImageSpace *space = nullptr;
    
public:
    Image () { dims.fill(0); strides.fill(0); }
    
    Image (const ArrayIndex &dims, const Element value)
        : dims(dims)
    {
        size_t length = 1;
        for (size_t i=0; i<Dimensionality; i++)
            length *= dims[i];
        
        data_ = std::vector<Element>(length, value);
        
        calculateStrides();
    }
    
    Image (const ArrayIndex &dims, const std::vector<Element> &data)
        : dims(dims)
    {
        size_t length = 1;
        for (size_t i=0; i<Dimensionality; i++)
            length *= dims[i];
        
        if (length == data.size())
            this->data_ = data;
        else
            throw std::runtime_error("Data size does not match the specified dimensions");
        
        calculateStrides();
    }
    
    Image (const RNifti::NiftiImage &source)
    {
        if (source.isNull())
            throw std::runtime_error("NiftiImage source is empty");
        if (source.nDims() != Dimensionality)
            throw std::runtime_error("NiftiImage source is not of the right dimensionality");
        
        auto sourceDims = source.dim();
        std::copy(sourceDims.begin(), sourceDims.begin() + Dimensionality, dims.begin());
        calculateStrides();
        
        RNifti::NiftiImageData sourceData = source.data();
        if (sourceData.isEmpty())
            throw std::runtime_error("NiftiImage source contains no voxel data");
        std::copy(sourceData.begin(), sourceData.end(), data_.begin());
    }
    
    const std::vector<Element> & data () const { return data_; }
    const ArrayIndex & dim () const { return dims; }
    size_t size () const { return data_.size(); }
    void fill (const Element &value) { data_.assign(data_.size(), value); }
    
    std::vector<Element>::iterator begin () { return data_.begin(); }
    std::vector<Element>::iterator end () { return data_.end(); }
    std::vector<Element>::const_iterator begin () const { return data_.begin(); }
    std::vector<Element>::const_iterator end () const { return data_.end(); }
    
    Element & operator[] (const size_t n) { return data_[n]; }
    Element & operator[] (const ArrayIndex &loc) { return data_[indexer.flatten(loc)]; }
    
    const Element & operator[] (const size_t n) const { return data_[n]; }
    const Element & operator[] (const ArrayIndex &loc) const { return data_[indexer.flatten(loc)]; }
    
    void flattenIndex (const ArrayIndex &loc, size_t &result) const { result = indexer.flatten(loc); }
};

#endif
