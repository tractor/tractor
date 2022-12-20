#ifndef _IMAGE_H_
#define _IMAGE_H_

#include <RcppCommon.h>
#include <array>

namespace Rcpp {
namespace traits {

// Partial specialisation to allow as<array<T,D>>(...)
template <typename ElementType, int Dimensionality>
class Exporter<std::array<ElementType,Dimensionality>>
{
private:
    std::array<ElementType,Dimensionality> value;

public:
    Exporter (SEXP x)
    {
        std::vector<ElementType> vec = as<std::vector<ElementType>>(x);
        if (vec.size() != Dimensionality)
            throw Rcpp::exception("Array does not have the expected number of elements");
        for (int i=0; i<Dimensionality; i++)
            value[i] = vec[i];
    }
    
    std::array<ElementType,Dimensionality> get () { return value; }
};

} // traits namespace
} // Rcpp namespace

#include <Rcpp.h>
#include "RNifti.h"

// Location conventions: voxel-indexed, scaled for voxel dimensions only (as
// with a diagonal xform), or world coordinates fully respecting the xform
enum struct PointType { Voxel, Scaled, World };

// Rounding strategies: none, standard for nearest-neighbour, or probabilistic
// for stochastic nearest neighbour (probabilities proportional to distance)
enum struct RoundingType { None, Conventional, Probabilistic };

// Handles the geometry of the 3D space an image is embedded within, including
// continuous point and vector concepts, affine xforms and conversion between
// point types
class ImageSpace
{
public:
    typedef RNifti::NiftiImage::Xform::Element Element;
    typedef RNifti::NiftiImage::Xform::Vector3 Point;
    typedef RNifti::NiftiImage::Xform::Vector3 Vector;
    typedef RNifti::NiftiImage::Xform::Matrix Transform;
    
    // RNifti-compatible dim and pixdim array types
    typedef std::array<RNifti::NiftiImage::dim_t,3> DimVector;
    typedef std::array<RNifti::NiftiImage::pixdim_t,3> PixdimVector;
    
    DimVector dim;
    PixdimVector pixdim;
    Transform transform;
    
    static Vector zeroVector ()
    {
        return Vector(0.0);
    }
    
    static Element norm (const Vector &vector)
    {
        const Element squaredNorm = vector[0]*vector[0] + vector[1]*vector[1] + vector[2]*vector[2];
        return sqrt(squaredNorm);
    }
    
    static Element dot (const Vector &first, const Vector &second)
    {
        const Element product = first[0]*second[0] + first[1]*second[1] + first[2]*second[2];
        return product;
    }
    
    static Vector step (const Point &from, const Point &to)
    {
        Vector result;
        for (int i=0; i<3; i++)
            result[i] = to[i] - from[i];
        return result;
    }
    
    static Vector sphericalToCartesian (const Vector &spherical)
    {
        Vector cartesian;
        cartesian[0] = spherical[0] * sin(spherical[1]) * cos(spherical[2]);
        cartesian[1] = spherical[0] * sin(spherical[1]) * sin(spherical[2]);
        cartesian[2] = spherical[0] * cos(spherical[1]);
        return cartesian;
    }
    
    ImageSpace (const DimVector &dim, const PixdimVector &pixdim, const Transform &transform)
        : dim(dim), pixdim(pixdim), transform(transform) {}
    
    ImageSpace (const DimVector &dim, const PixdimVector &pixdim)
        : dim(dim), pixdim(pixdim)
    {
        this->transform = Transform::eye();
        for (int i=0; i<3; i++)
            this->transform(i,i) = pixdim[i];
    }
    
    explicit ImageSpace (const DimVector &dim)
        : ImageSpace(dim, {1,1,1}, Transform::eye()) {}
    
    ImageSpace ()
        : ImageSpace({0,0,0}, {1,1,1}, Transform::eye()) {}
    
    explicit ImageSpace (const RNifti::NiftiImage &source)
    {
        std::vector<RNifti::NiftiImage::dim_t> vdim = source.dim();
        std::vector<RNifti::NiftiImage::pixdim_t> vpixdim = source.pixdim();
        
        dim = { 1, 1, 1 };
        pixdim = { 1.0, 1.0, 1.0 };
        
        for (unsigned i=0; i<std::min<size_t>(3,vdim.size()); i++)
        {
            dim[i] = vdim[i];
            pixdim[i] = vpixdim[i];
        }
        
        transform = source.xform().matrix();
    }
    
    std::string orientation () const { return RNifti::NiftiImage::Xform(transform).orientation(); }
    
    Point toVoxel (const Point &point, const PointType type, const RoundingType round = RoundingType::Conventional) const;
};

// Functionality for objects that conceptually exist within an image space
class ImageSpaceEmbedded
{
protected:
    ImageSpace *space = nullptr;
    bool sharedSpace = false;
    
public:
    virtual ~ImageSpaceEmbedded ()
    {
        if (!sharedSpace)
            delete space;
    }
    
    bool hasImageSpace () const { return (space != nullptr); }
    ImageSpace * imageSpace () const { return space; }
    
    void copyImageSpace (const ImageSpaceEmbedded &other)
    {
        this->space = new ImageSpace(*other.imageSpace());
        sharedSpace = false;
    }
    
    void setImageSpace (ImageSpace * const space, const bool shared = false)
    {
        this->space = space;
        sharedSpace = shared;
    }
};

// Handles image array functionality that does not depend on the datatype,
// notably indexing
template <int Dimensionality>
class ImageRaster
{
public:
    typedef std::array<size_t,Dimensionality> ArrayIndex;
    
protected:
    ArrayIndex dims, strides;
    size_t length;
    
    void calculateStrides ()
    {
        // The first index always moves fastest (as in R)
        strides[0] = length = 1;
        for (size_t i=1; i<Dimensionality; i++)
        {
            strides[i] = strides[i-1] * dims[i-1];
            length *= dims[i-1];
        }
        length *= dims[Dimensionality - 1];
    }
    
    template <int N>
    struct Indexer
    {
        Indexer<N-1> child;
        
        size_t flatten (const ArrayIndex &loc, const ArrayIndex &strides) const
        {
            return strides[N-1] * loc[N-1] + child.flatten(loc, strides);
        }
    };
    
    template<>
    struct Indexer<1>
    {
        size_t flatten (const ArrayIndex &loc, const ArrayIndex &strides) const { return loc[0]; }
    };
    
    Indexer<Dimensionality> indexer;
    
public:
    ImageRaster () : length(0) { dims.fill(0); strides.fill(0); }
    
    // These constructors are not explicit because we want to allow automatic conversion
    ImageRaster (const ArrayIndex &dims)
        : dims(dims)
    {
        calculateStrides();
    }
    
    ImageRaster (const ImageSpace::DimVector &dims)
    {
        this->dims.fill(1);
        for (int i=0; i<std::min(3,Dimensionality); i++)
            this->dims[i] = dims[i];
        calculateStrides();
    }
    
    ImageRaster (const std::vector<RNifti::NiftiImage::dim_t> &dims)
    {
        if (dims.size() != Dimensionality)
            throw std::runtime_error("Dimension vector is not of the right dimensionality");
        std::copy(dims.begin(), dims.end(), this->dims.begin());
        calculateStrides();
    }
    
    const ArrayIndex & dim () const { return dims; }
    const size_t size () const { return length; }
    
    size_t flattenIndex (const ArrayIndex &loc) const { return indexer.flatten(loc, strides); }
    void flattenIndex (const ArrayIndex &loc, size_t &result) const { result = indexer.flatten(loc, strides); }
};

// A typed and fixed-dimensionality bounded array embedded in 3D space
// This class focusses on handling the pixel/voxel data
template <class ElementType, int Dimensionality>
class Image : public ImageSpaceEmbedded
{
public:
    typedef ElementType Element;
    typedef std::vector<Element> Vector;
    typedef ImageRaster<Dimensionality> Raster;
    typedef typename ImageRaster<Dimensionality>::ArrayIndex ArrayIndex;
    
protected:
    Raster raster;
    Vector data_;
    
    template <class TargetType>
    void import (const RNifti::NiftiImage &source, std::vector<TargetType> &target)
    {
        const RNifti::NiftiImageData sourceData = source.data();
        std::copy(sourceData.begin(), sourceData.end(), target.begin());
    };
    
    template <>
    void import (const RNifti::NiftiImage &source, std::vector<ImageSpace::Vector> &target)
    {
        // We are currently assuming that vector images are 4D with fourth dimension 3 (FSL-style)
        // This is overly restrictive, and in particular doesn't handle NIFTI_INTENT_VECTOR
        if (source.nDims() != 4 || source->nt != 3)
            throw std::runtime_error("NiftiImage source does not seem to be vector-valued");
        
        const RNifti::NiftiImageData sourceData = source.data();
        const size_t volumeSize = source->nx * source->ny * source->nz;
        for (size_t i=0; i<volumeSize; i++)
        {
            ImageSpace::Element elements[3] { sourceData[i], sourceData[i+volumeSize], sourceData[i+2*volumeSize] };
            target[i] = ImageSpace::Vector(elements);
        }
    }
    
public:
    // First argument may implicitly be anything that can initialise a Raster
    explicit Image (const Raster &raster, const Element value = Element())
        : raster(raster), data_(raster.size(),value) {}
    
    explicit Image (const Raster &raster, const std::vector<Element> &data)
        : raster(raster)
    {
        if (raster.size() == data.size())
            this->data_ = data;
        else
            throw std::runtime_error("Data size does not match the specified dimensions");
    }
    
    explicit Image (const RNifti::NiftiImage &source)
    {
        if (source.isNull())
            throw std::runtime_error("NiftiImage source is empty");
        if (source->data == nullptr)
            throw std::runtime_error("NiftiImage source contains no voxel data");
        
        space = new ImageSpace(source);
        raster = Raster(source.dim());
        
        data_.resize(raster.size());
        import(source, data_);
    }
    
    Image (SEXP source)
        : Image(RNifti::NiftiImage(source)) {}
    
    Image () = default;
    
    operator SEXP () const
    {
        Rcpp::RObject object = Rcpp::wrap(data_);
        object.attr("dim") = raster.dim();
        return object;
    }
    
    const Raster & imageRaster () const { return raster; }
    const ArrayIndex & dim () const { return raster.dim(); }
    size_t size () const { return raster.size(); }
    
    const Vector & data () const { return data_; }
    void fill (const Element &value) { data_.assign(data_.size(), value); }
    
    RNifti::NiftiImage toNifti (const int datatype) const
    {
        std::vector<RNifti::NiftiImage::dim_t> dim(raster.dim().begin(), raster.dim().end());
        RNifti::NiftiImage object(dim, datatype);
        if (this->hasImageSpace())
        {
            std::copy(space->pixdim.begin(), space->pixdim.begin() + std::min(Dimensionality,3), &object->pixdim[1]);
            nifti_update_dims_from_array(object);
            object.qform() = space->transform;
            object->qform_code = 2;
        }
        std::copy(data_.begin(), data_.end(), object.data().begin());
        return object;
    }
    
    typename Vector::iterator begin () { return data_.begin(); }
    typename Vector::iterator end () { return data_.end(); }
    typename Vector::const_iterator begin () const { return data_.begin(); }
    typename Vector::const_iterator end () const { return data_.end(); }
    
    typename Vector::reference operator[] (const size_t n) { return data_[n]; }
    typename Vector::reference operator[] (const ArrayIndex &loc) { return data_[raster.flattenIndex(loc)]; }
    
    typename Vector::const_reference operator[] (const size_t n) const { return data_[n]; }
    typename Vector::const_reference operator[] (const ArrayIndex &loc) const { return data_[raster.flattenIndex(loc)]; }
    
    typename Vector::reference at (const size_t n) { return data_.at(n); }
    typename Vector::reference at (const ArrayIndex &loc)
    {
        const ArrayIndex &dims = raster.dim();
        for (int i=0; i<3; i++)
        {
            if (loc[i] >= dims[i])
                throw std::out_of_range("Array index is out of range");
        }
        return data_[raster.flattenIndex(loc)];
    }
    typename Vector::reference at (const ImageSpace::Point &point, const PointType type = PointType::Voxel, const RoundingType round = RoundingType::Conventional)
    {
        if (space == nullptr)
            throw std::runtime_error("No space is associated with the image");
        
        const ImageSpace::Point resolvedPoint = space->toVoxel(point, type, round);
        
        const ArrayIndex &dims = raster.dim();
        ArrayIndex loc;
        for (int i=0; i<3; i++)
        {
            loc[i] = static_cast<size_t>(resolvedPoint[i]);
            if (loc[i] >= dims[i])
                throw std::out_of_range("Array index is out of range");
        }
        return data_[raster.flattenIndex(loc)];
    }
};

#endif
