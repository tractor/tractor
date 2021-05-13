#ifndef _GRID_H_
#define _GRID_H_

#include <RcppEigen.h>

#include "RNifti.h"

template <int Dimensionality>
class Grid
{
public:
    typedef typename Eigen::Matrix<float,Dimensionality+1,Dimensionality+1> TransformMatrix;
    
protected:
    Eigen::Array<int,Dimensionality,1> dims;
    Eigen::Array<float,Dimensionality,1> spac;
    TransformMatrix xfm;
    
public:
    Grid () {}
    
    Grid (const Eigen::Array<int,Dimensionality,1> &dims)
        : dims(dims)
    {
        spac = Eigen::Array<float,Dimensionality,1>::Ones();
        xfm = TransformMatrix::Identity();
    }
    
    Grid (const Eigen::Array<int,Dimensionality,1> &dims, const Eigen::Array<float,Dimensionality,1> &spacings)
        : dims(dims), spac(spacings)
    {
        xfm = TransformMatrix::Identity();
        for (int i=0; i<Dimensionality; i++)
            xfm(i,i) = spacings[i];
    }
        
    Grid (const Eigen::Array<int,Dimensionality,1> &dims, const TransformMatrix &transform)
        : dims(dims), xfm(transform)
    {
        for (int i=0; i<Dimensionality; i++)
            spac[i] = transform.col(i).norm();
    }
    
    Grid (const Eigen::Array<int,Dimensionality,1> &dims, const Eigen::Array<float,Dimensionality,1> &spacings, const TransformMatrix &transform)
        : dims(dims), spac(spacings), xfm(transform) {}
    
    const Eigen::Array<int,Dimensionality,1> & dimensions () const { return dims; }
    const Eigen::Array<float,Dimensionality,1> & spacings () const { return spac; }
    const TransformMatrix & transform () const { return xfm; }
    
    Eigen::Array<int,Dimensionality,1> & dimensions () { return dims; }
    Eigen::Array<float,Dimensionality,1> & spacings () { return spac; }
    TransformMatrix & transform () { return xfm; }
};

class Griddable3D
{
public:
    virtual Grid<3> getGrid3D () const = 0;
};

inline Grid<3> getGrid3D (const Griddable3D &object)
{
    return object.getGrid3D();
}

inline Grid<3> getGrid3D (const RNifti::NiftiImage &object)
{
    Eigen::Array3i dims = Eigen::Array3i::Ones();
    Eigen::Array3f spacings = Eigen::Array3f::Zero();
    RNifti::NiftiImage::Xform::Matrix matrix = object.xform().matrix();
    Eigen::Matrix4f xform = Eigen::Map<Eigen::Matrix4f>(matrix.begin()).transpose();
    for (int i=0; i<std::min(3,object.nDims()); i++)
    {
        dims(i,0) = object->dim[i+1];
        spacings(i,0) = object->pixdim[i+1];
    }
    return Grid<3>(dims, spacings, xform);
}

inline std::string grid3DOrientation (const Grid<3> &grid)
{
    const Grid<3>::TransformMatrix transform = grid.transform();
    RNifti::NiftiImage::Xform::Matrix matrix;
    for (int i=0; i<4; i++)
    {
        for (int j=0; j<4; j++)
            matrix(i,j) = transform(i,j);
    }
    
    return RNifti::NiftiImage::Xform(matrix).orientation();
}

#endif
