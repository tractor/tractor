#ifndef _GRID_H_
#define _GRID_H_

#include <RcppEigen.h>

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

#endif
