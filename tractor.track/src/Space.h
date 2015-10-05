#ifndef _SPACE_H_
#define _SPACE_H_

#include <RcppEigen.h>

template <int Dimensionality, typename DataType = float> class Space
{
public:
    typedef typename Eigen::Array<DataType,Dimensionality,1> Point;
    typedef typename Eigen::Matrix<DataType,Dimensionality,1> Vector;
    
    static const Vector zeroVector ()
    {
        return Vector::Zero();
    }
    
    static bool zeroVector (const Vector &vector)
    {
        return (vector.norm() == 0.0);
    }
    
    static const Vector sphericalToCartesian (const Vector &spherical)
    {
        if (Dimensionality != 3)
            throw std::domain_error("Spherical coordinates are only defined in 3D spaces");
        
        Vector cartesian;
        cartesian[0] = spherical[0] * sin(spherical[1]) * cos(spherical[2]);
        cartesian[1] = spherical[0] * sin(spherical[1]) * sin(spherical[2]);
        cartesian[2] = spherical[0] * cos(spherical[1]);
        return cartesian;
    }
};

// template <> class Space<3>
// {
// public:
//     static const Vector sphericalToCartesian (const Vector &spherical)
//     {
//         Vector cartesian;
//         cartesian[0] = spherical[0] * sin(spherical[1]) * cos(spherical[2]);
//         cartesian[1] = spherical[0] * sin(spherical[1]) * sin(spherical[2]);
//         cartesian[2] = spherical[0] * cos(spherical[1]);
//         return cartesian;
//     }
// };

#endif
