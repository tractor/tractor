#ifndef _SPACE_H_
#define _SPACE_H_

#include <RcppArmadillo.h>

template <int Dimensionality, typename DataType = float> class Space
{
public:
    // This syntax boggles my mind, but seems to be correct...
    typedef typename arma::Col<DataType>::template fixed<Dimensionality> Point;
    typedef typename arma::Col<DataType>::template fixed<Dimensionality> Vector;
    
    static const Vector zeroVector ()
    {
        Vector zero;
        zero.zeros();
        return zero;
    }
    
    static bool zeroVector (const Vector &vector)
    {
        return (arma::norm(vector,2) == 0.0);
    }
    
    static const Vector sphericalToCartesian (const Vector &spherical)
    {
        if (Dimensionality != 3)
            throw new std::domain_error("Spherical coordinates are only defined in 3D spaces");
        
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
