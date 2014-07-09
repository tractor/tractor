#ifndef _SPACE_H_
#define _SPACE_H_

#include "RcppArmadillo.h"

template <int Dimensionality> struct Space
{
    typedef arma::fvec::fixed<Dimensionality> Point;
    typedef arma::fvec::fixed<Dimensionality> Vector;
};

#endif
