#ifndef _SPHERICAL_HARMONICS_H_
#define _SPHERICAL_HARMONICS_H_

#include "Image.h"

#include <utility>
#include <vector>

// Real, even-order spherical harmonic basis functions, following the MRtrix3
// storage and normalisation convention (see
// https://mrtrix.readthedocs.io/en/latest/concepts/spherical_harmonics.html).
// Coefficients are stored for even degrees l = 0, 2, 4, ..., order, and
// within each degree in order of increasing m from -l to +l, at storage
// index V = l(l+1)/2 + m. The associated Legendre functions used here do NOT
// include the Condon-Shortley (-1)^m phase, matching MRtrix's convention;
// this must not be "corrected" against textbook definitions that do include
// it, or every odd-m coefficient would take the wrong sign
namespace SphericalHarmonics
{
    // The even SH order implied by a coefficient count, or -1 if invalid
    int order (const size_t n);
    
    // The number of coefficients for the given even SH order
    size_t nCoefficients (const int order);
    
    // The values of every basis function up to the given even order, at the
    // specified (not necessarily normalised) direction, in storage order
    std::vector<double> basis (const int order, const ImageSpace::Vector &direction);
}

// A fixed, roughly-uniform set of directions covering the whole sphere,
// obtained by recursive subdivision of an icosahedron, along with vertex
// adjacency and precomputed spherical harmonic basis values, used to locate
// and refine the peaks (local maxima) of an even-order real SH series, i.e.,
// an FOD
class SphereTessellation
{
private:
    std::vector<ImageSpace::Vector> directions_;
    std::vector<std::vector<int>> neighbours_;
    std::vector<std::vector<double>> basisValues_;
    int order_;
    
    void refinePeak (ImageSpace::Vector &direction, double &amplitude, const std::vector<double> &coefficients) const;
    
public:
    SphereTessellation (const int subdivisions, const int order);
    
    const std::vector<ImageSpace::Vector> & directions () const { return directions_; }
    
    // The peaks of the FOD described by "coefficients" - local maxima of the
    // discretised amplitude, refined to subpixel (sub-mesh) precision by a
    // few steps of gradient ascent in the local tangent plane - as
    // (direction,amplitude) pairs, sorted by decreasing amplitude. Antipodal
    // duplicates (inevitable since only even orders are represented) are
    // collapsed to a single canonical hemisphere
    std::vector<std::pair<ImageSpace::Vector,double>> findPeaks (const std::vector<double> &coefficients) const;
};

#endif
