#include "SphericalHarmonics.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <map>
#include <stdexcept>

namespace {

const double pi = 3.14159265358979323846;

ImageSpace::Vector normalise (const ImageSpace::Vector &v)
{
    const double length = ImageSpace::norm(v);
    ImageSpace::Vector result;
    for (int i=0; i<3; i++)
        result[i] = v[i] / length;
    return result;
}

ImageSpace::Vector cross (const ImageSpace::Vector &a, const ImageSpace::Vector &b)
{
    ImageSpace::Vector result;
    result[0] = a[1]*b[2] - a[2]*b[1];
    result[1] = a[2]*b[0] - a[0]*b[2];
    result[2] = a[0]*b[1] - a[1]*b[0];
    return result;
}

// A point displaced from "direction" within the tangent plane spanned by the
// (assumed orthonormal, and orthogonal to "direction") vectors tangent1 and
// tangent2, renormalised back onto the unit sphere
ImageSpace::Vector tangentStep (const ImageSpace::Vector &direction, const ImageSpace::Vector &tangent1, const ImageSpace::Vector &tangent2, const double coeff1, const double coeff2)
{
    ImageSpace::Vector result;
    for (int i=0; i<3; i++)
        result[i] = direction[i] + coeff1*tangent1[i] + coeff2*tangent2[i];
    return normalise(result);
}

// Flip the sign of "direction" if necessary so that it lies in a fixed
// canonical hemisphere, chosen by the sign of its largest-magnitude
// component - this collapses the antipodal ambiguity inherent in an
// even-order-only (antipodally symmetric) SH series
void canonicalise (ImageSpace::Vector &direction)
{
    int largest = 0;
    for (int i=1; i<3; i++)
    {
        if (fabs(direction[i]) > fabs(direction[largest]))
            largest = i;
    }
    if (direction[largest] < 0.0)
    {
        for (int i=0; i<3; i++)
            direction[i] = -direction[i];
    }
}

double evaluateAmplitude (const int order, const ImageSpace::Vector &direction, const std::vector<double> &coefficients)
{
    const std::vector<double> values = SphericalHarmonics::basis(order, direction);
    double amplitude = 0.0;
    for (size_t i=0; i<coefficients.size(); i++)
        amplitude += values[i] * coefficients[i];
    return amplitude;
}

// Invert a 6x6 matrix in place via Gauss-Jordan elimination with partial
// pivoting. Returns false if the matrix is (numerically) singular, in which
// case "matrix" is left in an unspecified state
bool invert6x6 (std::array<std::array<double,6>,6> &matrix)
{
    std::array<std::array<double,6>,6> result;
    for (int i=0; i<6; i++)
    {
        for (int j=0; j<6; j++)
            result[i][j] = (i == j) ? 1.0 : 0.0;
    }
    
    for (int col=0; col<6; col++)
    {
        int pivotRow = col;
        double pivotValue = fabs(matrix[col][col]);
        for (int row=col+1; row<6; row++)
        {
            if (fabs(matrix[row][col]) > pivotValue)
            {
                pivotRow = row;
                pivotValue = fabs(matrix[row][col]);
            }
        }
        if (pivotValue < 1e-12)
            return false;
        
        if (pivotRow != col)
        {
            std::swap(matrix[pivotRow], matrix[col]);
            std::swap(result[pivotRow], result[col]);
        }
        
        const double pivot = matrix[col][col];
        for (int j=0; j<6; j++)
        {
            matrix[col][j] /= pivot;
            result[col][j] /= pivot;
        }
        
        for (int row=0; row<6; row++)
        {
            if (row == col)
                continue;
            const double factor = matrix[row][col];
            if (factor == 0.0)
                continue;
            for (int j=0; j<6; j++)
            {
                matrix[row][j] -= factor * matrix[col][j];
                result[row][j] -= factor * result[col][j];
            }
        }
    }

    matrix = result;
    return true;
}

// The least-squares operator mapping amplitude samples at a set of points
// (tangentCoords[0] is always the vertex itself, at (0,0); the rest are its
// neighbours) onto the 6 coefficients [a,b,c,d,e,f] of a quadratic surface
// a + b*u + c*w + d*u^2 + e*u*w + f*w^2 fitted through them, flattened as a
// 6-by-nRows matrix. Depends only on the (fixed) tangent-plane geometry of
// the points, not on any voxel data, so it can be precomputed once and
// reused for every voxel's peak refinement. If the fit is degenerate, the
// zero matrix is returned; this yields d=0 downstream, which fails the
// negative-definite check in refinePeak() and so is handled safely
std::vector<double> quadraticFitOperator (const std::vector<std::array<double,2>> &tangentCoords)
{
    const size_t rows = tangentCoords.size();
    std::vector<std::array<double,6>> design(rows);
    for (size_t r=0; r<rows; r++)
    {
        const double u = tangentCoords[r][0], w = tangentCoords[r][1];
        design[r] = { 1.0, u, w, u*u, u*w, w*w };
    }
    
    std::array<std::array<double,6>,6> normalMatrix;
    for (int i=0; i<6; i++)
    {
        for (int j=0; j<6; j++)
        {
            double sum = 0.0;
            for (size_t r=0; r<rows; r++)
                sum += design[r][i] * design[r][j];
            normalMatrix[i][j] = sum;
        }
    }
    
    std::vector<double> operatorMatrix(6 * rows, 0.0);
    if (invert6x6(normalMatrix))
    {
        for (int i=0; i<6; i++)
        {
            for (size_t r=0; r<rows; r++)
            {
                double sum = 0.0;
                for (int j=0; j<6; j++)
                    sum += normalMatrix[i][j] * design[r][j];
                operatorMatrix[i*rows + r] = sum;
            }
        }
    }
    
    return operatorMatrix;
}

// Associated Legendre function P_l^m(x), for m >= 0 and l >= m, WITHOUT the
// Condon-Shortley phase (matching MRtrix's convention), computed via the
// standard three-term recurrence, filled bottom-up over a small table (not
// the naive recursive definition, which has exponential complexity)
double associatedLegendre (const int l, const int m, const double x)
{
    const double sinTheta = sqrt(std::max(0.0, 1.0 - x*x));
    
    // table[k] holds P_{m+k}^m
    std::vector<double> table(l - m + 1);
    
    // Diagonal term: P_m^m = (2m-1)!! * sin(theta)^m
    double doubleFactorial = 1.0;
    for (int k=2*m-1; k>1; k-=2)
        doubleFactorial *= k;
    table[0] = doubleFactorial * pow(sinTheta, m);
    
    if (l > m)
    {
        // Next term: P_{m+1}^m = (2m+1) * x * P_m^m
        table[1] = (2*m + 1) * x * table[0];
        
        // General recurrence: (l-m) P_l^m = (2l-1) x P_{l-1}^m - (l+m-1) P_{l-2}^m
        for (int k=m+2; k<=l; k++)
            table[k-m] = ((2*k-1) * x * table[k-m-1] - (k+m-1) * table[k-m-2]) / (k-m);
    }
    
    return table[l-m];
}

// Ratio (l-m)! / (l+m)!, computed as a running product to avoid factorial
// overflow for larger l
double legendreNormalisationRatio (const int l, const int m)
{
    double ratio = 1.0;
    for (int k=l-m+1; k<=l+m; k++)
        ratio /= k;
    return ratio;
}

} // anonymous namespace

int SphericalHarmonics::order (const size_t n)
{
    for (int l=0; l<=32; l+=2)
    {
        if (SphericalHarmonics::nCoefficients(l) == n)
            return l;
    }
    return -1;
}

size_t SphericalHarmonics::nCoefficients (const int order)
{
    return static_cast<size_t>((order+1) * (order+2) / 2);
}

std::vector<double> SphericalHarmonics::basis (const int order, const ImageSpace::Vector &direction)
{
    const double length = ImageSpace::norm(direction);
    const double x = direction[0] / length, y = direction[1] / length, z = direction[2] / length;
    
    // MRtrix's convention measures colatitude theta from the NEGATIVE z axis
    // (confirmed empirically by cross-checking reconstructed FOD amplitudes
    // and sh2peaks output against this implementation - using cosTheta = z
    // instead reproduces the correct amplitude only in the x/y plane, and
    // silently evaluates the wrong point everywhere else)
    const double cosTheta = -z;
    const double phi = atan2(y, x);
    
    std::vector<double> result(SphericalHarmonics::nCoefficients(order));
    
    for (int l=0; l<=order; l+=2)
    {
        const int base = l * (l+1) / 2;
        
        result[base] = sqrt((2*l+1) / (4*pi)) * associatedLegendre(l, 0, cosTheta);
        
        for (int m=1; m<=l; m++)
        {
            const double normalisation = sqrt(2.0 * (2*l+1) / (4*pi) * legendreNormalisationRatio(l,m));
            const double legendreValue = associatedLegendre(l, m, cosTheta);
            
            result[base + m] = normalisation * legendreValue * cos(m * phi);
            result[base - m] = normalisation * legendreValue * sin(m * phi);
        }
    }
    
    return result;
}

SphereTessellation::SphereTessellation (const int subdivisions, const int order)
    : order_(order)
{
    const double t = (1.0 + sqrt(5.0)) / 2.0;
    const double baseCoords[12][3] = {
        {-1, t, 0}, {1, t, 0}, {-1,-t, 0}, {1,-t, 0},
        {0,-1, t}, {0, 1, t}, {0,-1,-t}, {0, 1,-t},
        { t, 0,-1}, { t, 0, 1}, {-t, 0,-1}, {-t, 0, 1}
    };
    
    directions_.resize(12);
    for (int i=0; i<12; i++)
    {
        ImageSpace::Vector v;
        v[0] = baseCoords[i][0];
        v[1] = baseCoords[i][1];
        v[2] = baseCoords[i][2];
        directions_[i] = normalise(v);
    }
    
    std::vector<std::array<int,3>> faces = {
        {0,11,5}, {0,5,1}, {0,1,7}, {0,7,10}, {0,10,11},
        {1,5,9}, {5,11,4}, {11,10,2}, {10,7,6}, {7,1,8},
        {3,9,4}, {3,4,2}, {3,2,6}, {3,6,8}, {3,8,9},
        {4,9,5}, {2,4,11}, {6,2,10}, {8,6,7}, {9,8,1}
    };
    
    for (int s=0; s<subdivisions; s++)
    {
        std::map<std::pair<int,int>,int> midpointCache;
        
        auto midpoint = [&] (int a, int b) -> int
        {
            const std::pair<int,int> key = (a < b) ? std::make_pair(a,b) : std::make_pair(b,a);
            auto it = midpointCache.find(key);
            if (it != midpointCache.end())
                return it->second;
            
            ImageSpace::Vector v;
            for (int i=0; i<3; i++)
                v[i] = (directions_[a][i] + directions_[b][i]) / 2.0;
            directions_.push_back(normalise(v));
            
            const int index = static_cast<int>(directions_.size()) - 1;
            midpointCache[key] = index;
            return index;
        };
        
        std::vector<std::array<int,3>> newFaces;
        newFaces.reserve(faces.size() * 4);
        for (const auto &face : faces)
        {
            const int ab = midpoint(face[0], face[1]);
            const int bc = midpoint(face[1], face[2]);
            const int ca = midpoint(face[2], face[0]);
            newFaces.push_back({face[0], ab, ca});
            newFaces.push_back({face[1], bc, ab});
            newFaces.push_back({face[2], ca, bc});
            newFaces.push_back({ab, bc, ca});
        }
        faces = std::move(newFaces);
    }
    
    neighbours_.resize(directions_.size());
    auto addNeighbour = [&] (int a, int b)
    {
        if (std::find(neighbours_[a].begin(), neighbours_[a].end(), b) == neighbours_[a].end())
            neighbours_[a].push_back(b);
    };
    for (const auto &face : faces)
    {
        addNeighbour(face[0], face[1]); addNeighbour(face[1], face[0]);
        addNeighbour(face[1], face[2]); addNeighbour(face[2], face[1]);
        addNeighbour(face[2], face[0]); addNeighbour(face[0], face[2]);
    }
    
    basisValues_.resize(directions_.size());
    for (size_t i=0; i<directions_.size(); i++)
        basisValues_[i] = SphericalHarmonics::basis(order_, directions_[i]);
    
    // Precompute, for every vertex, an orthonormal tangent-plane basis and
    // the least-squares operator that will later turn amplitude samples at
    // that vertex and its neighbours into a local quadratic fit - all of
    // this depends only on the mesh geometry, not on any voxel's data, so
    // it is done once here rather than on every call to findPeaks()
    tangent1_.resize(directions_.size());
    tangent2_.resize(directions_.size());
    quadraticFit_.resize(directions_.size());
    for (size_t i=0; i<directions_.size(); i++)
    {
        const ImageSpace::Vector &v0 = directions_[i];
        ImageSpace::Vector reference(0.0);
        reference[(fabs(v0[0]) < 0.9) ? 0 : 1] = 1.0;
        tangent1_[i] = normalise(cross(v0, reference));
        tangent2_[i] = cross(v0, tangent1_[i]);
        
        std::vector<std::array<double,2>> tangentCoords;
        tangentCoords.push_back({0.0, 0.0});
        for (const int j : neighbours_[i])
        {
            const double u = ImageSpace::dot(directions_[j], tangent1_[i]);
            const double w = ImageSpace::dot(directions_[j], tangent2_[i]);
            tangentCoords.push_back({u, w});
        }
        
        quadraticFit_[i] = quadraticFitOperator(tangentCoords);
    }
}

void SphereTessellation::refinePeak (int vertexIndex, ImageSpace::Vector &direction, double &amplitude, const std::vector<double> &amplitudeField, const std::vector<double> &coefficients) const
{
    const std::vector<int> &neighbours = neighbours_[vertexIndex];
    const std::vector<double> &fitOperator = quadraticFit_[vertexIndex];
    const size_t rows = neighbours.size() + 1;
    
    std::vector<double> samples(rows);
    samples[0] = amplitudeField[vertexIndex];
    for (size_t j=0; j<neighbours.size(); j++)
        samples[j+1] = amplitudeField[neighbours[j]];
    
    // Apply the precomputed fitting operator: this is the only work done
    // per peak besides the final polish below - no spherical harmonic
    // basis evaluations are needed, since the input amplitudes were already
    // computed (for every mesh vertex) by findPeaks()'s coarse scan
    double coeffs[6] = {0.0,0.0,0.0,0.0,0.0,0.0};
    for (int i=0; i<6; i++)
    {
        double sum = 0.0;
        for (size_t r=0; r<rows; r++)
            sum += fitOperator[i*rows + r] * samples[r];
        coeffs[i] = sum;
    }
    
    const double b=coeffs[1], c=coeffs[2], d=coeffs[3], e=coeffs[4], f=coeffs[5];
    const double det = 4.0*d*f - e*e;
    
    // The stationary point of a + b*u + c*w + d*u^2 + e*u*w + f*w^2 is a
    // genuine local maximum only if its Hessian [[2d,e],[e,2f]] is negative
    // definite (d<0 and det>0); otherwise the fit is untrustworthy (e.g. a
    // saddle, or a near-flat/degenerate configuration) and the coarse mesh
    // vertex is kept as is - a safe, if less precise, fallback
    if (d < 0.0 && det > 1e-12)
    {
        double u = (e*c - 2.0*b*f) / det;
        double w = (b*e - 2.0*c*d) / det;
        
        // Distrust the fit if its stationary point lies well beyond the
        // neighbouring vertices used to construct it - a quadratic is only
        // a local approximation to the true (smooth) FOD amplitude surface
        const double radius = sqrt(u*u + w*w);
        const double maxRadius = 0.15;   // radians; a little over one mesh spacing
        if (radius > maxRadius)
        {
            u *= maxRadius / radius;
            w *= maxRadius / radius;
        }
        
        direction = tangentStep(direction, tangent1_[vertexIndex], tangent2_[vertexIndex], u, w);
    }
    
    // A single fresh evaluation gives an accurate amplitude at the
    // (possibly refined) direction, rather than trusting the quadratic
    // fit's own estimate - cheap, since it replaces what was previously 32
    // such evaluations per peak (8 gradient-ascent iterations x 4 finite
    // difference samples)
    amplitude = evaluateAmplitude(order_, direction, coefficients);
}

std::vector<std::pair<ImageSpace::Vector,double>> SphereTessellation::findPeaks (const std::vector<double> &coefficients) const
{
    if (coefficients.size() != SphericalHarmonics::nCoefficients(order_))
        throw std::invalid_argument("Coefficient vector length does not match the tessellation's spherical harmonic order");
    
    const size_t n = directions_.size();
    std::vector<double> amplitude(n);
    for (size_t i=0; i<n; i++)
    {
        double value = 0.0;
        for (size_t j=0; j<coefficients.size(); j++)
            value += basisValues_[i][j] * coefficients[j];
        amplitude[i] = value;
    }
    
    std::vector<int> maximaIndices;
    for (size_t i=0; i<n; i++)
    {
        if (amplitude[i] <= 0.0)
            continue;
        
        bool isMaximum = true;
        for (const int j : neighbours_[i])
        {
            if (amplitude[j] > amplitude[i])
            {
                isMaximum = false;
                break;
            }
        }
        
        if (isMaximum)
            maximaIndices.push_back(static_cast<int>(i));
    }
    
    std::vector<std::pair<ImageSpace::Vector,double>> peaks;
    for (const int idx : maximaIndices)
    {
        ImageSpace::Vector direction = directions_[idx];
        double amp = amplitude[idx];
        refinePeak(idx, direction, amp, amplitude, coefficients);
        peaks.push_back(std::make_pair(direction, amp));
    }
    
    for (auto &peak : peaks)
        canonicalise(peak.first);
    
    std::sort(peaks.begin(), peaks.end(), [] (const std::pair<ImageSpace::Vector,double> &a, const std::pair<ImageSpace::Vector,double> &b) { return a.second > b.second; });
    
    // Antipodal peaks (and near-duplicates surviving discretisation) collapse
    // to a single entry, keeping the higher-amplitude (post-refinement) copy
    std::vector<std::pair<ImageSpace::Vector,double>> result;
    for (const auto &peak : peaks)
    {
        bool isDuplicate = false;
        for (const auto &existing : result)
        {
            if (ImageSpace::dot(peak.first, existing.first) > 0.999)
            {
                isDuplicate = true;
                break;
            }
        }
        if (!isDuplicate)
            result.push_back(peak);
    }
    
    return result;
}
