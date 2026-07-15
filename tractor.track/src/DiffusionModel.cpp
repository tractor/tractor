#include <Rcpp.h>

#include <stdexcept>

#include "DiffusionModel.h"

DiffusionTensorModel::DiffusionTensorModel (const std::string &pdFile)
{
    RNifti::NiftiImage image(pdFile);
    image.reorient("LAS");
    principalDirections = new Image<ImageSpace::Vector,3>(image);
    space = principalDirections->imageSpace();
}

ImageSpace::Vector DiffusionTensorModel::sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
{
    return principalDirections->at(point, PointType::Voxel, RoundingType::Probabilistic);
}

BedpostModel::BedpostModel (const std::vector<std::string> &avfFiles, const std::vector<std::string> &thetaFiles, const std::vector<std::string> &phiFiles)
    : avfThreshold(0.05)
{
    if (avfFiles.size() == 0)
        throw std::invalid_argument("Vectors of BEDPOSTX filenames should not have length zero");
    if (avfFiles.size() != thetaFiles.size() || thetaFiles.size() != phiFiles.size())
        throw std::invalid_argument("Vectors of BEDPOSTX filenames should all have equal length");
    
    nCompartments = avfFiles.size();
    avf.resize(nCompartments);
    theta.resize(nCompartments);
    phi.resize(nCompartments);
    
    for (int i=0; i<nCompartments; i++)
    {
        RNifti::NiftiImage avfImage(avfFiles[i]);
        avf[i] = new Image<float,4>(avfImage.reorient("LAS"));
        RNifti::NiftiImage thetaImage(thetaFiles[i]);
        theta[i] = new Image<float,4>(thetaImage.reorient("LAS"));
        RNifti::NiftiImage phiImage(phiFiles[i]);
        phi[i] = new Image<float,4>(phiImage.reorient("LAS"));
    }
    
    space = avf[0]->imageSpace();
    nSamples = avf[0]->dim()[3];
}

ImageSpace::Vector BedpostModel::sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
{
    // Round the point location and convert to array index
    ImageSpace::Point roundedPoint = space->toVoxel(point, PointType::Voxel, RoundingType::Probabilistic);
    Image<float,4>::ArrayIndex loc;
    for (int i=0; i<3; i++)
        loc[i] = static_cast<size_t>(roundedPoint[i]);
    
    // Randomly choose a sample number
    loc[3] = static_cast<size_t>(round(R::unif_rand() * (nSamples-1)));
    
    // NB: Currently assuming always at least one anisotropic compartment
    ImageSpace::Vector sphericalCoordsStep(1.0);
    int closestIndex = 0;
    float highestInnerProd = -1.0;
    for (int i=0; i<nCompartments; i++)
    {
        // Check AVF is above threshold
        const float currentAvfSample = avf[i]->at(loc);
        if (i == 0 || currentAvfSample >= avfThreshold)
        {
            sphericalCoordsStep[1] = theta[i]->at(loc);
            sphericalCoordsStep[2] = phi[i]->at(loc);
            ImageSpace::Vector stepVector = ImageSpace::sphericalToCartesian(sphericalCoordsStep);
            
            // Use AVF to choose population on first step
            float innerProd;
            if (ImageSpace::norm(referenceDirection) == 0.0)
                innerProd = currentAvfSample;
            else
                innerProd = static_cast<float>(fabs(ImageSpace::dot(stepVector, referenceDirection)));
            
            // If this direction is closer to the reference direction, choose it
            if (innerProd > highestInnerProd && (sphericalCoordsStep[1] != 0.0 || sphericalCoordsStep[2] != 0.0))
            {
                highestInnerProd = innerProd;
                closestIndex = i;
            }
        }
    }
    
    sphericalCoordsStep[1] = theta[closestIndex]->at(loc);
    sphericalCoordsStep[2] = phi[closestIndex]->at(loc);
    
    if (sphericalCoordsStep[1] == 0.0 && sphericalCoordsStep[2] == 0.0)
        return ImageSpace::zeroVector();
    else
        return ImageSpace::sphericalToCartesian(sphericalCoordsStep);
}

SphericalHarmonicModel::SphericalHarmonicModel (const std::string &fodFile, const double amplitudeThreshold)
    : amplitudeThreshold(amplitudeThreshold)
{
    RNifti::NiftiImage image(fodFile);

    // Reorientation does not rotate spherical harmonic coefficients to
    // compensate for a change of axes - each degree l would need its own
    // rotation matrix applied to its own coefficients, not just a permutation
    // or sign flip. So require LAS orientation to for now
    if (image.xform().orientation() != "LAS")
        throw std::runtime_error("Spherical harmonic coefficient images must currently be in LAS orientation");
    
    coefficients = new Image<float,4>(image);
    space = coefficients->imageSpace();
    
    order = SphericalHarmonics::order(coefficients->dim()[3]);
    if (order < 0)
        throw std::invalid_argument("Number of spherical harmonic coefficients does not correspond to a valid even order");
    
    // Subdividing three times gives a fixed set of 642 candidate directions
    // (~7 degree spacing), which findPeaks() then refines to sub-mesh
    // precision by local gradient ascent
    tessellation = new SphereTessellation(3, order);
}

ImageSpace::Vector SphericalHarmonicModel::sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
{
    // Round the point location and convert to array index
    ImageSpace::Point roundedPoint = space->toVoxel(point, PointType::Voxel, RoundingType::Probabilistic);
    Image<float,4>::ArrayIndex loc;
    for (int i=0; i<3; i++)
        loc[i] = static_cast<size_t>(roundedPoint[i]);

    const size_t nCoeffs = coefficients->dim()[3];

    loc[3] = 0;
    const float dcTerm = coefficients->at(loc);

    // The l=0 (isotropic) term of a valid FOD is always nonnegative; a
    // near-zero value indicates a background or masked-out voxel, so bail
    // out cheaply before searching for peaks
    if (dcTerm < 1e-6)
        return ImageSpace::zeroVector();

    std::vector<double> coeffs(nCoeffs);
    coeffs[0] = static_cast<double>(dcTerm);
    for (size_t i=1; i<nCoeffs; i++)
    {
        loc[3] = i;
        coeffs[i] = static_cast<double>(coefficients->at(loc));
    }

    const std::vector<std::pair<ImageSpace::Vector,double>> peaks = tessellation->findPeaks(coeffs);

    // NB: peaks is sorted by decreasing amplitude, so it's safe to stop as
    // soon as we drop below the threshold
    int bestIndex = -1;
    double highestScore = -1.0;
    for (size_t i=0; i<peaks.size(); i++)
    {
        if (peaks[i].second < amplitudeThreshold)
            break;

        const double score = (ImageSpace::norm(referenceDirection) == 0.0)
            ? peaks[i].second
            : static_cast<double>(fabs(ImageSpace::dot(peaks[i].first, referenceDirection)));

        if (score > highestScore)
        {
            highestScore = score;
            bestIndex = static_cast<int>(i);
        }
    }

    if (bestIndex < 0)
        return ImageSpace::zeroVector();

    return peaks[bestIndex].first;
}
