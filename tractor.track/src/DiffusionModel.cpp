#include <RcppEigen.h>

#include "Space.h"
#include "Grid.h"
#include "DiffusionModel.h"

std::vector<int> DiffusionModel::probabilisticRound (const Space<3>::Point &point, const int size) const
{
    std::vector<int> result(size, 0);
    const Eigen::Array3i imageDims = grid.dimensions();
    
    // Probabilistic trilinear interpolation: select the sample location with probability in proportion to proximity
    for (int i=0; i<3; i++)
    {
        float pointCeiling = ceil(point[i]);
        float pointFloor = floor(point[i]);
        
        float distance = point[i] - pointFloor;
        
        float uniformSample = static_cast<float>(R::unif_rand());
        if ((uniformSample > distance && pointFloor >= 0.0) || pointCeiling >= static_cast<float>(imageDims(i,0)))
            result[i] = static_cast<int>(pointFloor);
        else
            result[i] = static_cast<int>(pointCeiling);
    }
    
    return result;
}

DiffusionTensorModel::DiffusionTensorModel (const std::string &pdFile)
{
    RNifti::NiftiImage image(pdFile);
    
    const std::vector<int> &imageDims = image.dim();
    if (imageDims.size() != 4 || imageDims[3] != 3)
        throw std::runtime_error("Principal direction image does not seem to be vector-valued");
    
    image.reorient("LAS");
    grid = ::getGrid3D(image);
    principalDirections = getImageArray<float>(image);
}

Space<3>::Vector DiffusionTensorModel::sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection) const
{
    std::vector<int> roundedPoint = probabilisticRound(point, 4);
    Space<3>::Vector stepVector;
    
    for (int i=0; i<3; i++)
    {
        roundedPoint[3] = i;
        stepVector[i] = (*principalDirections)[roundedPoint];
    }
    
    return stepVector;
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
    
    grid = ::getGrid3D(RNifti::NiftiImage(avfFiles[0],false).reorient("LAS"));
    
    for (int i=0; i<nCompartments; i++)
    {
        avf[i] = getImageArray<float>(RNifti::NiftiImage(avfFiles[i]).reorient("LAS"));
        theta[i] = getImageArray<float>(RNifti::NiftiImage(thetaFiles[i]).reorient("LAS"));
        phi[i] = getImageArray<float>(RNifti::NiftiImage(phiFiles[i]).reorient("LAS"));
    }
    
    const std::vector<int> &avfDims = avf[0]->getDimensions();
    if (avfDims.size() != 4)
        throw std::runtime_error("AVF sample image does not seem to be 4D");
    nSamples = avfDims[3];
}

Space<3>::Vector BedpostModel::sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection) const
{
    std::vector<int> roundedPoint = probabilisticRound(point, 4);
    
    // Randomly choose a sample number
    roundedPoint[3] = static_cast<int>(round(R::unif_rand() * (nSamples-1)));
    
    // NB: Currently assuming always at least one anisotropic compartment
    int closestIndex = 0;
    float highestInnerProd = -1.0;
    for (int i=0; i<nCompartments; i++)
    {
        // Check AVF is above threshold
        float currentAvfSample = (*avf[i])[roundedPoint];
        if (i == 0 || currentAvfSample >= avfThreshold)
        {
            Space<3>::Vector sphericalCoordsStep;
            sphericalCoordsStep[0] = 1.0;
            sphericalCoordsStep[1] = (*theta[i])[roundedPoint];
            sphericalCoordsStep[2] = (*phi[i])[roundedPoint];
            Space<3>::Vector stepVector = Space<3>::sphericalToCartesian(sphericalCoordsStep);
            
            // Use AVF to choose population on first step
            float innerProd;
            if (Space<3>::zeroVector(referenceDirection))
                innerProd = currentAvfSample;
            else
                innerProd = static_cast<float>(fabs(stepVector.dot(referenceDirection)));
            
            // If this direction is closer to the reference direction, choose it
            if (innerProd > highestInnerProd && (sphericalCoordsStep[1] != 0.0 || sphericalCoordsStep[2] != 0.0))
            {
                highestInnerProd = innerProd;
                closestIndex = i;
            }
        }
    }
    
    Space<3>::Vector sphericalCoordsStep;
    sphericalCoordsStep[0] = 1.0;
    sphericalCoordsStep[1] = (*theta[closestIndex])[roundedPoint];
    sphericalCoordsStep[2] = (*phi[closestIndex])[roundedPoint];
    
    Space<3>::Vector stepVector;
    if (sphericalCoordsStep[1] == 0.0 && sphericalCoordsStep[2] == 0.0)
        stepVector = Space<3>::zeroVector();
    else
        stepVector = Space<3>::sphericalToCartesian(sphericalCoordsStep);
    return stepVector;
}
