#include <RcppEigen.h>

#include "Space.h"
#include "Grid.h"
#include "DiffusionModel.h"

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
    
    grid = NiftiImage(avfFiles[0],false).getGrid3D();
    
    for (int i=0; i<nCompartments; i++)
    {
        avf[i] = NiftiImage(avfFiles[i]).getData<float>();
        theta[i] = NiftiImage(thetaFiles[i]).getData<float>();
        phi[i] = NiftiImage(phiFiles[i]).getData<float>();
    }
    
    const std::vector<int> &avfDims = avf[0]->getDimensions();
    if (avfDims.size() != 4)
        throw std::runtime_error("AVF sample image does not seem to be 4D");
    nSamples = avfDims[3];
}

Space<3>::Vector BedpostModel::sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection) const
{
    std::vector<int> newPoint(4);
    const Eigen::Array3i imageDims = grid.dimensions();
    
    // Probabilistic trilinear interpolation: select the sample location with probability in proportion to proximity
    for (int i=0; i<3; i++)
    {
        float pointCeiling = ceil(point[i]);
        float pointFloor = floor(point[i]);
        
        float distance = point[i] - pointFloor;
        
        float uniformSample = static_cast<float>(R::unif_rand());
        if ((uniformSample > distance && pointFloor >= 0.0) || pointCeiling >= static_cast<float>(imageDims(i,0)))
            newPoint[i] = static_cast<int>(pointFloor);
        else
            newPoint[i] = static_cast<int>(pointCeiling);
    }
    
    // Randomly choose a sample number
    newPoint[3] = static_cast<int>(round(R::unif_rand() * (nSamples-1)));
    
    // NB: Currently assuming always at least one anisotropic compartment
    int closestIndex = 0;
    float highestInnerProd = -1.0;
    for (int i=0; i<nCompartments; i++)
    {
        // Check AVF is above threshold
        float currentAvfSample = (*avf[i])[newPoint];
        if (i == 0 || currentAvfSample >= avfThreshold)
        {
            Space<3>::Vector sphericalCoordsStep;
            sphericalCoordsStep[0] = 1.0;
            sphericalCoordsStep[1] = (*theta[i])[newPoint];
            sphericalCoordsStep[2] = (*phi[i])[newPoint];
            Space<3>::Vector stepVector = Space<3>::sphericalToCartesian(sphericalCoordsStep);
            
            // Use AVF to choose population on first step
            float innerProd;
            if (Space<3>::zeroVector(referenceDirection))
                innerProd = currentAvfSample;
            else
                innerProd = static_cast<float>(fabs(stepVector.dot(referenceDirection)));
            
            // If this direction is closer to the reference direction, choose it
            if (innerProd > highestInnerProd)
            {
                highestInnerProd = innerProd;
                closestIndex = i;
            }
        }
    }
    
    Space<3>::Vector sphericalCoordsStep;
    sphericalCoordsStep[0] = 1.0;
    sphericalCoordsStep[1] = (*theta[closestIndex])[newPoint];
    sphericalCoordsStep[2] = (*phi[closestIndex])[newPoint];
    Space<3>::Vector stepVector = Space<3>::sphericalToCartesian(sphericalCoordsStep);
    return stepVector;
}
