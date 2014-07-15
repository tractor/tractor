#include "RcppArmadillo.h"

#include "Space.h"
#include "DiffusionDataSource.h"

void BedpostDataSource::sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection, float &thetaSample, float &phiSample)
{
    const std::vector<int> &imageDims = avf[0]->getDimensions();
    std::vector<int> newPoint(4);
    
    // Probabilistic trilinear interpolation: select the sample location with probability in proportion to proximity
    for (int i=0; i<3; i++)
    {
        float pointCeiling = ceilf(point[i]);
        float pointFloor = floorf(point[i]);
        
        float distance = point[i] - pointFloor;
        
        float uniformSample = static_cast<float>(R::unif_rand());
        if ((uniformSample > distance && pointFloor >= 0) || pointCeiling >= imageDims[i])
            newPoint[i] = pointFloor;
        else
            newPoint[i] = pointCeiling;
    }
    
    // Randomly choose a sample number
    newPoint[3] = (int) roundf(R::unif_rand() * (imageDims[3]-1));
    
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
            sphericalCoordsStep[0] = 0.0;
            sphericalCoordsStep[1] = (*theta[i])[newPoint];
            sphericalCoordsStep[2] = (*phi[i])[newPoint];
            Space<3>::Vector stepVector = Space<3>::sphericalToCartesian(sphericalCoordsStep);
            
            // Use AVF to choose population on first step
            float innerProd;
            if (norm(referenceDirection,2) == 0.0)
                innerProd = currentAvfSample;
            else
                innerProd = (float) fabs(dot(stepVector, referenceDirection));
            
            // If this direction is closer to the reference direction, choose it
            if (innerProd > highestInnerProd)
            {
                highestInnerProd = innerProd;
                closestIndex = i;
            }
        }
    }
    
    // Set final theta and phi values
    thetaSample = (*theta[closestIndex])[newPoint];
    phiSample = (*phi[closestIndex])[newPoint];
}
