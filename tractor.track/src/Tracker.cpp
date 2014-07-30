#include <RcppArmadillo.h>

#include "Tracker.h"

Streamline Tracker::run (const int maxSteps)
{
    const std::vector<int> &spaceDims = mask->getDimensions();
    const std::vector<float> &voxelDims = mask->getVoxelDimensions();
    
    if (visited == NULL)
        visited = new Array<bool>(spaceDims, false);
    else
        visited->fill(false);
    
    if (flags["loopcheck"])
    {
        if (loopcheck == NULL)
        {
            std::vector<int> loopcheckDims(3);
            for (int i=0; i<3; i++)
                loopcheckDims[i] = static_cast<int>(round(spaceDims[i] / LOOPCHECK_RATIO)) + 1;
            loopcheck = new Array<Space<3>::Vector>(loopcheckDims, Space<3>::zeroVector());
        }
        else
            loopcheck->fill(Space<3>::zeroVector());
    }
    
    bool starting = true;
    bool rightwardsVectorValid = !Space<3>::zeroVector(rightwardsVector);
    int timesLeftMask = 0;
    float sign;
    Space<3>::Point loc;
    std::vector<int> roundedLoc(3), loopcheckLoc(3);
    size_t vectorLoc;
    Space<3>::Vector firstStep;
    Space<3>::Vector previousStep = Space<3>::zeroVector();
    
    std::vector<Space<3>::Point> leftPoints, rightPoints;
    
    // We go right first (dir=0), then left (dir=1)
    for (int dir=0; dir<2; dir++)
    {
        loc = seed;
        if (rightwardsVectorValid)
            previousStep = rightwardsVector * (dir==0 ? 1 : -1);
        
        bool leftMask = false;
        int previouslyInsideMask = -1;
        bool terminateOnNextStep = false;
        
        // Run the tracking
        for (int step=0; step<(maxSteps/2); step++)
        {
            if (terminateOnNextStep)
                break;
            
            // Check that the current step location is in bounds
            bool inBounds = true;
            for (int i=0; i<3; i++)
            {
                roundedLoc[i] = static_cast<int>(round(loc[i]));
                if (roundedLoc[i] < 0 || roundedLoc[i] > spaceDims[i] - 1)
                {
                    inBounds = false;
                    break;
                }
            }
            if (!inBounds)
                break;
            
            // Index for current location
            visited->flattenIndex(roundedLoc, vectorLoc);
            
            if (starting && (*mask)[vectorLoc] == 0)
                timesLeftMask++;
            
            // Stop if we've stepped outside the mask, possibly deferring termination if required
            if ((*mask)[vectorLoc] == 0 && previouslyInsideMask == 1)
            {
                leftMask = true;
                timesLeftMask++;
                
                if (flags["terminate-outside"])
                    terminateOnNextStep = true;
                else
                    break;
            }
            previouslyInsideMask = ((*mask)[vectorLoc] == 0 ? 0 : 1);
            
            // Mark visit
            if (!(*visited)[vectorLoc])
            {
                (*visited)[vectorLoc] = true;
                // if (flags["visitation-map"])
                //     visitationCounts[vectorLoc]++;
            }
            
            // Store current (unrounded) location if required
            // NB: This part of the code must always be reached at the seed point
            if (dir == 0)
                rightPoints.push_back(loc);
            else
                leftPoints.push_back(loc);
            
            // Sample a direction for the current step
            Space<3>::Vector currentStep = dataSource->sampleDirection(loc, previousStep);
            
            // Perform loopcheck if requested: within the current 5x5x5 voxel block, has the streamline been going in the opposite direction?
            if (flags["loopcheck"])
            {
                for (int i=0; i<3; i++)
                    loopcheckLoc[i] = static_cast<int>(round(loc[i]/LOOPCHECK_RATIO));
                
                if (arma::dot((*loopcheck)[loopcheckLoc], previousStep) < 0.0)
                    break;
                
                (*loopcheck)[loopcheckLoc] = previousStep;
            }
            
            // Reverse the sampled direction if its inner product with the previous step is negative
            // If there is no previous step (or rightwards vector), the sign is random
            if (starting && !rightwardsVectorValid)
            {
                double uniformSample = R::unif_rand();
                sign = (uniformSample > 0.5) ? 1.0 : -1.0;
            }
            else
            {
                float innerProd = arma::dot(previousStep, currentStep);
                if (fabs(innerProd) < innerProductThreshold)
                    break;
                sign = (innerProd > 0.0) ? 1.0 : -1.0;
            }
            
            // Update streamline front and previous step
            loc += (currentStep / arma::conv_to<arma::fvec>::from(voxelDims)) * sign * stepLength;
            previousStep = currentStep * sign;
            
            // Store the first step to ensure that subsequent samples go the same way
            if (starting)
            {
                if (!rightwardsVectorValid)
                    rightwardsVector = previousStep;
                starting = false;
            }
        }
        
        // Store the number of steps taken in each direction, if required
        if (flags["must-leave"] && !leftMask)
        {
            if (dir == 0)
                rightPoints.clear();
            else
                leftPoints.clear();
        }
    }
    
    Streamline streamline(leftPoints, rightPoints, Streamline::VoxelPointType, true);
    return streamline;
}
