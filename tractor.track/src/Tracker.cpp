#include <RcppArmadillo.h>

#include "Tracker.h"

using namespace std;

Streamline Tracker::run ()
{
    const std::vector<int> &spaceDims = mask->getDimensions();
    const std::vector<float> &voxelDims = mask->getVoxelDimensions();
    
    Rcpp::Rcout << std::fixed;
    Rcpp::Rcout.precision(3);
    logger.debug1.indent() << "Tracking from seed point " << seed << endl;
    
    if (visited == NULL)
    {
        logger.debug2.indent() << "Creating visitation map" << endl;
        visited = new Array<bool>(spaceDims, false);
    }
    else
    {
        logger.debug2.indent() << "Resetting visitation map" << endl;
        visited->fill(false);
    }
    
    if (flags["loopcheck"] && loopcheck == NULL)
    {
        logger.debug2.indent() << "Creating loopcheck vector field" << endl;
        std::vector<int> loopcheckDims(3);
        for (int i=0; i<3; i++)
            loopcheckDims[i] = static_cast<int>(round(spaceDims[i] / LOOPCHECK_RATIO)) + 1;
        loopcheck = new Array<Space<3>::Vector>(loopcheckDims, Space<3>::zeroVector());
    }
    
    bool starting = true;
    bool rightwardsVectorValid = !Space<3>::zeroVector(rightwardsVector);
    int timesLeftMask = 0;
    Space<3>::Point loc;
    std::vector<int> roundedLoc(3), loopcheckLoc(3);
    size_t vectorLoc;
    Space<3>::Vector previousStep = Space<3>::zeroVector();
    
    std::vector<Space<3>::Point> leftPoints, rightPoints;
    
    // We go right first (dir=0), then left (dir=1)
    for (int dir=0; dir<2; dir++)
    {
        logger.debug2.indent() << "Tracking " << (dir==0 ? "\"right\"" : "\"left\"") << endl;
        
        if (flags["loopcheck"])
            loopcheck->fill(Space<3>::zeroVector());
        
        loc = seed;
        if (rightwardsVectorValid)
            previousStep = rightwardsVector * (dir==0 ? 1.0 : -1.0);
        
        int step;
        bool leftMask = false;
        int previouslyInsideMask = -1;
        bool terminateOnNextStep = false;
        
        // Run the tracking
        for (step=0; step<(maxSteps/2); step++)
        {
            if (terminateOnNextStep)
            {
                logger.debug2.indent() << "Terminating: deferred termination" << endl;
                break;
            }
            
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
            {
                logger.debug2.indent() << "Terminating: stepped out of bounds" << endl;
                break;
            }
            
            // Index for current location
            visited->flattenIndex(roundedLoc, vectorLoc);
            
            if (starting && (*maskData)[vectorLoc] == 0)
                timesLeftMask++;
            
            // Stop if we've stepped outside the mask, possibly deferring termination if required
            if ((*maskData)[vectorLoc] == 0 && previouslyInsideMask == 1)
            {
                leftMask = true;
                timesLeftMask++;
                
                if (flags["terminate-outside"])
                    terminateOnNextStep = true;
                else
                {
                    logger.debug2.indent() << "Terminating: stepped outside tracking mask" << endl;
                    break;
                }
            }
            previouslyInsideMask = ((*maskData)[vectorLoc] == 0 ? 0 : 1);
            
            // Mark visit
            if (!(*visited)[vectorLoc])
                (*visited)[vectorLoc] = true;
            
            // Store current (unrounded) location if required
            // NB: This part of the code must always be reached at the seed point
            if (dir == 0)
                rightPoints.push_back(loc);
            else
                leftPoints.push_back(loc);
            
            // Sample a direction for the current step
            Space<3>::Vector currentStep = dataSource->sampleDirection(loc, previousStep);
            logger.debug3.indent() << "Sampled step direction is " << currentStep << endl;
            
            // Perform loopcheck if requested: within the current 5x5x5 voxel block, has the streamline been going in the opposite direction?
            if (flags["loopcheck"])
            {
                for (int i=0; i<3; i++)
                    loopcheckLoc[i] = static_cast<int>(round(loc[i]/LOOPCHECK_RATIO));
                
                float loopcheckInnerProduct = arma::dot((*loopcheck)[loopcheckLoc], previousStep);
                if (loopcheckInnerProduct < 0.0)
                {
                    logger.debug2.indent() << "Terminating: loop detected" << endl;
                    break;
                }
                else if (loopcheckInnerProduct == 0.0)
                    (*loopcheck)[loopcheckLoc] = previousStep;
            }
            
            // Reverse the sampled direction if its inner product with the previous step is negative
            // If there is no previous step (or rightwards vector), we're heading right so the sign is positive
            float sign;
            if (starting && !rightwardsVectorValid)
                sign = 1.0;
            else
            {
                float innerProduct = arma::dot(previousStep, currentStep);
                if (fabs(innerProduct) < innerProductThreshold)
                {
                    logger.debug2.indent() << "Terminating: curvature too high" << endl;
                    break;
                }
                sign = (innerProduct > 0.0) ? 1.0 : -1.0;
            }
            
            // Update streamline front and previous step
            loc += (currentStep / arma::conv_to<arma::fvec>::from(voxelDims)) * sign * stepLength;
            previousStep = currentStep * sign;
            logger.debug3.indent() << "New location is " << loc << endl;
            
            // Store the first step to ensure that subsequent samples go the same way
            if (starting)
            {
                if (!rightwardsVectorValid)
                {
                    // The choice of sign above makes this always towards the right
                    rightwardsVector = previousStep;
                    rightwardsVectorValid = true;
                }
                starting = false;
            }
        }
        
        // Store the number of steps taken in each direction, if required
        if (flags["must-leave"] && !leftMask)
        {
            logger.debug2.indent() << "Streamline piece did not leave mask; discarding it" << endl;
            if (dir == 0)
                rightPoints.clear();
            else
                leftPoints.clear();
        }
        else
            logger.debug2.indent() << "Completed " << step << " steps" << endl;
    }
    
    logger.debug1.indent() << "Tracking finished" << endl;
    
    Streamline streamline(leftPoints, rightPoints, Streamline::VoxelPointType, true, visited);
    return streamline;
}
