#include <RcppEigen.h>

#include "Tracker.h"

using namespace std;

Streamline Tracker::run ()
{
    if (model == NULL)
        throw std::runtime_error("No diffusion model has been specified");
    
    const Eigen::Array3i imageDims = model->getGrid3D().dimensions();
    std::vector<int> spaceDims(3);
    for (int i=0; i<3; i++)
        spaceDims[i] = imageDims(i,0);
    const Eigen::Array3f voxelDims = model->getGrid3D().spacings();
    
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
            loopcheckDims[i] = static_cast<int>(ceil(spaceDims[i] / LOOPCHECK_RATIO));
        loopcheck = new Array<Space<3>::Vector>(loopcheckDims, Space<3>::zeroVector());
    }
    
    bool starting = true;
    bool rightwardsVectorValid = !Space<3>::zeroVector(rightwardsVector);
    Space<3>::Point loc;
    std::vector<int> roundedLoc(3), loopcheckLoc(3);
    size_t vectorLoc;
    Space<3>::Vector previousStep = Space<3>::zeroVector();
    
    std::vector<Space<3>::Point> leftPoints, rightPoints;
    std::set<int> labels;
    
    Space<3>::Point currentSeed = seed;
    if (jitter)
    {
        for (int i=0; i<3; i++)
            currentSeed[i] += R::unif_rand() - 0.5;
    }
    
    int startTarget = 0;
    if (targetData != NULL)
    {
        for (int i=0; i<3; i++)
            roundedLoc[i] = static_cast<int>(round(currentSeed[i]));
        startTarget = targetData->at(roundedLoc);
        if (startTarget < 0)
            startTarget = 0;
    }
    
    // We go right first (dir=0), then left (dir=1)
    Streamline::TerminationReason terminationReasons[2] = { Streamline::UnknownReason, Streamline::UnknownReason };
    for (int dir=0; dir<2; dir++)
    {
        logger.debug2.indent() << "Tracking " << (dir==0 ? "\"right\"" : "\"left\"") << endl;
        
        if (flags["loopcheck"])
            loopcheck->fill(Space<3>::zeroVector());
        
        loc = currentSeed;
        if (rightwardsVectorValid)
            previousStep = rightwardsVector * (dir==0 ? 1.0 : -1.0);
        
        int step;
        int previouslyInsideMask = -1;
        
        // Run the tracking
        for (step=0; step<(maxSteps/2); step++)
        {
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
                terminationReasons[dir] = Streamline::BoundsReason;
                logger.debug2.indent() << "Terminating: stepped out of bounds" << endl;
                break;
            }
            
            // Index for current location
            visited->flattenIndex(roundedLoc, vectorLoc);
            
            // Stop if we've stepped outside the mask, possibly deferring termination if required
            if ((*maskData)[vectorLoc] == 0 && previouslyInsideMask == 1)
            {
                terminationReasons[dir] = Streamline::MaskReason;
                logger.debug2.indent() << "Terminating: stepped outside tracking mask" << endl;
                break;
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
            {
                leftPoints.push_back(loc);
                
                if (flags["one-way"])
                {
                    terminationReasons[dir] = Streamline::OneWayReason;
                    logger.debug2.indent() << "Terminating: one-way tracking" << endl;
                    break;
                }
            }
            
            // Add label if we're in a target area; terminate if required and we've left the starting region
            if (targetData != NULL && (*targetData)[vectorLoc] > 0)
            {
                labels.insert((*targetData)[vectorLoc]);
                
                if (flags["terminate-targets"] && (*targetData)[vectorLoc] != startTarget)
                {
                    terminationReasons[dir] = Streamline::TargetReason;
                    logger.debug2.indent() << "Terminating: target hit" << endl;
                    break;
                }
            }
            
            // Sample a direction for the current step
            Space<3>::Vector currentStep = model->sampleDirection(loc, previousStep);
            logger.debug3.indent() << "Sampled step direction is " << currentStep << endl;
            if (Space<3>::zeroVector(currentStep))
            {
                terminationReasons[dir] = Streamline::NoDataReason;
                logger.debug2.indent() << "Terminating: zero step vector" << endl;
                break;
            }
            
            // Perform loopcheck if requested: within the current 5x5x5 voxel block, has the streamline been going in the opposite direction?
            if (flags["loopcheck"])
            {
                for (int i=0; i<3; i++)
                    loopcheckLoc[i] = static_cast<int>(round((loc[i] + 0.5) / LOOPCHECK_RATIO - 0.5));
                
                float loopcheckInnerProduct = (*loopcheck)[loopcheckLoc].dot(previousStep);
                if (loopcheckInnerProduct < 0.0)
                {
                    terminationReasons[dir] = Streamline::LoopReason;
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
                float innerProduct = previousStep.dot(currentStep);
                if (fabs(innerProduct) < innerProductThreshold)
                {
                    terminationReasons[dir] = Streamline::CurvatureReason;
                    logger.debug2.indent() << "Terminating: curvature too high" << endl;
                    break;
                }
                sign = (innerProduct > 0.0) ? 1.0 : -1.0;
            }
            
            // Update streamline front and previous step
            loc += currentStep.array() / voxelDims * sign * stepLength;
            previousStep = currentStep * sign;
            logger.debug3.indent() << "New location is " << loc << endl;
            
            // Store the first step to ensure that subsequent samples go the same way
            if (starting)
            {
                if (!rightwardsVectorValid && !flags["one-way"])
                {
                    // The choice of sign above makes this always towards the right
                    rightwardsVector = previousStep;
                    rightwardsVectorValid = true;
                }
                starting = false;
            }
        }
        
        logger.debug2.indent() << "Completed " << step << " steps" << endl;
    }
    
    logger.debug1.indent() << "Tracking finished" << endl;
    
    Streamline streamline(leftPoints, rightPoints, Streamline::VoxelPointType, voxelDims, true);
    streamline.setTerminationReasons(terminationReasons[0], terminationReasons[1]);
    streamline.setLabels(labels);
    return streamline;
}
