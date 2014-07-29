#include <RcppArmadillo.h>

#include "Tracker.h"

Streamline Tracker::run (const int maxSteps)
{
    // int i, j, starting, dir, sample, step, left_steps, right_steps, max_steps_per_dir, this_point, terminate_on_next_step, times_left_mask, left_mask, previously_inside_mask;
    // int loopcheck_dims[4], points_dims[2], rounded_loc[3], loopcheck_loc[4], points_loc[2];
    // size_t k, dim_prod, loopcheck_dim_prod, vector_loc;
    // float theta_sample, phi_sample;
    // double loopcheck_ratio, uniform_sample, inner_prod, sign;
    // double loc[3], old_step[3], prev_step[3], first_step[3], current_step[3];
    
    const std::vector<int> &spaceDims = mask->getDimensions();
    
    if (visited == NULL)
        visited = new Array<bool>(spaceDims, false);
    else
        visited->fill(false);
    
    if (flags["loopcheck"])
    {
        if (loopcheck == NULL)
        {
            std::vector<int> loopcheckDims(4);
            for (int i=0; i<3; i++)
                loopcheckDims[i] = static_cast<int>(R::round(spaceDims[i] / loopcheckRatio)) + 1
            loopcheckDims[3] = 3;
            
            loopcheck = new Array<float>(loopcheckDims, 0.0);
        }
        else
            loopcheck->fill(0.0);
    }
    
    bool starting = true;
    int timesLeftMask = 0;
    Space<3>::Point loc;
    std::vector<int> roundedLoc(3), loopcheckLoc(3);
    size_t vectorLoc;
    Space<3>::Vector firstStep;
    Space<3>::Vector *previousStep = NULL;
    
    std::vector<Space<3>::Point> leftPoints, rightPoints;
    
    // We go right first (dir=0), then left (dir=1)
    for (int dir=0; dir<2; dir++)
    {
        loc = seed;
        if (rightwardsVector != NULL)
            *previousStep = (*rightwardsVector) * (dir==0 ? 1 : -1);
        
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
                roundedLoc[i] = static_cast<int>(R::round(loc[i]));
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
            
            if (starting && mask[vectorLoc] == 0)
                timesLeftMask++;
            
            // Stop if we've stepped outside the mask, possibly deferring termination if required
            if (mask[vectorLoc] == 0 && previouslyInsideMask == 1)
            {
                leftMask = 1;
                timesLeftMask++;
                
                if (flags["terminate-outside"])
                    terminateOnNextStep = 1;
                else
                    break;
            }
            previouslyInsideMask = (mask[vectorLoc] == 0 ? 0 : 1);
            
            // Mark visit
            if (!visited[vectorLoc])
            {
                visited[vectorLoc] = true;
                if (flags["visitation-map"])
                    visitationCounts[vectorLoc]++;
            }
            
            // Store current (unrounded) location if required
            // NB: This part of the code must always be reached at the seed point
            if (dir == 0)
                rightPoints.push_back(loc);
            else
                leftPoints.push_back(loc);
            
            // Sample a direction for the current step
            currentStep = sampleDirection(loc, previousStep);
            
            // Perform loopcheck if requested: within the current 5x5x5 voxel block, has the streamline been going in the opposite direction?
            if (flags["loopcheck"])
            {
                for (i=0; i<3; i++)
                    loopcheckLoc[i] = static_cast<int>(R::round(loc[i]/loopcheckRatio));
                
                for (i=0; i<3; i++)
                {
                    loopcheck_loc[3] = i;
                    old_step[i] = index_double_array(loopcheck, loopcheck_loc, loopcheck_dims, 4);
                }
                
                if (inner_product(old_step, prev_step, 3) < 0)
                    break;
                
                for (i=0; i<3; i++)
                {
                    loopcheck_loc[3] = i;
                    loopcheck[get_vector_loc(loopcheck_loc, loopcheck_dims, 4)] = prev_step[i];
                }
            }
            
            // Reverse the sampled direction if its inner product with the previous step is negative
            // If there is no previous step (or rightwards vector), the sign is random
            if (starting && sample==0 && rightwards_vector==NULL)
            {
                uniform_sample = unif_rand();
                sign = (uniform_sample > 0.5) ? 1.0 : -1.0;
            }
            else
            {
                inner_prod = inner_product(prev_step, current_step, 3);
                if (fabs(inner_prod) < curvature_threshold)
                    break;
                sign = (inner_prod > 0) ? 1.0 : -1.0;
            }
            
            // Update streamline front and previous step
            for (i=0; i<3; i++)
            {
                loc[i] = loc[i] + (sign * current_step[i] * step_length / voxel_dims[i]);
                prev_step[i] = sign * current_step[i];
            }
            
            // Store the first step to ensure that subsequent samples go the same way
            if (starting)
            {
                for (i=0; i<3; i++)
                    first_step[i] = prev_step[i];
                starting = 0;
            }
        }
        
        // Store the number of steps taken in each direction, if required
        if (require_streamlines)
        {
            if (dir == 1)
                left_steps = (must_leave_mask && !left_mask) ? 0 : step;
            else
                right_steps = (must_leave_mask && !left_mask) ? 0 : step;
        }
    }
    
    // If using the loop check, allocate an image vector for it (1/5th resolution)
    if (use_loopcheck)
    {   
        loopcheck_ratio = 5.0;
        for (i=0; i<3; i++)
            loopcheck_dims[i] = ((int) round(image_dims[i] / loopcheck_ratio)) + 1;
        loopcheck_dims[3] = 3;
        loopcheck_dim_prod = loopcheck_dims[0] * loopcheck_dims[1] * loopcheck_dims[2] * loopcheck_dims[3];
        
        if (loopcheck == NULL)
            loopcheck = (double *) Calloc(loopcheck_dim_prod, double);
    }
    
    // Initialise R's random number generator
    GetRNGstate();
    
    for (sample=0; sample<n_samples; sample++)
    {
        // Check if the user wants us to stop
        R_CheckUserInterrupt();
        
        // Indicates first step for this sample
        starting = 1;
        
        // Zero out boolean visitation tracker
        for (k=0; k<dim_prod; k++)
            visited[k] = 0;
        
        times_left_mask = 0;
        
        // We go right first (dir=0), then left (dir=1)
        for (dir=0; dir<2; dir++)
        {
            // Initialise streamline front
            for (i=0; i<3; i++)
            {
                loc[i] = seed[i];
                
                if (starting && sample==0)
                {
                    if (rightwards_vector != NULL)
                        prev_step[i] = rightwards_vector[i];
                    else
                        prev_step[i] = 0.0;
                }
                else
                    prev_step[i] = starting ? first_step[i] : -first_step[i];
            }
            
            // Zero out loopcheck array if needed
            if (use_loopcheck)
            {
                for (k=0; k<loopcheck_dim_prod; k++)
                    loopcheck[k] = 0.0;
            }
            
            left_mask = 0;
            previously_inside_mask = -1;
            terminate_on_next_step = 0;
            
            // Run the tracking
            for (step=0; step<max_steps_per_dir; step++)
            {
                if (terminate_on_next_step)
                    break;
                
                // Check that the current step location is in bounds
                for (i=0; i<3; i++)
                    rounded_loc[i] = (int) round(loc[i]);
                if (loc_in_bounds(rounded_loc, image_dims, 3) == 0)
                    break;
                
                // Index for current location
                vector_loc = get_vector_loc(rounded_loc, image_dims, 3);
                
                if (starting && mask[vector_loc] == 0)
                    times_left_mask++;
                
                // Stop if we've stepped outside the mask, possibly deferring termination if required
                if (mask[vector_loc] == 0 && previously_inside_mask == 1)
                {
                    left_mask = 1;
                    times_left_mask++;
                    
                    if (terminate_outside_mask)
                        terminate_on_next_step = 1;
                    else
                        break;
                }
                previously_inside_mask = (mask[vector_loc] == 0 ? 0 : 1);
                
                // Mark visit
                if (!visited[vector_loc])
                {
                    visited[vector_loc] = 1;
                    if (require_visitation_map)
                        visitation_counts[vector_loc]++;
                }
                
                // Store current (unrounded) location if required
                // NB: This part of the code must always be reached at the seed point
                if (require_streamlines)
                {
                    points_loc[0] = step;
                    
                    for (i=0; i<3; i++)
                    {
                        points_loc[1] = i;
                        if (dir == 1)
                            left_points[get_vector_loc(points_loc,points_dims,2)] = loc[i];
                        else
                            right_points[get_vector_loc(points_loc,points_dims,2)] = loc[i];
                    }
                }
                
                // Sample a direction (represented by theta and phi) and convert it to a Cartesian vector
                if (starting)
                    sample_direction(loc, NULL, avf, theta, phi, image_dims, n_compartments, avf_threshold, &theta_sample, &phi_sample);
                else
                    sample_direction(loc, prev_step, avf, theta, phi, image_dims, n_compartments, avf_threshold, &theta_sample, &phi_sample);
                spherical_to_cartesian((double) theta_sample, (double) phi_sample, current_step);
                
                // Perform loopcheck if requested: within the current 5x5x5 voxel block, has the streamline been going in the opposite direction?
                if (use_loopcheck)
                {
                    for (i=0; i<3; i++)
                        loopcheck_loc[i] = (int) round(loc[i]/loopcheck_ratio);
                    
                    for (i=0; i<3; i++)
                    {
                        loopcheck_loc[3] = i;
                        old_step[i] = index_double_array(loopcheck, loopcheck_loc, loopcheck_dims, 4);
                    }
                    
                    if (inner_product(old_step, prev_step, 3) < 0)
                        break;
                    
                    for (i=0; i<3; i++)
                    {
                        loopcheck_loc[3] = i;
                        loopcheck[get_vector_loc(loopcheck_loc, loopcheck_dims, 4)] = prev_step[i];
                    }
                }
                
                // Reverse the sampled direction if its inner product with the previous step is negative
                // If there is no previous step (or rightwards vector), the sign is random
                if (starting && sample==0 && rightwards_vector==NULL)
                {
                    uniform_sample = unif_rand();
                    sign = (uniform_sample > 0.5) ? 1.0 : -1.0;
                }
                else
                {
                    inner_prod = inner_product(prev_step, current_step, 3);
                    if (fabs(inner_prod) < curvature_threshold)
                        break;
                    sign = (inner_prod > 0) ? 1.0 : -1.0;
                }
                
                // Update streamline front and previous step
                for (i=0; i<3; i++)
                {
                    loc[i] = loc[i] + (sign * current_step[i] * step_length / voxel_dims[i]);
                    prev_step[i] = sign * current_step[i];
                }
                
                // Store the first step to ensure that subsequent samples go the same way
                if (starting)
                {
                    for (i=0; i<3; i++)
                        first_step[i] = prev_step[i];
                    starting = 0;
                }
            }
            
            // Store the number of steps taken in each direction, if required
            if (require_streamlines)
            {
                if (dir == 1)
                    left_steps = (must_leave_mask && !left_mask) ? 0 : step;
                else
                    right_steps = (must_leave_mask && !left_mask) ? 0 : step;
            }
        }
        
        if (require_streamlines && (left_steps > 0 || right_steps > 0) && (!must_leave_mask || times_left_mask > 1))
        {
            // The seed will be trimmed from the left points, and must always be present in the right points
            if (left_steps > 0)
                left_steps--;
            if (right_steps == 0)
                right_steps++;
            
            if (current_point + left_steps + right_steps > points_allocated)
            {
                while (current_point + left_steps + right_steps > points_allocated)
                    points_allocated += point_block_size;
                points = (double *) Realloc(points, (size_t) 3*points_allocated, double);
            }
            
            // NB: Ending criterion of i>0 is intentional: we don't want to duplicate the seed
            for (i=left_steps; i>0; i--)
            {
                this_point = current_point + left_steps - i;
                for (j=0; j<3; j++)
                    points[3*this_point + j] = left_points[i + j*max_steps_per_dir];
            }
            
            for (i=0; i<right_steps; i++)
            {
                this_point = current_point + left_steps + i;
                for (j=0; j<3; j++)
                    points[3*this_point + j] = right_points[i + j*max_steps_per_dir];
            }
            
            start_indices[current_index] = current_point;
            seed_indices[current_index] = current_point + left_steps;
            current_point += left_steps + right_steps;
            current_index++;
        }
    }
    
    // Tell R we've finished with the random number generator
    PutRNGstate();
}
