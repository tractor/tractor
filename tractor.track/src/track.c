#include "nifti1_io.h"

#include "vector.h"
#include "r_utils.h"
#include "track.h"

#include <R.h>
#include <Rdefines.h>

static double *points = NULL;
static int points_allocated = 0, current_point = 0;
static size_t point_block_size = 0;

static int *start_indices = NULL, *seed_indices = NULL;
static int indices_allocated = 0, current_index = 0;
static size_t index_block_size = 0;

static int *visited = NULL;
static double *loopcheck = NULL, *left_points = NULL, *right_points = NULL;

void clean_up_streamlines ()
{
    if (points != NULL)
        Free(points);
    if (start_indices != NULL)
        Free(start_indices);
    if (seed_indices != NULL)
        Free(seed_indices);
    if (visited != NULL)
        Free(visited);
    if (loopcheck != NULL)
        Free(loopcheck);
    if (left_points != NULL)
        Free(left_points);
    if (right_points != NULL)
        Free(right_points);
}

SEXP track_with_seeds (SEXP seeds, SEXP n_seeds, SEXP mode, SEXP mask_image_name, SEXP parameter_image_names, SEXP n_compartments, SEXP n_samples, SEXP max_steps, SEXP step_length, SEXP volfrac_threshold, SEXP curvature_threshold, SEXP use_loopcheck, SEXP rightwards_vector, SEXP require_visitation_map, SEXP require_streamlines, SEXP terminate_outside_mask, SEXP must_leave_mask)
{
    unsigned char *mask = NULL;
    int image_dims[4];
    double voxel_dims[3], zero_based_seed[3];
    char *image_name;
    size_t dim_prod = 1;
    int i;
    
    int nd = *INTEGER(n_seeds);
    int nc = *INTEGER(n_compartments);
    int ns = *INTEGER(n_samples);
    double *rv = NULL;
    
    SEXP return_value;
    
    if (!isNull(rightwards_vector))
        rv = REAL(rightwards_vector);
    
    mask = read_mask_image(CHAR(STRING_ELT(mask_image_name,0)), image_dims, voxel_dims);
    for (int i=0; i<3; i++)
        dim_prod *= abs(image_dims[i]);
    
    switch (*INTEGER(mode))
    {
        case TRACK_MODE_FDT:
        {
            float **avf = (float **) R_alloc(nc, sizeof(float*));
            float **theta = (float **) R_alloc(nc, sizeof(float*));
            float **phi = (float **) R_alloc(nc, sizeof(float*));
            
            int *visitation_counts = NULL;
            int j, elements_to_return = 1 + (*LOGICAL(require_streamlines) ? 3 : 0);
            size_t len;
            
            double *double_ptr;
            int *int_ptr;
            
            SEXP visitation_map, streamline_points, streamline_starts, streamline_seeds;
            
            for (i=0; i<nc; i++)
            {
                // This can be expensive, so allow the user to interrupt
                R_CheckUserInterrupt();
                
                image_name = (char *) CHAR(STRING_ELT(get_list_element(parameter_image_names,"avf"),i));
                avf[i] = read_parameter_image(image_name, &len);
                image_name = (char *) CHAR(STRING_ELT(get_list_element(parameter_image_names,"theta"),i));
                theta[i] = read_parameter_image(image_name, &len);
                image_name = (char *) CHAR(STRING_ELT(get_list_element(parameter_image_names,"phi"),i));
                phi[i] = read_parameter_image(image_name, &len);
            }
            
            image_dims[3] = (int) (len / dim_prod);
            
            if (*LOGICAL(require_visitation_map))
            {
                visitation_counts = (int *) R_alloc(dim_prod, sizeof(int));
                memset(visitation_counts, 0, dim_prod * sizeof(int));
            }
            
            if (point_block_size <= 0)
                point_block_size = (size_t) nd * ns * (*INTEGER(max_steps)) / 20;
            if (index_block_size <= 0)
                index_block_size = (size_t) nd * ns;
            
            for (j=0; j<nd; j++)
            {
                for (i=0; i<3; i++)
                    zero_based_seed[i] = REAL(seeds)[j+nd*i] - 1.0;
                
                track_fdt(zero_based_seed, image_dims, voxel_dims, mask, (const float **) avf, (const float **) theta, (const float **) phi, nc, ns, *INTEGER(max_steps), *REAL(step_length), *REAL(volfrac_threshold), *REAL(curvature_threshold), (int) *LOGICAL(use_loopcheck), rv, (int) *LOGICAL(require_visitation_map), (int) *LOGICAL(require_streamlines), (int) *LOGICAL(terminate_outside_mask), *LOGICAL(must_leave_mask), visitation_counts);
            }
            
            PROTECT(return_value = NEW_LIST((R_len_t) elements_to_return));
            
            if (*LOGICAL(require_visitation_map))
            {
                PROTECT(visitation_map = NEW_INTEGER((R_len_t) dim_prod));
                memcpy(INTEGER(visitation_map), visitation_counts, dim_prod*sizeof(int));
                SET_ELEMENT(return_value, 0, visitation_map);
                UNPROTECT(1);
            }
            else
                SET_ELEMENT(return_value, 0, R_NilValue);
            
            if (*LOGICAL(require_streamlines))
            {
                PROTECT(streamline_points = allocMatrix(REALSXP,current_point,3));
                double_ptr = REAL(streamline_points);
                for (i=0; i<3; i++)
                {
                    for (j=0; j<current_point; j++)
                        double_ptr[i*current_point+j] = points[i+j*3] + 1.0;
                }
                SET_ELEMENT(return_value, 1, streamline_points);
                
                PROTECT(streamline_starts = NEW_INTEGER((R_len_t) current_index));
                int_ptr = INTEGER(streamline_starts);
                for (j=0; j<(current_index); j++)
                    int_ptr[j] = start_indices[j] + 1;
                SET_ELEMENT(return_value, 2, streamline_starts);
                
                PROTECT(streamline_seeds = NEW_INTEGER((R_len_t) current_index));
                int_ptr = INTEGER(streamline_seeds);
                for (j=0; j<(current_index); j++)
                    int_ptr[j] = seed_indices[j] + 1;
                SET_ELEMENT(return_value, 3, streamline_seeds);
                
                UNPROTECT(3);
            }
            
            UNPROTECT(1);
            
            break;
        }
    }
    
    return return_value;
}

unsigned char * read_mask_image (const char *mask_image_name, int *image_dims, double *voxel_dims)
{
    nifti_image *image = nifti_image_read(mask_image_name, 1);
    if (image == NULL)
        error("Cannot read mask image at %s", mask_image_name);
    if (image->ndim != 3)
        error("Mask image should be three-dimensional");
    
    unsigned char *buffer = (unsigned char *) R_alloc(image->nvox, sizeof(unsigned char));
    
    switch (image->datatype)
    {
        case DT_UINT8:
        memcpy(buffer, image->data, nifti_get_volsize(image));
        break;
        
        case DT_INT16:
        for (size_t i=0; i<image->nvox; i++)
            buffer[i] = (((int16_t *) image->data)[i] == 0) ? 0 : 1;
        break;
        
        case DT_INT32:
        for (size_t i=0; i<image->nvox; i++)
            buffer[i] = (((int32_t *) image->data)[i] == 0) ? 0 : 1;
        break;
        
        default:
        error("Mask image does not have a supported data type (%s)", nifti_datatype_string(image->datatype));
    }
    
    for (int i=0; i<3; i++)
    {
        image_dims[i] = image->dim[i+1];
        voxel_dims[i] = image->pixdim[i+1];
    }
    
    nifti_image_free(image);
    
    return buffer;
}

float * read_parameter_image (const char *parameter_image_name, size_t *len)
{
    nifti_image *image = nifti_image_read(parameter_image_name, 1);
    if (image == NULL)
        error("Cannot read parameter image at %s", parameter_image_name);
    
    float *buffer = (float *) R_alloc(image->nvox, sizeof(float));
    
    switch (image->datatype)
    {
        case DT_FLOAT32:
        memcpy(buffer, image->data, nifti_get_volsize(image));
        break;
        
        case DT_FLOAT64:
        for (size_t i=0; i<image->nvox; i++)
            buffer[i] = (float) ((double *) image->data)[i];            
        break;
        
        default:
        error("Parameter image %s does not have a supported data type (%s)", parameter_image_name, nifti_datatype_string(image->datatype));
    }
    
    *len = image->nvox;
    
    nifti_image_free(image);
    
    return buffer;
}

void track_fdt (const double *seed, const int *image_dims, const double *voxel_dims, const unsigned char *mask, const float **avf, const float **theta, const float **phi, const int n_compartments, const int n_samples, const int max_steps, const double step_length, const double avf_threshold, const double curvature_threshold, const int use_loopcheck, const double *rightwards_vector, const int require_visitation_map, const int require_streamlines, const int terminate_outside_mask, const int must_leave_mask, int *visitation_counts)
{
    int i, j, starting, dir, sample, step, left_steps, right_steps, max_steps_per_dir, this_point, terminate_on_next_step, times_left_mask, left_mask, previously_inside_mask;
    int loopcheck_dims[4], points_dims[2], rounded_loc[3], loopcheck_loc[4], points_loc[2];
    size_t k, dim_prod, loopcheck_dim_prod, vector_loc;
    float theta_sample, phi_sample;
    double loopcheck_ratio, uniform_sample, inner_prod, sign;
    double loc[3], old_step[3], prev_step[3], first_step[3], current_step[3];
    
    dim_prod = image_dims[0] * image_dims[1] * image_dims[2];
    
    // Maximum steps for each direction ("left" and "right")
    max_steps_per_dir = max_steps / 2;
    
    // Allocate boolean visitation count vector
    if (visited == NULL)
        visited = (int *) Calloc(dim_prod, int);
    
    if (require_streamlines)
    {
        if (point_block_size <= 0)
            point_block_size = (size_t) n_samples * max_steps / 20;
        if (index_block_size <= 0)
            index_block_size = (size_t) n_samples;
        
        if (points == NULL)
        {
            points_allocated = point_block_size;
            points = (double *) Calloc((size_t) 3*points_allocated, double);
            current_point = 0;
        }
        
        if (start_indices == NULL)
        {
            indices_allocated = index_block_size;
            start_indices = (int *) Calloc(indices_allocated, int);
            seed_indices = (int *) Calloc(indices_allocated, int);
            current_index = 0;
        }
        else if (current_index + n_samples > indices_allocated)
        {
            while (current_index + n_samples > indices_allocated)
                indices_allocated += index_block_size;
            start_indices = (int *) Realloc(start_indices, indices_allocated, int);
            seed_indices = (int *) Realloc(start_indices, indices_allocated, int);
        }
        
        if (left_points == NULL)
        {
            left_points = (double *) Calloc((size_t) max_steps_per_dir*3, double);
            right_points = (double *) Calloc((size_t) max_steps_per_dir*3, double);
        }
        
        points_dims[0] = max_steps_per_dir;
        points_dims[1] = 3;
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

void sample_direction (const double *point, const double *reference_direction, const float **avf, const float **theta, const float **phi, const int *image_dims, const int n_compartments, const double avf_threshold, float *out_theta, float *out_phi)
{
    int i, point_ceil, point_floor;
    int closest_index = 0;
    int new_point[4];
    float avf_sample, theta_sample, phi_sample;
    double distance, uniform_sample, inner_prod;
    double step_vector[3];
    double highest_inner_prod = -1;
    
    // Probabilistic trilinear interpolation: select the sample location with probability in proportion to proximity
    for (i=0; i<3; i++)
    {
        point_ceil = (int) ceil(point[i]);
        point_floor = (int) floor(point[i]);
        
        distance = point[i] - point_floor;
        
        uniform_sample = unif_rand();
        if ((uniform_sample > distance && point_floor >= 0) || point_ceil >= image_dims[i])
            new_point[i] = point_floor;
        else
            new_point[i] = point_ceil;
    }
    
    // Randomly choose a sample number
    new_point[3] = (int) round(unif_rand() * (image_dims[3]-1));
    
    // NB: Currently assuming always at least one anisotropic compartment
    for (i=0; i<n_compartments; i++)
    {
        // Check AVF is above threshold
        avf_sample = index_float_array(avf[i], new_point, image_dims, 4);
        if (i == 0 || avf_sample >= avf_threshold)
        {
            theta_sample = index_float_array(theta[i], new_point, image_dims, 4);
            phi_sample = index_float_array(phi[i], new_point, image_dims, 4);
            spherical_to_cartesian((double) theta_sample, (double) phi_sample, step_vector);
            
            // Use AVF to choose population on first step
            if (reference_direction == NULL)
                inner_prod = (double) avf_sample;
            else
                inner_prod = fabs(inner_product(step_vector, reference_direction, 3));
            
            // If this direction is closer to the reference direction, choose it
            if (inner_prod > highest_inner_prod)
            {
                highest_inner_prod = inner_prod;
                closest_index = i;
            }
        }
    }
    
    // Set final theta and phi values
    *out_theta = index_float_array(theta[closest_index], new_point, image_dims, 4);
    *out_phi = index_float_array(phi[closest_index], new_point, image_dims, 4);
}
