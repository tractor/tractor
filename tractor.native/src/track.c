#include "track.h"

#include <R.h>

static double *points = NULL;
static int points_allocated = 0, current_point = 0;
static size_t point_block_size = 0;

static int *start_indices = NULL, *seed_indices = NULL;
static int indices_allocated = 0, current_index = 0;
static size_t index_block_size = 0;

void clean_up_streamlines ()
{
    if (points != NULL)
        Free(points);
    if (start_indices != NULL)
        Free(start_indices);
    if (seed_indices != NULL)
        Free(seed_indices);
}

bool interrupts_pending ()
{
    return (R_interrupts_pending && !R_interrupts_suspended);
}

SEXP get_list_element (SEXP list, const char *name)
{
    SEXP element = R_NilValue;
    SEXP names = getAttrib(list, R_NamesSymbol);
     
    for (R_len_t i = 0; i < length(list); i++)
    {
        if (strcmp(CHAR(STRING_ELT(names,i)), name) == 0)
        {
            element = VECTOR_ELT(list, i);
            break;
        }
    }
    
    return element;
}

SEXP track_with_seed (SEXP seed, SEXP mode, SEXP mask_image_name, SEXP parameter_image_names, SEXP n_compartments, SEXP n_samples, SEXP max_steps, SEXP step_length, SEXP volfrac_threshold, SEXP curvature_threshold, SEXP use_loopcheck, SEXP rightwards_vector, SEXP require_visitation_map, SEXP require_streamlines)
{
    unsigned char *mask = NULL;
    int image_dims[4];
    double voxel_dims[3];
    char *image_name;
    long dim_prod = 1;
    
    int nc = *INTEGER(n_compartments);
    int ns = *INTEGER(n_samples);
    double *rv = NULL;
    
    SEXP return_value;
    
    if (!isNull(rightwards_vector))
        rv = REAL(rightwards_vector);
    
    read_mask_image(CHAR(STRING_ELT(mask_image_name,0)), mask, image_dims, voxel_dims);
    for (int i=0; i<3; i++)
        dim_prod *= image_dims[i];
    
    switch (*INTEGER(mode))
    {
        case TRACK_MODE_FDT:
            float **avf = R_alloc(nc, sizeof(double*));
            float **theta = R_alloc(nc, sizeof(double*));
            float **phi = R_alloc(nc, sizeof(double*));
            
            int *visitation_counts = NULL;
            int elements_to_return = 1 + (*LOGICAL(require_streamlines) ? 3 : 0);
            size_t len;
            
            SEXP visitation_map, streamline_points, streamline_starts, streamline_seeds;
            
            for (i=0; i<nc; i++)
            {
                avf[i] = theta[i] = phi[i] = NULL;
                image_name = CHAR(STRING_ELT(get_list_element(parameter_image_names,"avf"),0));
                read_parameter_image(image_name, avf[i], &len);
                image_name = CHAR(STRING_ELT(get_list_element(parameter_image_names,"theta"),0));
                read_parameter_image(image_name, theta[i], &len);
                image_name = CHAR(STRING_ELT(get_list_element(parameter_image_names,"phi"),0));
                read_parameter_image(image_name, phi[i], &len);
            }
            
            image_dims[3] = (int) (len / dim_prod);
            
            track_fdt(REAL(seed), image_dims, voxel_dims, mask, avf, theta, phi, nc, ns, *INTEGER(max_steps), *REAL(step_length), *REAL(volfrac_threshold), *REAL(curvature_threshold), (bool) *LOGICAL(use_loopcheck), rv, (bool) *LOGICAL(require_visitation_map), (bool) *LOGICAL(require_streamlines), visitation_counts);
            
            PROTECT(return_value = NEW_LIST((R_len_t) elements_to_return));
            
            if (*LOGICAL(require_visitation_map))
            {
                PROTECT(visitation_map = NEW_NUMERIC((R_len_t) dim_prod));
                REAL(visitation_map) = visitation_counts;
                SET_ELEMENT(return_value, 0, visitation_map);
                UNPROTECT(1);
            }
            else
                SET_ELEMENT(return_value, 0, R_NilValue);
            
            if (*LOGICAL(require_streamlines))
            {
                PROTECT(streamline_points = allocMatrix(REALSXP,3,n_points));
                REAL(streamline_points) = points;
                SET_ELEMENT(return_value, 1, streamline_points);
                PROTECT(streamline_starts = NEW_INTEGER((R_len_t) ns));
                INTEGER(streamline_starts) = start_indices;
                SET_ELEMENT(return_value, 2, streamline_starts);
                PROTECT(streamline_seeds = NEW_INTEGER((R_len_t) ns));
                INTEGER(streamline_seeds) = seed_indices;
                SET_ELEMENT(return_value, 3, streamline_seeds);
                UNPROTECT(3);
            }
            
            UNPROTECT(1);
            
            break;
    }
    
    return return_value;
}

void read_mask_image (const char *mask_image_name, unsigned char *buffer, int *image_dims, double *voxel_dims)
{
    nifti_image *image = nifti_image_read(mask_image_name, 1);
    if (image == NULL)
        error("Cannot read mask image at %s", mask_image_name);
    if (image->ndim != 3)
        error("Mask image should be three-dimensional");
    
    if (buffer == NULL)
        buffer = (unsigned char *) R_alloc(image->nvox, sizeof(unsigned char));
    if (image_dims == NULL)
        image_dims = (int *) R_alloc(3, sizeof(int));
    if (voxel_dims == NULL)
        voxel_dims = (double *) R_alloc(3, sizeof(double));
    
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
    
    for (i=0; i<3; i++)
    {
        image_dims[i] = image->dim[i+1];
        voxel_dims[i] = image->pixdim[i+1];
    }
}

void read_parameter_image (const char *parameter_image_name, float *buffer, size_t *len)
{
    nifti_image *image = nifti_image_read(parameter_image_name, 1);
    if (image == NULL)
        error("Cannot read parameter image at %s", parameter_image_name);
    
    if (buffer == NULL)
        buffer = (float *) R_alloc(image->nvox, sizeof(float));
    if (len == NULL)
        len = (size_t *) R_alloc(1, sizeof(size_t));
    
    *len = image->nvox;
    
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
}

void track_fdt (const double *seed, const int *image_dims, const double *voxel_dims, const unsigned char *mask, const float **avf, const float **theta, const float **phi, const int n_compartments, const int n_samples, const int max_steps, const double step_length, const double avf_threshold, const double curvature_threshold, const bool use_loopcheck, const double *rightwards_vector, const bool require_visitation_map, const bool require_streamlines, int *visitation_counts, double *points, int *n_points, int *start_indices, int *seed_indices)
{
    bool starting;
    int i, j, dir, sample, step, left_steps, right_steps, max_steps_per_dir, this_point;
    int dim[4], loopcheck_dims[4], points_dims[2], rounded_loc[3], loopcheck_loc[4], points_loc[2], dim3[3];
    bool *visited;
    long k, dim_prod, loopcheck_dim_prod, vector_loc;
    float theta_sample, phi_sample;
    double loopcheck_ratio, uniform_sample, inner_prod, sign;
    double voxdim[3], loc[3], old_step[3], prev_step[3], first_step[3], current_step[3];
    double *loopcheck, *left_points, *right_points;
    
    dim_prod = image_dims[0] * image_dims[1] * image_dims[2];
    
    // Maximum steps for each direction ("left" and "right")
    max_steps_per_dir = max_steps / 2;
    
    // Allocate boolean and integer visitation count vectors
    if (require_visitation_map)
    {
        visited = (bool *) R_alloc(dim_prod, sizeof(bool));
        if (visitation_counts == NULL)
            visitation_counts = (int *) R_alloc(dim_prod, sizeof(int));
    }
    
    if (require_streamlines)
    {
        if (point_block_size <= 0)
            point_block_size = n_samples * max_steps / 20;
        if (index_block_size <= 0)
            index_block_size = n_samples;
        
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
        
        left_points = (double *) R_alloc((size_t) max_steps_per_dir*3, sizeof(double));
        right_points = (double *) R_alloc((size_t) max_steps_per_dir*3, sizeof(double));
        
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
        
        loopcheck = (double *) R_alloc(loopcheck_dim_prod, sizeof(double));
    }
    
    // Initialise R's random number generator
    GetRNGstate();
    
    for (sample=0; sample<n_samples; sample++)
    {
        // Check if the user wants us to stop
        R_CheckUserInterrupt();
        
        // Indicates first step for this sample
        starting = true;
        
        // Zero out boolean visitation tracker
        for (k=0; k<dim_prod; k++)
            visited[k] = false;
        
        // We go right first (dir=0), then left (dir=1)
        for (dir=0; dir<2; dir++)
        {
            // Initialise streamline front
            for (i=0; i<3; i++)
            {
                // The seed is given in R coordinates - subtract 1 for C coordinates
                loc[i] = (seed[i] - 1.0);
                
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
            
            // Run the tracking
            for (step=0; step<max_steps_per_dir; step++)
            {
                // Check that the current step location is in bounds
                for (i=0; i<3; i++)
                    rounded_loc[i] = (int) round(loc[i]);
                if (loc_in_bounds(rounded_loc, dim3, 3) == 0)
                    break;
                
                // Index for current location
                vector_loc = get_vector_loc(rounded_loc, image_dims, 3);
                
                // Stop if outside the mask, otherwise mark visit
                if (mask[vector_loc] != 1)
                    break;
                else if (require_visitation_map && !visited[vector_loc])
                {
                    visited[vector_loc] = true;
                    visitation_counts[vector_loc]++;
                }
                
                // Store current (unrounded) location if required
                if (require_streamlines)
                {
                    points_loc[0] = step;
                    
                    for (i=0; i<3; i++)
                    {
                        particles_loc[1] = i;
                        if (dir == 1)
                            left_points[get_vector_loc(points_loc,points_dims,2)] = loc[i] + 1;
                        else
                            right_points[get_vector_loc(points_loc,points_dims,2)] = loc[i] + 1;
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
                if (starting && sample==0 && use_rightwards_vector==NULL)
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
                    starting = false;
                }
            }
            
            // Store the number of steps taken in each direction, if required
            if (require_streamlines)
            {
                if (dir == 1)
                    left_steps = step;
                else
                    right_steps = step;
            }
        }
        
        if (require_streamlines)
        {
            if (current_point + (left_steps - 1) + right_steps > points_allocated)
            {
                while (current_point + (left_steps - 1) + right_steps > points_allocated)
                    points_allocated += point_block_size;
                points = (double *) Realloc(points, (size_t) 3*points_allocated, double);
            }
            
            // NB: Ending criterion of i>0 is intentional: we don't want to duplicate the seed
            for (i=left_steps-1; i>0; i--)
            {
                for (j=0; j<3; j++)
                {
                    this_point = current_point + (left_steps - 1) - i;
                    points[this_point + j*points_allocated] = left_points[i + j*max_steps_per_dir];
                }
            }
            
            for (i=0; i<right_steps; i++)
            {
                for (j=0; j<3; j++)
                {
                    this_point = current_point + (left_steps - 1) + i;
                    points[this_point + j*points_allocated] = right_points[i + j*max_steps_per_dir];
                }
            }
            
            start_indices[current_index+sample] = current_point;
            seed_indices[current_index+sample] = current_point + (left_steps - 1);
            current_point += (left_steps - 1) + right_steps;
        }
    }
    
    // Tell R we've finished with the random number generator
    PutRNGstate();
}

void sample_direction (double *point, double *reference_direction, float **avf, float **theta, float **phi, int *image_dims, int n_compartments, double avf_threshold, float *out_theta, float *out_phi)
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
            new_point[i] = (int) point_floor;
        else
            new_point[i] = (int) point_ceil;
    }
    
    // Randomly choose a sample number
    new_point[3] = (int) round(unif_rand() * (image_dims[3]-1));
    
    // NB: Currently assuming always at least one anisotropic compartment
    for (int i=0; i<n_compartments; i++)
    {
        // Check AVF is above threshold
        avf_sample = index_float_array(avf[i], new_point, image_dims, 4);
        if (i == 0 || avf_sample >= avf_threshold)
        {
            theta_sample = index_float_array(theta[i], new_point, image_dims, 4);
            phi_sample = index_float_array(phi[i], new_point, image_dims, 4);
            spherical_to_cartesian(theta, phi, step_vector);
            
            // Use AVF to choose population on first step
            if (reference_direction == NULL)
                inner_prod = (double) avf;
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

void spherical_to_cartesian (double theta, double phi, double *out_vector)
{
    out_vector[0] = sin(theta) * cos(phi);
    out_vector[1] = sin(theta) * sin(phi);
    out_vector[2] = cos(theta);
}

unsigned char index_uchar_array (unsigned char *array, int *loc, int *dim, int ndims)
{
    long vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

int index_int_array (int *array, int *loc, int *dim, int ndims)
{
    long vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

float index_float_array (float *array, int *loc, int *dim, int ndims)
{
    long vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

double index_double_array (double *array, int *loc, int *dim, int ndims)
{
    long vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

bool loc_in_bounds (int *loc, int *dim, int ndims)
{
    int i;
    bool in_bounds = true;
    
    for (i=0; i<ndims; i++)
    {
        if (loc[i] < 0 || loc[i] > (dim[i]-1))
        {
            in_bounds = false;
            break;
        }
    }
    
    return in_bounds;
}

long get_vector_loc (int *loc, int *dim, int ndims)
{
    long vector_loc;
    switch (ndims)
    {
        case 2:
            vector_loc = loc[0] + (loc[1] * dim[0]);
            break;
        case 3:
            vector_loc = loc[0] + (loc[1] * dim[0]) + (loc[2] * dim[0] * dim[1]);
            break;
        case 4:
            vector_loc = loc[0] + (loc[1] * dim[0]) + (loc[2] * dim[0] * dim[1]) + (loc[3] * dim[0] * dim[1] * dim[2]);
            break;
    }
    
    return (vector_loc);
}

double inner_product (double *a, double *b, int len)
{
    int i;
    double result = 0.0;
    
    for (i=0; i<len; i++)
        result = result + (a[i] * b[i]);
    
    return (result);
}
