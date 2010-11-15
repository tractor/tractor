#include "track.h"

#include <R.h>

void track_fdt (int *seed, char **mask_name, char **avf_names, char **theta_names, char **phi_names, int *n_compartments, int *n_samples, int *max_steps, double *step_length, double *avf_threshold, double *curvature_threshold, int *use_loopcheck, int *use_rightwards_vector, double *rightwards_vector, int *visitation_counts, int *left_lengths, int *right_lengths, double *left_particles, double *right_particles, int *require_visitation_map, int *require_particles)
{
    int i, j, dir, sample, step, starting, max_steps_per_dir;
    int dim[4], loopcheck_dims[4], particles_dims[3], rounded_loc[3], loopcheck_loc[4], particles_loc[3], dim3[3];
    int *visited;
    long k, dimprod, loopcheck_dimprod, vector_loc;
    double loopcheck_ratio, uniform_sample, inner_prod, theta, phi, sign;
    double voxdim[3], loc[3], old_step[3], prev_step[3], first_step[3], current_step[3];
    double *loopcheck;
    
    nifti_image *mask_image = nifti_image_read(*mask_name, 1);
    if (mask_image == NULL)
    {
        error("Cannot read mask image at %s", *mask_name);
        return;
    }
    if (mask_image->datatype != DT_INT16)
    {
        nifti_image_free(mask_image);
        error("Mask image does not use the expected data type (code %d)", mask_image->datatype);
        return;
    }
    
    nifti_image **avf_images = (nifti_image**) R_alloc(*n_compartments, sizeof(nifti_image*));
    nifti_image **theta_images = (nifti_image**) R_alloc(*n_compartments, sizeof(nifti_image*));
    nifti_image **phi_images = (nifti_image**) R_alloc(*n_compartments, sizeof(nifti_image*));
    for (i=0; i<(*n_compartments); i++)
    {
        avf_images[i] = nifti_image_read(avf_names[i], 1);
        theta_images[i] = nifti_image_read(theta_names[i], 1);
        phi_images[i] = nifti_image_read(phi_names[i], 1);
        
        if (avf_images[i] == NULL || theta_images[i] == NULL || phi_images[i] == NULL)
        {
            cleanup_fdt_images(mask_image, avf_images, theta_images, phi_images, *n_compartments);
            error("Cannot read samples files");
            return;
        }
        if (avf_images[i]->datatype != DT_FLOAT32 || theta_images[i]->datatype != DT_FLOAT32 || phi_images[i]->datatype != DT_FLOAT32)
        {
            cleanup_fdt_images(mask_image, avf_images, theta_images, phi_images, *n_compartments);
            error("One or more samples image does not use the expected data type");
            return;
        }
    }
    
    for (i=0; i<3; i++)
    {
        dim3[i] = mask_image->dim[i+1];
        voxdim[i] = (double) mask_image->pixdim[i+1];
    }
    dimprod = dim3[0] * dim3[1] * dim3[2];
    
    max_steps_per_dir = (*max_steps) / 2;
    
    visited = (int *) R_alloc(dimprod, sizeof(int));
    
    if (*use_loopcheck)
    {   
        loopcheck_ratio = 5.0;
        for (i=0; i<3; i++)
            loopcheck_dims[i] = ((int) round(dim3[i] / loopcheck_ratio)) + 1;
        loopcheck_dims[3] = 3;
        loopcheck_dimprod = loopcheck_dims[0] * loopcheck_dims[1] * loopcheck_dims[2] * loopcheck_dims[3];
        
        loopcheck = (double *) R_alloc(loopcheck_dimprod, sizeof(double));
    }
    
    if (*require_particles)
    {
        particles_dims[0] = max_steps_per_dir;
        particles_dims[1] = 3;
        particles_dims[2] = (*n_samples);
    }
    
    GetRNGstate();
    
    for (sample=0; sample<(*n_samples); sample++)
    {
        starting = 1;
        
        for (k=0; k<dimprod; k++)
            visited[k] = 0;
        
        // We go right first (dir=0), then left (dir=1)
        for (dir=0; dir<=1; dir++)
        {
            for (i=0; i<3; i++)
            {
                loc[i] = (double) (seed[i] - 1);
                if (starting && sample==0)
                {
                    if (*use_rightwards_vector)
                        prev_step[i] = rightwards_vector[i];
                    else
                        prev_step[i] = 0.0;
                }
                else
                    prev_step[i] = starting ? first_step[i] : -first_step[i];
            }
            
            if (*use_loopcheck)
            {
                for (k=0; k<loopcheck_dimprod; k++)
                    loopcheck[k] = 0.0;
            }
            
            for (step=0; step<max_steps_per_dir; step++)
            {
                for (i=0; i<3; i++)
                    rounded_loc[i] = (int) round(loc[i]);
                if (loc_in_bounds(rounded_loc, dim3, 3) == 0)
                {
                    step--;
                    break;
                }
                
                vector_loc = get_vector_loc(rounded_loc, dim3, 3);
                
                if (((int16_t *)(mask_image->data))[vector_loc] != 1)
                {
                    step--;
                    break;
                }
                else if (visited[vector_loc] == 0)
                {
                    visited[vector_loc] = 1;
                    visitation_counts[vector_loc]++;
                }
                
                if (*require_particles)
                {
                    particles_loc[0] = step;
                    particles_loc[2] = sample;
                    
                    for (i=0; i<3; i++)
                    {
                        particles_loc[1] = i;
                        if (dir == 1)
                            left_particles[get_vector_loc(particles_loc,particles_dims,3)] = loc[i] + 1;
                        else
                            right_particles[get_vector_loc(particles_loc,particles_dims,3)] = loc[i] + 1;
                    }
                }
                
                if (starting)
                    sample_direction(loc, NULL, avf_images, theta_images, phi_images, dim3, *n_compartments, *avf_threshold, &theta, &phi);
                else
                    sample_direction(loc, prev_step, avf_images, theta_images, phi_images, dim3, *n_compartments, *avf_threshold, &theta, &phi);
                spherical_to_cartesian(theta, phi, current_step);
                
                if (*use_loopcheck)
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
                
                if (starting && sample==0)
                {
                    uniform_sample = unif_rand();
                    sign = (uniform_sample > 0.5) ? 1.0 : -1.0;
                }
                else
                {
                    inner_prod = inner_product(prev_step, current_step, 3);
                    if (fabs(inner_prod) < (*curvature_threshold))
                        break;
                    sign = (inner_prod > 0) ? 1.0 : -1.0;
                }
                
                for (i=0; i<3; i++)
                {
                    loc[i] = loc[i] + (sign * current_step[i] * (*step_length) / voxdim[i]);
                    prev_step[i] = sign * current_step[i];
                }
                
                if (starting)
                {
                    for (i=0; i<3; i++)
                        first_step[i] = prev_step[i];
                    starting = 0;
                }
            }
            
            if (*require_particles)
            {
                if (dir == 1)
                    left_lengths[sample] = step;
                else
                    right_lengths[sample] = step;
            }
        }
    }
    
    cleanup_fdt_images(mask_image, avf_images, theta_images, phi_images, *n_compartments);
    
    PutRNGstate();
}

void cleanup_fdt_images (nifti_image *mask_image, nifti_image **avf_images, nifti_image **theta_images, nifti_image **phi_images, int n_compartments)
{
    int i;
    
    nifti_image_free(mask_image);
    
    for (i=0; i<n_compartments; i++)
    {
        nifti_image_free(avf_images[i]);
        nifti_image_free(theta_images[i]);
        nifti_image_free(phi_images[i]);
    }
}

void sample_direction (double *point, double *reference_direction, nifti_image **avf_images, nifti_image **theta_images, nifti_image **phi_images, int *dim3, int n_compartments, double avf_threshold, double *out_theta, double *out_phi)
{
    int i, point_ceil, point_floor;
    int closest_index = 0;
    int dim4[4], new_point[4];
    float avf, theta, phi;
    double distance, uniform_sample, inner_prod;
    double step_vector[3];
    double highest_inner_prod = -1;
    
    for (i=0; i<3; i++)
    {
        dim4[i] = dim3[i];
        
        point_ceil = (int) ceil(point[i]);
        point_floor = (int) floor(point[i]);
        
        distance = point[i] - point_floor;
        
        uniform_sample = unif_rand();
        if ((uniform_sample > distance && point_floor >= 0) || point_ceil >= dim4[i])
            new_point[i] = (int) point_floor;
        else
            new_point[i] = (int) point_ceil;
    }
    
    dim4[3] = avf_images[0]->dim[4];
    new_point[3] = (int) round(unif_rand() * (dim4[3]-1));
    
    // NB: Currently assuming always at least one anisotropic compartment
    for (int i=0; i<n_compartments; i++)
    {
        avf = index_float_array((float *) avf_images[i]->data, new_point, dim4, 4);
        if (i == 0 || avf >= avf_threshold)
        {
            theta = index_float_array((float *) theta_images[i]->data, new_point, dim4, 4);
            phi = index_float_array((float *) phi_images[i]->data, new_point, dim4, 4);
            spherical_to_cartesian(theta, phi, step_vector);
            
            // Use AVF to choose population on first step
            if (reference_direction == NULL)
                inner_prod = (double) avf;
            else
                inner_prod = fabs(inner_product(step_vector, reference_direction, 3));
            
            if (inner_prod > highest_inner_prod)
            {
                highest_inner_prod = inner_prod;
                closest_index = i;
            }
        }
    }
    
    *out_theta = index_float_array((float *) theta_images[closest_index]->data, new_point, dim4, 4);
    *out_phi = index_float_array((float *) phi_images[closest_index]->data, new_point, dim4, 4);
}

void spherical_to_cartesian (double theta, double phi, double *out_vector)
{
    out_vector[0] = sin(theta) * cos(phi);
    out_vector[1] = sin(theta) * sin(phi);
    out_vector[2] = cos(theta);
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

char loc_in_bounds (int *loc, int *dim, int ndims)
{
    int i;
    char in_bounds = 1;
    
    for (i=0; i<ndims; i++)
    {
        if (loc[i] < 0 || loc[i] > (dim[i]-1))
        {
            in_bounds = 0;
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
