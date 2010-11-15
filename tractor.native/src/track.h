#ifndef _TRACK_H_
#define _TRACK_H_

#include "nifti1_io.h"

void track_fdt (int *seed, char **mask_name, char **avf_names, char **theta_names, char **phi_names, int *n_compartments, int *n_samples, int *max_steps, double *step_length, double *avf_threshold, double *curvature_threshold, int *use_loopcheck, int *use_rightwards_vector, double *rightwards_vector, int *visitation_counts, int *left_lengths, int *right_lengths, double *left_particles, double *right_particles, int *require_visitation_map, int *require_particles);

void cleanup_fdt_images (nifti_image *mask_image, nifti_image **avf_images, nifti_image **theta_images, nifti_image **phi_images, int n_compartments);

void sample_direction (double *point, double *reference_direction, nifti_image **avf_images, nifti_image **theta_images, nifti_image **phi_images, int *dim3, int n_compartments, double avf_threshold, double *out_theta, double *out_phi);

void spherical_to_cartesian (double theta, double phi, double *out_vector);

int index_int_array (int *array, int *loc, int *dim, int ndims);

float index_float_array (float *array, int *loc, int *dim, int ndims);

double index_double_array (double *array, int *loc, int *dim, int ndims);

char loc_in_bounds (int *loc, int *dim, int ndims);

long get_vector_loc (int *loc, int *dim, int ndims);

double inner_product (double *a, double *b, int len);

#endif
