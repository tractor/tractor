#ifndef _TRACK_H_
#define _TRACK_H_

#include "config.h"

#include <Rinternals.h>

#define TRACK_MODE_FDT 1

void clean_up_streamlines ();

SEXP get_list_element (SEXP list, const char *name);

SEXP track_with_seed (SEXP seed, SEXP mode, SEXP mask_image_name, SEXP parameter_image_names, SEXP n_compartments, SEXP n_samples, SEXP max_steps, SEXP step_length, SEXP volfrac_threshold, SEXP curvature_threshold, SEXP use_loopcheck, SEXP rightwards_vector, SEXP require_visitation_map, SEXP require_streamlines);

unsigned char * read_mask_image (const char *mask_image_name, int *image_dims, double *voxel_dims);

float * read_parameter_image (const char *parameter_image_name, size_t *len);

void track_fdt (const double *seed, const int *image_dims, const double *voxel_dims, const unsigned char *mask, const float **avf, const float **theta, const float **phi, const int n_compartments, const int n_samples, const int max_steps, const double step_length, const double avf_threshold, const double curvature_threshold, const int use_loopcheck, const double *rightwards_vector, const int require_visitation_map, const int require_streamlines, int *visitation_counts);

void sample_direction (const double *point, const double *reference_direction, const float **avf, const float **theta, const float **phi, const int *image_dims, const int n_compartments, const double avf_threshold, float *out_theta, float *out_phi);

void spherical_to_cartesian (const double theta, const double phi, double *vector);

unsigned char index_uchar_array (const unsigned char *array, const int *loc, const int *dim, const int ndims);

int index_int_array (const int *array, const int *loc, const int *dim, const int ndims);

float index_float_array (const float *array, const int *loc, const int *dim, const int ndims);

double index_double_array (const double *array, const int *loc, const int *dim, const int ndims);

int loc_in_bounds (const int *loc, const int *dim, const int ndims);

long get_vector_loc (const int *loc, const int *dim, const int ndims);

double inner_product (const double *a, const double *b, const int len);

#endif
