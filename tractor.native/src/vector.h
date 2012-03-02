#ifndef _VECTOR_H_
#define _VECTOR_H_

#include "config.h"

#include <stddef.h>

size_t get_vector_loc (const int *loc, const int *dim, const int ndims);

unsigned char index_uchar_array (const unsigned char *array, const int *loc, const int *dim, const int ndims);

int index_int_array (const int *array, const int *loc, const int *dim, const int ndims);

float index_float_array (const float *array, const int *loc, const int *dim, const int ndims);

double index_double_array (const double *array, const int *loc, const int *dim, const int ndims);

int loc_in_bounds (const int *loc, const int *dim, const int ndims);

double inner_product (const double *a, const double *b, const int len);

void spherical_to_cartesian (const double theta, const double phi, double *vector);

#endif
