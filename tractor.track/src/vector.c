#include "vector.h"

#include <Rmath.h>

size_t get_vector_loc (const int *loc, const int *dim, const int ndims)
{
    size_t vector_loc;
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

unsigned char index_uchar_array (const unsigned char *array, const int *loc, const int *dim, const int ndims)
{
    size_t vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

int index_int_array (const int *array, const int *loc, const int *dim, const int ndims)
{
    size_t vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

float index_float_array (const float *array, const int *loc, const int *dim, const int ndims)
{
    size_t vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

double index_double_array (const double *array, const int *loc, const int *dim, const int ndims)
{
    size_t vector_loc = get_vector_loc(loc, dim, ndims);
    return (array[vector_loc]);
}

int loc_in_bounds (const int *loc, const int *dim, const int ndims)
{
    int in_bounds = 1;
    
    for (int i=0; i<ndims; i++)
    {
        if (loc[i] < 0 || loc[i] > (dim[i]-1))
        {
            in_bounds = 0;
            break;
        }
    }
    
    return in_bounds;
}

double inner_product (const double *a, const double *b, const int len)
{
    double result = 0.0;
    
    for (int i=0; i<len; i++)
        result += a[i] * b[i];
    
    return (result);
}

void spherical_to_cartesian (const double theta, const double phi, double *vector)
{
    vector[0] = sin(theta) * cos(phi);
    vector[1] = sin(theta) * sin(phi);
    vector[2] = cos(theta);
}
