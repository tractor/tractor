#include "vector.h"
#include "waypoint.h"

#include <R.h>
#include <Rdefines.h>

SEXP find_waypoint_hits (SEXP points, SEXP n_points, SEXP start_indices, SEXP lengths, SEXP n_starts, SEXP mask_points, SEXP n_masks, SEXP n_mask_points)
{
    int i, j, n_matches, current_match;
    int ns = *INTEGER(n_starts);
    int *int_ptr;
    int *temp_match = (int *) R_alloc(ns, sizeof(int));
    int *match = (int *) R_alloc(ns, sizeof(int));

    SEXP current_mask_points, matching_indices;
    
    int *zero_based_start_indices = (int *) R_alloc(ns, sizeof(int));
    
    for (j=0; j<ns; j++)
    {
        match[j] = 1;
        zero_based_start_indices[j] = INTEGER(start_indices)[j] - 1;
    }
    
    for (i=0; i<(*INTEGER(n_masks)); i++)
    {
        n_matches = 0;
        
        current_mask_points = VECTOR_ELT(mask_points, i);
        match_points(INTEGER(points), *INTEGER(n_points), zero_based_start_indices, INTEGER(lengths), *INTEGER(n_starts), INTEGER(current_mask_points), INTEGER(n_mask_points)[i], 3, temp_match);
        
        for (j=0; j<ns; j++)
        {
            match[j] = match[j] && temp_match[j];
            n_matches += match[j];
        }
    }
    
    PROTECT(matching_indices = NEW_INTEGER((R_len_t) n_matches));
    
    int_ptr = INTEGER(matching_indices);
    current_match = 0;
    for (j=0; j<ns; j++)
    {
        if (match[j])
        {
            int_ptr[current_match] = j + 1;
            current_match++;
        }
    }
    
    UNPROTECT(1);
    
    return matching_indices;
}

void match_points (const int *points, const int n_points, const int *start_indices, const int *lengths, const int n_starts, const int *target_points, const int n_target_points, const int n_dims, int *match)
{
    for (int i=0; i<n_starts; i++)
        match[i] = points_intersect(points, n_points, start_indices[i], lengths[i], target_points, n_target_points, n_dims);
}

int points_intersect (const int *points, const int n_points, const int start_index, const int length, const int *target_points, const int n_target_points, const int n_dims)
{
    int i, j, k, match;
    
    for (i=start_index; i<(start_index+length); i++)
    {
        for (j=0; j<n_target_points; j++)
        {
            match = 1;
            for (k=0; k<n_dims; k++)
            {
                if (points[i + k*n_points] != target_points[j + k*n_target_points])
                {
                    match = 0;
                    break;
                }
            }
            
            if (match)
                return 1;
        }
    }
    
    return 0;
}
