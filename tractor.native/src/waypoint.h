#ifndef _WAYPOINT_H_
#define _WAYPOINT_H_

#include "config.h"

#include <Rinternals.h>

SEXP find_waypoint_hits (SEXP points, SEXP n_points, SEXP start_indices, SEXP lengths, SEXP n_starts, SEXP mask_points, SEXP n_masks, SEXP n_mask_points, SEXP exclusion);

void match_points (const int *points, const int n_points, const int *start_indices, const int *lengths, const int n_starts, const int *target_points, const int n_target_points, const int exclusion, const int n_dims, int *match);

int points_intersect (const int *points, const int n_points, const int start_index, const int length, const int *target_points, const int n_target_points, const int n_dims);

#endif
