#include <RcppArmadillo.h>

#include "Streamline.h"

using namespace Rcpp;

typedef std::vector<int> int_vector;
typedef std::vector<std::string> str_vector;

RcppExport SEXP track_bedpost (SEXP seeds, SEXP n_seeds, SEXP mode, SEXP mask_image_name, SEXP parameter_image_names, SEXP n_compartments, SEXP n_samples, SEXP max_steps, SEXP step_length, SEXP volfrac_threshold, SEXP curvature_threshold, SEXP use_loopcheck, SEXP rightwards_vector, SEXP require_visitation_map, SEXP require_streamlines, SEXP terminate_outside_mask, SEXP must_leave_mask)
{
BEGIN_RCPP
    List parameterMapPaths(parameter_map_paths_);
    BedpostDataSource *dataSource = new BedpostDataSource(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    dataSource->setAvfThreshold(as<float>(volfrac_threshold_));
    
    NiftiImage<short> *mask = new NiftiImage<short>(as<std::string>(maskPath_));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(use_loopcheck_);
    flags["terminate-outside"] = as<bool>(terminate_outside_mask_);
    flags["must-leave"] = as<bool>(must_leave_mask_);
    
    NumericMatrix seeds(seeds_);
    
    RNGScope scope;
END_RCPP
}
