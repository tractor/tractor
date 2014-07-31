#include <RcppArmadillo.h>

#include "Space.h"
#include "Streamline.h"

using namespace Rcpp;

typedef std::vector<std::string> str_vector;

RcppExport SEXP track_bedpost (SEXP seeds_, SEXP mask_path_, SEXP parameter_map_paths_, SEXP n_samples_, SEXP max_steps_, SEXP step_length_, SEXP volfrac_threshold_, SEXP curvature_threshold_, SEXP use_loopcheck_, SEXP rightwards_vector_, SEXP terminate_outside_mask_, SEXP must_leave_mask_)
{
BEGIN_RCPP
    List parameterMapPaths(parameter_map_paths_);
    BedpostDataSource *dataSource = new BedpostDataSource(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    dataSource->setAvfThreshold(as<float>(volfrac_threshold_));
    
    NiftiImage<short> *mask = new NiftiImage<short>(as<std::string>(mask_path_));
    
    Tracker tracker(dataSource, mask);
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(use_loopcheck_);
    flags["terminate-outside"] = as<bool>(terminate_outside_mask_);
    flags["must-leave"] = as<bool>(must_leave_mask_);
    
    Space<3>::Vector rightwardsVector;
    if (isNULL(rightwards_vector_))
        rightwardsVector = Space<3>::zeroVector();
    else
        rightwardsVector = as<Space<3>::Vector>(rightwards_vector_);
    
    tracker.setFlags(flags);
    tracker.setRightwardsVector(rightwardsVector);
    tracker.setStepLength(as<float>(step_length_));
    
    RNGScope scope;
    
    int streamlinesPerSeed = as<int>(n_samples_);
    int maxSteps = as<int>(max_steps_);
    arma::fmat seeds = as<arma::fmat>(seeds_);
    std::vector<Streamline> streamlines;
    for (int i=0; i<seeds.n_rows; i++)
    {
        tracker.setSeed(seeds.row(i));
        for (int j=0; j<streamlinesPerSeed; j++)
            streamlines.push_back(tracker.run(maxSteps));
    }
END_RCPP
}
