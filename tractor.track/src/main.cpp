#include <RcppArmadillo.h>

#include "Space.h"
#include "NiftiImage.h"
#include "Streamline.h"
#include "Tracker.h"
#include "Pipeline.h"

using namespace Rcpp;

typedef std::vector<int> int_vector;
typedef std::vector<std::string> str_vector;

RcppExport SEXP track_bedpost (SEXP seeds_, SEXP mask_path_, SEXP parameter_map_paths_, SEXP n_samples_, SEXP max_steps_, SEXP step_length_, SEXP volfrac_threshold_, SEXP curvature_threshold_, SEXP use_loopcheck_, SEXP rightwards_vector_, SEXP terminate_outside_mask_, SEXP must_leave_mask_, SEXP debug_level_)
{
BEGIN_RCPP
    List parameterMapPaths(parameter_map_paths_);
    BedpostDataSource *bedpost = new BedpostDataSource(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    bedpost->setAvfThreshold(as<float>(volfrac_threshold_));
    
    NiftiImage *mask = new NiftiImage(as<std::string>(mask_path_));
    
    Tracker tracker(bedpost, mask);
    tracker.setDebugLevel(as<int>(debug_level_));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(use_loopcheck_);
    flags["terminate-outside"] = as<bool>(terminate_outside_mask_);
    flags["must-leave"] = as<bool>(must_leave_mask_);
    
    Space<3>::Vector rightwardsVector;
    if (Rf_isNull(rightwards_vector_))
        rightwardsVector = Space<3>::zeroVector();
    else
        rightwardsVector = as<arma::fvec>(rightwards_vector_);
    
    tracker.setFlags(flags);
    tracker.setRightwardsVector(rightwardsVector);
    tracker.setInnerProductThreshold(as<float>(curvature_threshold_));
    tracker.setStepLength(as<float>(step_length_));
    tracker.setMaxSteps(as<int>(max_steps_));
    
    RNGScope scope;
    
    TractographyDataSource dataSource(&tracker, as<arma::fmat>(seeds_) - 1.0, as<size_t>(n_samples_));
    StreamlineMatrixDataSink dataSink;
    Pipeline<Streamline> pipeline(&dataSource, 0);
    pipeline.addSink(&dataSink);
    pipeline.run();
    
    delete mask;
    delete bedpost;
    
    return List::create(Named("points")=dataSink.getPoints()+1.0, Named("startIndices")=dataSink.getStartIndices()+1, Named("seedIndices")=dataSink.getSeedIndices()+1);
END_RCPP
}
