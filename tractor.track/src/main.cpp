#include <RcppArmadillo.h>

#include "Space.h"
#include "NiftiImage.h"
#include "Streamline.h"
#include "Tracker.h"

using namespace Rcpp;

typedef std::vector<int> int_vector;
typedef std::vector<std::string> str_vector;

RcppExport SEXP track_bedpost (SEXP seeds_, SEXP mask_path_, SEXP parameter_map_paths_, SEXP n_samples_, SEXP max_steps_, SEXP step_length_, SEXP volfrac_threshold_, SEXP curvature_threshold_, SEXP use_loopcheck_, SEXP rightwards_vector_, SEXP terminate_outside_mask_, SEXP must_leave_mask_, SEXP debug_level_)
{
BEGIN_RCPP
    List parameterMapPaths(parameter_map_paths_);
    BedpostDataSource *dataSource = new BedpostDataSource(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    dataSource->setAvfThreshold(as<float>(volfrac_threshold_));
    
    NiftiImage<short> *mask = new NiftiImage<short>(as<std::string>(mask_path_));
    
    Tracker tracker(dataSource, mask);
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
    
    RNGScope scope;
    
    int streamlinesPerSeed = as<int>(n_samples_);
    int maxSteps = as<int>(max_steps_);
    arma::fmat seeds = as<arma::fmat>(seeds_);
    std::vector<Streamline> streamlines;
    for (size_t i=0; i<seeds.n_rows; i++)
    {
        // Vectors are columns to Armadillo, so we have to transpose
        tracker.setSeed(seeds.row(i).t() - 1.0);
        for (int j=0; j<streamlinesPerSeed; j++)
            streamlines.push_back(tracker.run(maxSteps));
    }
    
    int_vector startIndices, seedIndices;
    size_t nTotalPoints = 0;
    for (size_t i=0; i<(seeds.n_rows*streamlinesPerSeed); i++)
        nTotalPoints += streamlines[i].nPoints();
    
    size_t currentStart = 0;
    arma::fmat allPoints(nTotalPoints, 3);
    arma::fmat currentPoints;
    for (size_t i=0; i<(seeds.n_rows*streamlinesPerSeed); i++)
    {
        size_t seedIndex = streamlines[i].concatenatePoints(currentPoints);
        if (!currentPoints.empty())
        {
            // These indices are 1-based for R's benefit
            startIndices.push_back(currentStart + 1);
            seedIndices.push_back(currentStart + seedIndex + 1);
            allPoints(arma::span(currentStart, currentStart+currentPoints.n_rows-1), arma::span::all) = currentPoints + 1.0;
            currentStart += currentPoints.n_rows;
        }
    }
    
    delete mask;
    delete dataSource;
    
    return List::create(Named("points")=allPoints, Named("startIndices")=startIndices, Named("seedIndices")=seedIndices);
END_RCPP
}
