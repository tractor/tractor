#include <RcppArmadillo.h>

#include "Space.h"
#include "NiftiImage.h"
#include "Streamline.h"
#include "Tracker.h"
#include "Trackvis.h"
#include "VisitationMap.h"
#include "RCallback.h"
#include "Pipeline.h"

using namespace Rcpp;

typedef std::vector<int> int_vector;
typedef std::vector<std::string> str_vector;

RcppExport SEXP createBedpostTracker (SEXP _parameterMapPaths, SEXP _avfThreshold, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _maxSteps, SEXP _stepLength)
{
BEGIN_RCPP
    List parameterMapPaths(_parameterMapPaths);
    BedpostDataSource *bedpost = new BedpostDataSource(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    bedpost->setAvfThreshold(as<float>(_avfThreshold));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(_useLoopcheck);
    
    Tracker *tracker = new Tracker(bedpost);
    tracker->setFlags(flags);
    tracker->setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker->setStepLength(as<float>(_stepLength));
    tracker->setMaxSteps(as<int>(_maxSteps));
    
    XPtr<Tracker> trackerPtr(tracker);
    return trackerPtr;
END_RCPP
}

RcppExport SEXP setTrackerMask (SEXP _tracker, SEXP _maskPath)
{
BEGIN_RCPP
    XPtr<Tracker> trackerPtr(_tracker);
    Tracker *tracker = trackerPtr;
    
    NiftiImage *mask = new NiftiImage(as<std::string>(_maskPath));
    tracker->setMask(mask);
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP track (SEXP _tracker, SEXP _seeds, SEXP _count, SEXP _rightwardsVector, SEXP _terminateOutsideMask, SEXP _mustLeaveMask, SEXP _mapPath, SEXP _trkPath, SEXP _profilePath, SEXP _debugLevel)
{
BEGIN_RCPP
    XPtr<Tracker> trackerPtr(_tracker);
    Tracker *tracker = trackerPtr;
    
    tracker->setFlag("terminate-outside", as<bool>(_terminateOutsideMask));
    tracker->setFlag("must-leave", as<bool>(_mustLeaveMask));
    
    Space<3>::Vector rightwardsVector;
    if (isNull(_rightwardsVector))
        rightwardsVector = Space<3>::zeroVector();
    else
        rightwardsVector = as<arma::fvec>(_rightwardsVector);
    tracker->setRightwardsVector(rightwardsVector);
    
    RNGScope scope;
    
    TractographyDataSource dataSource(tracker, as<arma::fmat>(_seeds) - 1.0, as<size_t>(_count));
    Pipeline<Streamline> pipeline(&dataSource);
    
    if (!isNull(_mapPath))
    {
        VisitationMapDataSink visitationMap(mask->getDimensions());
        pipeline.addSink(&visitationMap);
    }
    if (!isNull(_trkPath))
    {
        TrackvisDataSink trkFile(as<std::string>(_trkPath), *mask);
        pipeline.addSink(&trkFile);
    }
    
    pipeline.run();
    
    if (!isNull(_mapPath))
        visitationMap.writeToNifti(*mask, as<std::string>(_mapPath));
    
    return R_NilValue;
END_RCPP
}
