#include <RcppArmadillo.h>

#include "Space.h"
#include "NiftiImage.h"
#include "Streamline.h"
#include "Tracker.h"
#include "Filter.h"
#include "Trackvis.h"
#include "VisitationMap.h"
#include "RCallback.h"
#include "Pipeline.h"

using namespace Rcpp;

typedef std::vector<int> int_vector;
typedef std::vector<std::string> str_vector;

RcppExport SEXP createBedpostModel (SEXP _parameterMapPaths, SEXP _avfThreshold)
{
BEGIN_RCPP
    List parameterMapPaths(_parameterMapPaths);
    BedpostModel *model = new BedpostModel(as<str_vector>(parameterMapPaths["avf"]), as<str_vector>(parameterMapPaths["theta"]), as<str_vector>(parameterMapPaths["phi"]));
    model->setAvfThreshold(as<float>(_avfThreshold));
    
    XPtr<DiffusionModel> modelPtr(model);
    return modelPtr;
END_RCPP
}

RcppExport SEXP track (SEXP _model, SEXP _seeds, SEXP _count, SEXP _maskPath, SEXP _targetPath, SEXP _rightwardsVector, SEXP _maxSteps, SEXP _stepLength, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _terminateAtTargets, SEXP _minTargetHits, SEXP _minLength, SEXP _terminateOutsideMask, SEXP _mustLeaveMask, SEXP _jitter, SEXP _mapPath, SEXP _trkPath, SEXP _profilePath, SEXP _debugLevel)
{
BEGIN_RCPP
    XPtr<DiffusionModel> modelPtr(_model);
    DiffusionModel *model = modelPtr;
    
    Tracker tracker(model);
    
    NiftiImage *mask = new NiftiImage(as<std::string>(_maskPath));
    tracker.setMask(mask);
    tracker.setDebugLevel(as<int>(_debugLevel));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(_useLoopcheck);
    flags["terminate-targets"] = as<bool>(_terminateAtTargets);
    flags["terminate-outside"] = as<bool>(_terminateOutsideMask);
    flags["must-leave"] = as<bool>(_mustLeaveMask);
    tracker.setFlags(flags);
    
    Space<3>::Vector rightwardsVector;
    if (Rf_isNull(_rightwardsVector))
        rightwardsVector = Space<3>::zeroVector();
    else
        rightwardsVector = as<arma::fvec>(_rightwardsVector);
    tracker.setRightwardsVector(rightwardsVector);
    
    tracker.setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker.setStepLength(as<float>(_stepLength));
    tracker.setMaxSteps(as<int>(_maxSteps));
    
    if (!Rf_isNull(_targetPath))
    {
        NiftiImage targets(as<std::string>(_targetPath));
        tracker.setTargets(&targets);
    }
    
    RNGScope scope;
    
    TractographyDataSource dataSource(&tracker, as<arma::fmat>(_seeds) - 1.0, as<size_t>(_count), as<bool>(_jitter));
    Pipeline<Streamline> pipeline(&dataSource);
    
    LabelCountFilter *hitFilter = NULL;
    LengthFilter *lengthFilter = NULL;
    
    const int minTargetHits = as<int>(_minTargetHits);
    if (minTargetHits > 0)
    {
        hitFilter = new LabelCountFilter(minTargetHits);
        pipeline.addManipulator(hitFilter);
    }
    
    const double minLength = as<double>(_minLength);
    if (minLength > 0.0)
    {
        lengthFilter = new LengthFilter(minLength);
        pipeline.addManipulator(lengthFilter);
    }
    
    VisitationMapDataSink *visitationMap = NULL;
    TrackvisDataSink *trkFile = NULL;
    if (!Rf_isNull(_mapPath))
    {
        visitationMap = new VisitationMapDataSink(mask->getDimensions());
        pipeline.addSink(visitationMap);
    }
    if (!Rf_isNull(_trkPath))
    {
        trkFile = new TrackvisDataSink(as<std::string>(_trkPath), *mask);
        pipeline.addSink(trkFile);
    }
    
    pipeline.run();
    
    if (visitationMap != NULL)
    {
        visitationMap->writeToNifti(*mask, as<std::string>(_mapPath));
        delete visitationMap;
    }
    
    delete hitFilter;
    delete lengthFilter;
    delete trkFile;
    delete mask;
    
    return R_NilValue;
END_RCPP
}
