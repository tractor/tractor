#include "RNifti.h"
#include "RNiftiAPI.h"

#include "Image.h"
#include "Streamline.h"
#include "DiffusionModel.h"
#include "Tracker.h"
#include "Filter.h"
#include "Files.h"
#include "VisitationMap.h"
#include "RCallback.h"
#include "Pipeline.h"

#include <Rcpp.h>

using namespace Rcpp;

typedef std::vector<std::string> str_vector;

RcppExport SEXP createDtiModel (SEXP _principalDirectionsPath)
{
BEGIN_RCPP
    DiffusionTensorModel *model = new DiffusionTensorModel(as<std::string>(_principalDirectionsPath));
    XPtr<DiffusionModel> modelPtr(model);
    return modelPtr;
END_RCPP
}

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

RcppExport SEXP createTracker (SEXP _model, SEXP _mask, SEXP _maxSteps, SEXP _stepLength, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _oneWay)
{
BEGIN_RCPP
    XPtr<DiffusionModel> modelPtr(_model);
    DiffusionModel *model = modelPtr;
    ImageSpace *space = model->imageSpace();
    
    Tracker *tracker = new Tracker(model);
    
    RNifti::NiftiImage mask(_mask);
    mask.reorient(space->orientation());
    tracker->setMask(mask);
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(_useLoopcheck);
    flags["one-way"] = as<bool>(_oneWay);
    tracker->setFlags(flags);
    
    tracker->setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker->setStepLength(as<float>(_stepLength));
    tracker->setMaxSteps(as<int>(_maxSteps));
    
    return XPtr<Tracker>(tracker);
END_RCPP
}

RcppExport SEXP setTrackerTargets (SEXP _tracker, SEXP _targetInfo, SEXP _terminateAtTargets)
{
BEGIN_RCPP
    XPtr<Tracker> trackerPtr(_tracker);
    Tracker *tracker = trackerPtr;
    ImageSpace *space = tracker->getModel()->imageSpace();
    
    List targetInfo(_targetInfo);
    if (Rf_isNull(targetInfo["image"]))
        tracker->clearTargets();
    else
    {
        RObject targets = targetInfo["image"];
        RNifti::NiftiImage targetImage(targets);
        tracker->setTargets(targetImage.reorient(space->orientation()));
    }
    
    if (!Rf_isNull(targetInfo["indices"]) && !Rf_isNull(targetInfo["labels"]))
    {
        IntegerVector indices = targetInfo["indices"];
        CharacterVector labels = targetInfo["labels"];
        std::map<int,std::string> labelDictionary;
        for (int i=0; i<std::min(indices.size(),labels.size()); i++)
            labelDictionary[indices[i]] = labels[i];
        
        tracker->labelDictionary() = labelDictionary;
    }
    
    tracker->setFlag("terminate-targets", as<bool>(_terminateAtTargets));
    return R_NilValue;
END_RCPP
}

RcppExport SEXP initialiseTracker (SEXP _tracker, SEXP _seeds, SEXP _count, SEXP _rightwardsVector, SEXP _jitter)
{
BEGIN_RCPP
    XPtr<Tracker> trackerPtr(_tracker);
    Tracker *tracker = trackerPtr;
    
    ImageSpace::Vector rightwardsVector = ImageSpace::zeroVector();
    if (!Rf_isNull(_rightwardsVector))
    {
        NumericVector rightwardsVectorR(_rightwardsVector);
        if (rightwardsVectorR.length() != 3)
            throw Rcpp::exception("Rightwards vector should be a point in 3D space");
        for (int i=0; i<3; i++)
            rightwardsVector[i] = rightwardsVectorR[i];
    }
    tracker->setRightwardsVector(rightwardsVector);
    
    NumericMatrix seedsR(_seeds);
    if (seedsR.ncol() != 3)
        throw Rcpp::exception("Seed matrix must have three columns");
    
    std::vector<ImageSpace::Point> seeds;
    for (int i=0; i<seedsR.nrow(); i++)
    {
        ImageSpace::Point seed;
        std::transform(seedsR.row(i).begin(), seedsR.row(i).end(), &seed[0], [](const double &x) { return x - 1.0; });
        seeds.push_back(seed);
    }
    
    DataSource<Streamline> *source = new TractographyDataSource(tracker, seeds, as<size_t>(_count), as<bool>(_jitter));
    Pipeline<Streamline> *pipeline = new Pipeline<Streamline>(source);
    return XPtr<Pipeline<Streamline>>(pipeline);
END_RCPP
}

RcppExport SEXP trkOpen (SEXP _path, SEXP _readLabels)
{
BEGIN_RCPP
    StreamlineFileSource *source = new StreamlineFileSource(as<std::string>(_path), as<bool>(_readLabels));
    StreamlineFileMetadata *metadata = source->fileMetadata();
    Pipeline<Streamline> *pipeline = new Pipeline<Streamline>(source);
    
    List result;
    result["count"] = metadata->count;
    result["labels"] = source->hasLabels();
    result["properties"] = metadata->properties;
    result["pointer"] = XPtr<Pipeline<Streamline>>(pipeline);
    
    return result;
END_RCPP
}

RcppExport SEXP createListSource (SEXP _list)
{
BEGIN_RCPP
    RListDataSource *source = new RListDataSource(_list);
    Pipeline<Streamline> *pipeline = new Pipeline<Streamline>(source);
    return XPtr<Pipeline<Streamline>>(pipeline);
END_RCPP
}

RcppExport SEXP setFilters (SEXP _pipeline, SEXP _minLabels, SEXP _maxLabels, SEXP _minLength, SEXP _maxLength, SEXP _medianOnly, SEXP _medianQuantileLength)
{
BEGIN_RCPP
    Pipeline<Streamline> *pipeline = XPtr<Pipeline<Streamline>>(_pipeline).checked_get();
    
    pipeline->clearManipulators();
    
    const int minLabels = as<int>(_minLabels);
    const int maxLabels = as<int>(_maxLabels);
    if (minLabels > 0 || maxLabels > 0)
        pipeline->addManipulator(new LabelCountFilter(minLabels, maxLabels));
    
    const double minLength = as<double>(_minLength);
    double maxLength = as<double>(_maxLength);
    maxLength = (maxLength == R_PosInf ? 0.0 : maxLength);
    if (minLength > 0.0 || maxLength > 0.0)
        pipeline->addManipulator(new LengthFilter(minLength, maxLength));
    
    if (as<bool>(_medianOnly))
    {
        pipeline->addManipulator(new MedianStreamlineFilter(as<double>(_medianQuantileLength)));
        
        // Calculating a median requires all streamlines to be in one block
        const size_t count = pipeline->dataSource()->count();
        if (count == 0)
            throw Rcpp::exception("Streamline source has zero or unknown size - can't calculate a median");
        pipeline->setBlockSize(count);
    }
    return R_NilValue;
END_RCPP
}

RcppExport SEXP runPipeline (SEXP _pipeline, SEXP _selection, SEXP _path, SEXP _requireStreamlines, SEXP _requireMap, SEXP _mapScope, SEXP _normaliseMap, SEXP _requireProfile, SEXP _requireLengths, SEXP _leftLength, SEXP _rightLength, SEXP _refImage, SEXP _debugLevel, SEXP _streamlineFun)
{
BEGIN_RCPP
    Pipeline<Streamline> *pipeline = XPtr<Pipeline<Streamline>>(_pipeline).checked_get();
    pipeline->setSubset(_selection);
    
    Tracker *tracker = nullptr;
    ImageSpace *space = nullptr;
    bool sharedSpace = true;
    const std::string sourceType = pipeline->dataSource()->type();
    if (sourceType == "tracker")
    {
        tracker = static_cast<TractographyDataSource *>(pipeline->dataSource())->streamlineTracker();
        tracker->setDebugLevel(as<int>(_debugLevel));
        space = tracker->getModel()->imageSpace();
    }
    else if (sourceType == "file")
        space = static_cast<StreamlineFileSource *>(pipeline->dataSource())->imageSpace();
    else if (sourceType == "list")
        space = static_cast<RListDataSource *>(pipeline->dataSource())->imageSpace();
    
    if (!Rf_isNull(_refImage) && space == nullptr)
    {
        const RNifti::NiftiImage image(_refImage, false, true);
        space = new ImageSpace(image);
        sharedSpace = false;
    }
    
    const std::string path = as<std::string>(_path);
    
    if (!Rf_isNull(_leftLength) || !Rf_isNull(_rightLength))
        pipeline->addManipulator(new StreamlineTruncator(as<double>(_leftLength), as<double>(_rightLength)));
    
    std::map<std::string,bool> requirements;
    requirements["file"] = as<bool>(_requireStreamlines) && !path.empty();
    requirements["list"] = as<bool>(_requireStreamlines) && path.empty();
    requirements["map"] = as<bool>(_requireMap);
    requirements["profile"] = as<bool>(_requireProfile);
    requirements["lengths"] = as<bool>(_requireLengths);
    
    // For jitter and probabilistic interpolation
    RNGScope rng;
    
    if (requirements["file"])
    {
        StreamlineFileSink *trkFile = new StreamlineFileSink(path);
        trkFile->setImageSpace(space);
        if (tracker != nullptr)
            trkFile->labelDictionary() = tracker->labelDictionary();
        pipeline->addSink(trkFile);
    }
    
    RListDataSink *list = nullptr;
    if (requirements["list"])
    {
        list = new RListDataSink(_streamlineFun);
        pipeline->addSink(list);
    }
    
    VisitationMapDataSink *visitationMap = nullptr;
    if (requirements["map"])
    {
        if (space == nullptr)
            throw Rcpp::exception("Visitation map cannot be created because the image space is unknown");
        
        const std::string scopeString = as<std::string>(_mapScope);
        VisitationMapDataSink::MappingScope scope = VisitationMapDataSink::MappingScope::All;
        if (scopeString == "seed")
            scope = VisitationMapDataSink::MappingScope::Seed;
        else if (scopeString == "ends")
            scope = VisitationMapDataSink::MappingScope::Ends;
        
        visitationMap = new VisitationMapDataSink(space, scope, as<bool>(_normaliseMap));
        pipeline->addSink(visitationMap);
    }
    
    LabelProfileDataSink *profile = nullptr;
    if (requirements["profile"])
    {
        profile = new LabelProfileDataSink;
        pipeline->addSink(profile);
    }
    
    StreamlineLengthsDataSink *lengths = nullptr;
    if (requirements["lengths"])
    {
        lengths = new StreamlineLengthsDataSink;
        pipeline->addSink(lengths);
    }
    
    // Run the pipeline, storing outputs in files and/or sink objects
    const size_t count = pipeline->run();
    
    List result;
    result["count"] = count;
    
    if (requirements["map"])
        result["map"] = visitationMap->getImage().toNifti(DT_FLOAT64).toPointer("visitation map");
    if (requirements["list"])
        result["streamlines"] = list->getList();
    if (requirements["profile"])
        result["profile"] = profile->getProfile();
    if (requirements["lengths"])
        result["lengths"] = lengths->getLengths();
    
    // Reset the source and clear all sinks and manipulators
    pipeline->reset();
    
    if (!sharedSpace)
        delete space;
    
    return result;
END_RCPP
}

RcppExport SEXP trkFind (SEXP _pipeline, SEXP _labels, SEXP _map, SEXP _combine)
{
BEGIN_RCPP
    Pipeline<Streamline> *pipeline = XPtr<Pipeline<Streamline>>(_pipeline).checked_get();
    const std::vector<int> labels = as<std::vector<int>>(_labels);
    
    bool labelIndexAvailable = false;
    StreamlineFileSource *source = nullptr;
    if (pipeline->dataSource()->type() == "file")
    {
        source = static_cast<StreamlineFileSource *>(pipeline->dataSource());
        labelIndexAvailable = source->hasLabels();
    }
    else if (pipeline->dataSource()->type() == "tracker")
        Rf_warning("Streamlines from a tracker source are generally not stable, so indices may be unreliable");
    
    // Convert the string "combine" argument to an enum value
    const StreamlineLabelMatcher::CombineOperation combine = std::unordered_map<std::string,StreamlineLabelMatcher::CombineOperation>({
        { "none", StreamlineLabelMatcher::CombineOperation::None },
        { "and",  StreamlineLabelMatcher::CombineOperation::And },
        { "or",   StreamlineLabelMatcher::CombineOperation::Or }
    }).at(as<std::string>(_combine));
    
    // Create the label matcher
    StreamlineLabelMatcher *matcher = new StreamlineLabelMatcher(labels, combine);
    bool matcherOwned = true;
    
    // If a map has been specified, it takes priority
    // In this case the streamlines are relabelled and piped to the matcher
    if (!Rf_isNull(_map) || !labelIndexAvailable)
    {
        // The labeller needs to be the only manipulator so that its indices will be right
        if (!Rf_isNull(_map))
        {
            pipeline->clearManipulators();
            pipeline->addManipulator(new StreamlineLabeller(_map));
        }
        pipeline->addSink(matcher);
        pipeline->run();
        
        // The pipeline object will clear up
        matcherOwned = false;
    }
    else if (labelIndexAvailable)           // Must be true, but kept for clarity
        matcher->put(source->labelList());
    
    std::vector<std::vector<size_t>> indices = matcher->getMatches();
    
    for (std::vector<size_t> &ind : indices)
        std::transform(ind.begin(), ind.end(), ind.begin(), [](const size_t x) { return x+1; });
    
    pipeline->reset();
    
    if (matcherOwned)
        delete matcher;
    
    return wrap(indices);
END_RCPP
}
