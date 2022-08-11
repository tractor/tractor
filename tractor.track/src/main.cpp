#include <Rcpp.h>

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

using namespace Rcpp;

typedef std::vector<int> int_vector;
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

RcppExport SEXP createTracker (SEXP _model, SEXP _mask, SEXP _targetInfo, SEXP _maxSteps, SEXP _stepLength, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _oneWay, SEXP _terminateAtTargets, SEXP _debugLevel)
{
BEGIN_RCPP
    XPtr<DiffusionModel> modelPtr(_model);
    DiffusionModel *model = modelPtr;
    ImageSpace *space = model->imageSpace();
    const std::string orientation = space->orientation();
    
    Tracker *tracker = new Tracker(model);
    
    RNifti::NiftiImage mask(_mask);
    mask.reorient(orientation);
    tracker->setMask(mask);
    tracker->setDebugLevel(as<int>(_debugLevel));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(_useLoopcheck);
    flags["one-way"] = as<bool>(_oneWay);
    flags["terminate-targets"] = as<bool>(_terminateAtTargets);
    tracker->setFlags(flags);
    
    tracker->setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker->setStepLength(as<float>(_stepLength));
    tracker->setMaxSteps(as<int>(_maxSteps));
    
    List targetInfo(_targetInfo);
    if (!Rf_isNull(targetInfo["path"]))
    {
        RNifti::NiftiImage targets(as<std::string>(targetInfo["path"]));
        tracker->setTargets(targets.reorient(orientation));
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
    
    return XPtr<Tracker>(tracker);
END_RCPP
}

RcppExport SEXP track (SEXP _tracker, SEXP _seeds, SEXP _count, SEXP _rightwardsVector, SEXP _minTargetHits, SEXP _minLength, SEXP _maxLength, SEXP _jitter, SEXP _mapPath, SEXP _trkPath, SEXP _medianPath, SEXP _profileFunction, SEXP _debugLevel)
{
BEGIN_RCPP
    XPtr<Tracker> trackerPtr(_tracker);
    Tracker *tracker = trackerPtr;
    ImageSpace *space = tracker->getModel()->imageSpace();
    
    ImageSpace::Vector rightwardsVector;
    if (Rf_isNull(_rightwardsVector))
        rightwardsVector = ImageSpace::zeroVector();
    else
    {
        NumericVector rightwardsVectorR(_rightwardsVector);
        if (rightwardsVectorR.length() != 3)
            throw std::runtime_error("Rightwards vector should be a point in 3D space");
        for (int i=0; i<3; i++)
            rightwardsVector[i] = rightwardsVectorR[i];
    }
    tracker->setRightwardsVector(rightwardsVector);
    
    // Before TractographyDataSource is initialised, as RNG needed for jitter
    RNGScope scope;
    
    NumericMatrix seedsR(_seeds);
    if (seedsR.ncol() != 3)
        throw std::runtime_error("Seed matrix must have three columns");
    std::vector<ImageSpace::Point> seeds;
    for (int i=0; i<seedsR.nrow(); i++)
    {
        ImageSpace::Point seed;
        std::transform(seedsR.row(i).begin(), seedsR.row(i).end(), &seed[0], [](double &x) { return x - 1.0; });
        seeds.push_back(seed);
    }
    TractographyDataSource dataSource(tracker, seeds, as<size_t>(_count), as<bool>(_jitter));
    Pipeline<Streamline> pipeline(&dataSource);
    
    const int minTargetHits = as<int>(_minTargetHits);
    if (minTargetHits > 0)
        pipeline.addManipulator(new LabelCountFilter(minTargetHits));
    
    const double minLength = as<double>(_minLength);
    double maxLength = as<double>(_maxLength);
    maxLength = (maxLength == R_PosInf ? 0.0 : maxLength);
    if (minLength > 0.0 || maxLength > 0.0)
        pipeline.addManipulator(new LengthFilter(minLength, maxLength));
    
    VisitationMapDataSink *visitationMap = nullptr;
    if (!Rf_isNull(_mapPath))
    {
        visitationMap = new VisitationMapDataSink(space->dim);
        pipeline.addSink(visitationMap);
    }
    if (!Rf_isNull(_trkPath))
    {
        StreamlineFileSink *trkFile = new StreamlineFileSink(as<std::string>(_trkPath));
        trkFile->labelDictionary() = tracker->labelDictionary();
        pipeline.addSink(trkFile);
    }
    // Only one of trkPath and medianPath is used
    else if (!Rf_isNull(_medianPath))
    {
        pipeline.addManipulator(new MedianStreamlineFilter);
        pipeline.addSink(new StreamlineFileSink(as<std::string>(_medianPath)));
        
        // Pipeline must contain all streamlines at once for calculating median
        pipeline.setBlockSize(as<size_t>(_count));
    }
    
    Rcpp::Function *function = nullptr;
    if (!Rf_isNull(_profileFunction))
    {
        function = new Rcpp::Function(_profileFunction);
        pipeline.addSink(new ProfileMatrixDataSink(*function));
    }
    
    size_t nRetained = pipeline.run();
    
    if (visitationMap != nullptr)
        visitationMap->writeToNifti(as<std::string>(_mapPath), space);
    
    delete function;
    
    return wrap(nRetained);
END_RCPP
}

RcppExport SEXP trkOpen (SEXP _trkPath, SEXP _readLabels)
{
BEGIN_RCPP
    StreamlineFileSource *source = new StreamlineFileSource(as<std::string>(_trkPath), as<bool>(_readLabels));
    
    StreamlineFileMetadata *metadata = source->fileMetadata();
    std::vector<std::string> properties = metadata->properties;
    if (properties.size() == 0)
        properties.push_back("(none)");
    
    List result;
    result["count"] = metadata->count;
    result["labels"] = source->hasLabels();
    result["properties"] = properties;
    result["pointer"] = XPtr<StreamlineFileSource>(source);
    
    return result;
END_RCPP
}

RcppExport SEXP trkApply (SEXP _source, SEXP _indices, SEXP _function)
{
BEGIN_RCPP
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source);
    pipeline.setSubset(_indices);
    Function function(_function);
    pipeline.addSink(new RCallbackDataSink(function));
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkFastMapAndLengths (SEXP _source, SEXP _indices)
{
BEGIN_RCPP
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source);
    pipeline.setSubset(_indices);
    
    VisitationMapDataSink *map = new VisitationMapDataSink(source->imageSpace()->dim);
    pipeline.addSink(map);
    
    StreamlineLengthsDataSink *lengths = new StreamlineLengthsDataSink;
    pipeline.addSink(lengths);
    
    pipeline.run();
    
    return List::create(Named("map")=map->getImage(), Named("lengths")=lengths->getLengths());
END_RCPP
}

RcppExport SEXP trkFind (SEXP _source, SEXP _labels)
{
BEGIN_RCPP
    XPtr<StreamlineFileSource> source(_source);
    
    if (!source->hasLabels())
        Rf_error("Streamline source has no label information");
    
    std::vector<size_t> indices = source->matchLabels(as<int_vector>(_labels));
    std::transform(indices.begin(), indices.end(), indices.begin(), [](const size_t x) { return static_cast<int>(x+1); });
    return wrap(indices);
END_RCPP
}

RcppExport SEXP trkLengths (SEXP _source, SEXP _indices)
{
BEGIN_RCPP
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source);
    pipeline.setSubset(_indices);
    
    StreamlineLengthsDataSink *sink = new StreamlineLengthsDataSink;
    pipeline.addSink(sink);
    pipeline.run();
    
    return wrap(sink->getLengths());
END_RCPP
}

RcppExport SEXP trkMap (SEXP _source, SEXP _indices, SEXP _imagePath, SEXP _scope, SEXP _normalise, SEXP _resultPath)
{
BEGIN_RCPP
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source);
    pipeline.setSubset(_indices);
    
    const std::string scopeString = as<std::string>(_scope);
    VisitationMapDataSink::MappingScope scope = VisitationMapDataSink::MappingScope::All;
    if (scopeString == "seed")
        scope = VisitationMapDataSink::MappingScope::Seed;
    else if (scopeString == "ends")
        scope = VisitationMapDataSink::MappingScope::Ends;
    
    RNifti::NiftiImage reference(as<std::string>(_imagePath), false);
    VisitationMapDataSink *map = new VisitationMapDataSink(reference.dim(), scope, as<bool>(_normalise));
    pipeline.addSink(map);
    
    pipeline.run();
    
    ImageSpace space(reference);
    map->writeToNifti(as<std::string>(_resultPath), &space);
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkMedian (SEXP _source, SEXP _indices, SEXP _resultPath, SEXP _quantile)
{
BEGIN_RCPP
    // Block size must match number of streamlines, as a running median can't be calculated
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source, source->count());
    pipeline.setSubset(_indices);
    
    pipeline.addManipulator(new MedianStreamlineFilter(as<double>(_quantile)));
    pipeline.addSink(new StreamlineFileSink(as<std::string>(_resultPath)));
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkTruncate (SEXP _source, SEXP _indices, SEXP _resultPath, SEXP _leftLength, SEXP _rightLength)
{
BEGIN_RCPP
    // Truncating streamlines may invalidate labels, so drop them
    XPtr<StreamlineFileSource> source(_source);
    Pipeline<Streamline> pipeline(source);
    pipeline.setSubset(_indices);
    
    StreamlineTruncator *truncator = new StreamlineTruncator(as<double>(_leftLength), as<double>(_rightLength));
    pipeline.addManipulator(truncator);
    pipeline.addSink(new StreamlineFileSink(as<std::string>(_resultPath)));
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkCreate (SEXP _trkPath, SEXP _mask)
{
BEGIN_RCPP
    const RNifti::NiftiImage mask(_mask, false);
    StreamlineFileSink *sink = new StreamlineFileSink(as<std::string>(_trkPath));
    sink->setImageSpace(new ImageSpace(mask));
    return XPtr<StreamlineFileSink>(sink);
END_RCPP
}

RcppExport SEXP trkAppend (SEXP _sink, SEXP _points, SEXP _seedIndex, SEXP _pointType, SEXP _fixedSpacing)
{
BEGIN_RCPP
    XPtr<StreamlineFileSink> sink(_sink);
    
    Rcpp::NumericMatrix pointsR(_points);
    const PointType pointType = (as<std::string>(_pointType) == "mm" ? PointType::World : PointType::Voxel);
    const int seedIndex = as<int>(_seedIndex) - 1;
    
    std::vector<ImageSpace::Point> leftPoints(seedIndex+1), rightPoints(pointsR.rows()-seedIndex);
    for (int i=seedIndex; i>=0; i--)
    {
        ImageSpace::Point point;
        point[0] = pointsR(i,0) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[1] = pointsR(i,1) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[2] = pointsR(i,2) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        leftPoints[seedIndex-i] = point;
    }
    for (int i=seedIndex; i<pointsR.rows(); i++)
    {
        ImageSpace::Point point;
        point[0] = pointsR(i,0) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[1] = pointsR(i,1) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        point[2] = pointsR(i,2) - (pointType == PointType::Voxel ? 1.0 : 0.0);
        rightPoints[i-seedIndex] = point;
    }
    
    Streamline streamline(leftPoints, rightPoints, pointType, sink->fileMetadata()->space->pixdim, as<bool>(_fixedSpacing));
    std::list<Streamline> list;
    list.push_back(streamline);
    sink->setup(1);
    sink->put(streamline);
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkClose (SEXP _sink)
{
BEGIN_RCPP
    XPtr<StreamlineFileSink> sink(_sink);
    sink->done();
    sink.release();
    return R_NilValue;
END_RCPP
}

RcppExport SEXP tck2trk (SEXP _tckPath, SEXP _image)
{
BEGIN_RCPP
    const std::string path = as<std::string>(_tckPath);
    const RNifti::NiftiImage image(_image, false);
    
    StreamlineFileSource tckFile(path);
    Pipeline<Streamline> pipeline(&tckFile);
    
    StreamlineFileSink *sink = new StreamlineFileSink(path);
    sink->setImageSpace(new ImageSpace(image));
    pipeline.addSink(sink);
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}
