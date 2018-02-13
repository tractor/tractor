#include <RcppEigen.h>

#include "Space.h"
#include "RNifti.h"
#include "Streamline.h"
#include "DiffusionModel.h"
#include "Tracker.h"
#include "Filter.h"
#include "Trackvis.h"
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

template <typename OriginalType, typename FinalType>
FinalType decrement (OriginalType x)
{
    return static_cast<FinalType>(x - OriginalType(1));
}

template <typename OriginalType, typename FinalType>
FinalType increment (OriginalType x)
{
    return static_cast<FinalType>(x + OriginalType(1));
}

RcppExport SEXP track (SEXP _model, SEXP _seeds, SEXP _count, SEXP _maskPath, SEXP _targetInfo, SEXP _rightwardsVector, SEXP _maxSteps, SEXP _stepLength, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _terminateAtTargets, SEXP _minTargetHits, SEXP _minLength, SEXP _maxLength, SEXP _jitter, SEXP _mapPath, SEXP _trkPath, SEXP _medianPath, SEXP _profileFunction, SEXP _debugLevel)
{
BEGIN_RCPP
    XPtr<DiffusionModel> modelPtr(_model);
    DiffusionModel *model = modelPtr;
    const Grid<3> &grid = model->getGrid3D();
    const std::string gridOrientation = grid3DOrientation(grid);
    
    Tracker tracker(model);
    
    RNifti::NiftiImage mask(as<std::string>(_maskPath));
    mask.reorient(gridOrientation);
    tracker.setMask(mask);
    tracker.setDebugLevel(as<int>(_debugLevel));
    
    std::map<std::string,bool> flags;
    flags["loopcheck"] = as<bool>(_useLoopcheck);
    flags["terminate-targets"] = as<bool>(_terminateAtTargets);
    tracker.setFlags(flags);
    
    Space<3>::Vector rightwardsVector;
    if (Rf_isNull(_rightwardsVector))
        rightwardsVector = Space<3>::zeroVector();
    else
        rightwardsVector = as<Eigen::VectorXf>(_rightwardsVector);
    tracker.setRightwardsVector(rightwardsVector);
    
    tracker.setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker.setStepLength(as<float>(_stepLength));
    tracker.setMaxSteps(as<int>(_maxSteps));
    
    List targetInfo(_targetInfo);
    if (!Rf_isNull(targetInfo["path"]))
    {
        RNifti::NiftiImage targets(as<std::string>(targetInfo["path"]));
        tracker.setTargets(targets.reorient(gridOrientation));
    }
    
    RNGScope scope;
    
    NumericMatrix seedsR(_seeds);
    Eigen::MatrixXf seeds(seedsR.rows(), seedsR.cols());
    std::transform(seedsR.begin(), seedsR.end(), seeds.data(), decrement<double,float>);
    TractographyDataSource dataSource(&tracker, seeds.array(), as<size_t>(_count), as<bool>(_jitter));
    Pipeline<Streamline> pipeline(&dataSource);
    
    const int minTargetHits = as<int>(_minTargetHits);
    if (minTargetHits > 0)
        pipeline.addManipulator(new LabelCountFilter(minTargetHits));
    
    const double minLength = as<double>(_minLength);
    double maxLength = as<double>(_maxLength);
    maxLength = (maxLength == R_PosInf ? 0.0 : maxLength);
    if (minLength > 0.0 || maxLength > 0.0)
        pipeline.addManipulator(new LengthFilter(minLength, maxLength));
    
    VisitationMapDataSink *visitationMap = NULL;
    Rcpp::Function *function = NULL;
    if (!Rf_isNull(_mapPath))
    {
        visitationMap = new VisitationMapDataSink(mask.dim());
        pipeline.addSink(visitationMap);
    }
    if (!Rf_isNull(_trkPath))
    {
        TrackvisDataSink *trkFile;
        
        if (!Rf_isNull(targetInfo["indices"]) && !Rf_isNull(targetInfo["labels"]))
        {
            IntegerVector indices = targetInfo["indices"];
            CharacterVector labels = targetInfo["labels"];
            std::map<int,std::string> labelDictionary;
            for (int i=0; i<std::min(indices.size(),labels.size()); i++)
                labelDictionary[indices[i]] = labels[i];
            trkFile = new LabelledTrackvisDataSink(as<std::string>(_trkPath), grid, labelDictionary);
        }
        else
            trkFile = new BasicTrackvisDataSink(as<std::string>(_trkPath), grid);
        
        pipeline.addSink(trkFile);
    }
    if (!Rf_isNull(_medianPath))
    {
        pipeline.addSink(new MedianTrackvisDataSink(as<std::string>(_medianPath), grid));
        
        // Pipeline must contain all streamlines at once for calculating median
        pipeline.setBlockSize(as<size_t>(_count));
    }
    if (!Rf_isNull(_profileFunction))
    {
        function = new Rcpp::Function(_profileFunction);
        pipeline.addSink(new ProfileMatrixDataSink(*function));
    }
    
    size_t nRetained = pipeline.run();
    
    if (visitationMap != NULL)
        visitationMap->writeToNifti(mask, as<std::string>(_mapPath));
    
    delete function;
    
    return wrap(nRetained);
END_RCPP
}

RcppExport SEXP trkApply (SEXP _trkPath, SEXP _indices, SEXP _function)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    Function function(_function);
    pipeline.addSink(new RCallbackDataSink(function));
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkCount (SEXP _trkPath)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    return wrap(trkFile.nStreamlines());
END_RCPP
}

RcppExport SEXP trkFastMapAndLengths (SEXP _trkPath, SEXP _indices, SEXP _pointer)
{
BEGIN_RCPP
    StreamlineLabelList *labelList = NULL;
    if (!Rf_isNull(_pointer))
    {
        XPtr<StreamlineLabelList> labelsPtr(_pointer);
        labelList = labelsPtr;
    }
    
    // A labelled source is used for speed, since it stores the streamline offsets for seeking
    LabelledTrackvisDataSource trkFile(as<std::string>(_trkPath), labelList);
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    
    int_vector dims(3);
    const Grid<3> &grid = trkFile.getGrid3D();
    std::copy(grid.dimensions().data(), grid.dimensions().data()+3, dims.begin());
    VisitationMapDataSink *map = new VisitationMapDataSink(dims);
    pipeline.addSink(map);
    
    StreamlineLengthsDataSink *lengths = new StreamlineLengthsDataSink;
    pipeline.addSink(lengths);
    
    pipeline.run();
    
    const Array<double> &array = map->getArray();
    NumericVector arrayR = wrap(array.getData());
    arrayR.attr("dim") = array.getDimensions();
    List result = List::create(Named("map")=arrayR, Named("lengths")=lengths->getLengths());
    return result;
END_RCPP
}

RcppExport SEXP trkFind (SEXP _trkPath, SEXP _labels, SEXP _pointer)
{
BEGIN_RCPP
    std::vector<int> indices;
    if (Rf_isNull(_pointer))
    {
        StreamlineLabelList labelList(as<std::string>(_trkPath));
        indices = labelList.find(as<int_vector>(_labels));
    }
    else
    {
        XPtr<StreamlineLabelList> labelsPtr(_pointer);
        StreamlineLabelList *labelList = labelsPtr;
        indices = labelList->find(as<int_vector>(_labels));
    }
    std::transform(indices.begin(), indices.end(), indices.begin(), increment<int,int>);
    return wrap(indices);
END_RCPP
}

RcppExport SEXP trkInfo (SEXP _trkPath)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    std::vector<std::string> properties = trkFile.getPropertyNames();
    if (properties.size() == 0)
        properties.push_back("(none)");
    return wrap(properties);
END_RCPP
}

RcppExport SEXP trkLabels (SEXP _trkPath)
{
BEGIN_RCPP
    StreamlineLabelList *labelList = new StreamlineLabelList(as<std::string>(_trkPath));
    XPtr<StreamlineLabelList> labelsPtr(labelList);
    return labelsPtr;
END_RCPP
}

RcppExport SEXP trkLengths (SEXP _trkPath, SEXP _indices)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    
    StreamlineLengthsDataSink *sink = new StreamlineLengthsDataSink;
    pipeline.addSink(sink);
    
    pipeline.run();
    
    return wrap(sink->getLengths());
END_RCPP
}

RcppExport SEXP trkMap (SEXP _trkPath, SEXP _indices, SEXP _imagePath, SEXP _scope, SEXP _normalise, SEXP _resultPath)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    
    const std::string scopeString = as<std::string>(_scope);
    VisitationMapDataSink::MappingScope scope = VisitationMapDataSink::FullMappingScope;
    if (scopeString == "seed")
        scope = VisitationMapDataSink::SeedMappingScope;
    else if (scopeString == "ends")
        scope = VisitationMapDataSink::EndsMappingScope;
    
    int_vector dims(3);
    const Grid<3> &grid = trkFile.getGrid3D();
    std::copy(grid.dimensions().data(), grid.dimensions().data()+3, dims.begin());
    VisitationMapDataSink *map = new VisitationMapDataSink(dims, scope, as<bool>(_normalise));
    pipeline.addSink(map);
    
    pipeline.run();
    
    RNifti::NiftiImage reference(as<std::string>(_imagePath), false);
    map->writeToNifti(reference, as<std::string>(_resultPath));
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkMedian (SEXP _trkPath, SEXP _indices, SEXP _resultPath, SEXP _quantile)
{
BEGIN_RCPP
    // Block size must match number of streamlines, as a running median can't be calculated
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile, trkFile.nStreamlines());
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    MedianTrackvisDataSink *medianFile = new MedianTrackvisDataSink(as<std::string>(_resultPath), trkFile.getGrid3D(), as<double>(_quantile));
    pipeline.addSink(medianFile);
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkTruncate (SEXP _trkPath, SEXP _indices, SEXP _resultPath, SEXP _leftLength, SEXP _rightLength)
{
BEGIN_RCPP
    // BasicTrackvisDataSource does not read labels, but when truncating they may not be preserved anyway
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    StreamlineTruncator *truncator = new StreamlineTruncator(as<double>(_leftLength), as<double>(_rightLength));
    pipeline.addManipulator(truncator);
    BasicTrackvisDataSink *resultFile = new BasicTrackvisDataSink(as<std::string>(_resultPath), trkFile.getGrid3D());
    pipeline.addSink(resultFile);
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkCreate (SEXP _trkPath, SEXP _mask)
{
BEGIN_RCPP
    RNifti::NiftiImage mask(_mask);
    BasicTrackvisDataSink *sink = new BasicTrackvisDataSink(as<std::string>(_trkPath), getGrid3D(mask));
    XPtr<BasicTrackvisDataSink> sinkPtr(sink);
    return sinkPtr;
END_RCPP
}

RcppExport SEXP trkAppend (SEXP _sink, SEXP _points, SEXP _seedIndex, SEXP _pointType, SEXP _fixedSpacing)
{
BEGIN_RCPP
    XPtr<BasicTrackvisDataSink> sinkPtr(_sink);
    BasicTrackvisDataSink *sink = sinkPtr;
    
    Rcpp::NumericMatrix pointsR(_points);
    const Streamline::PointType pointType = (as<std::string>(_pointType) == "mm" ? Streamline::WorldPointType : Streamline::VoxelPointType);
    const int seedIndex = as<int>(_seedIndex) - 1;
    
    std::vector<Space<3>::Point> leftPoints(seedIndex+1), rightPoints(pointsR.rows()-seedIndex);
    for (int i=seedIndex; i>=0; i--)
    {
        Space<3>::Point point;
        point[0] = pointsR(i,0) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        point[1] = pointsR(i,1) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        point[2] = pointsR(i,2) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        leftPoints[seedIndex-i] = point;
    }
    for (int i=seedIndex; i<pointsR.rows(); i++)
    {
        Space<3>::Point point;
        point[0] = pointsR(i,0) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        point[1] = pointsR(i,1) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        point[2] = pointsR(i,2) - (pointType == Streamline::VoxelPointType ? 1.0 : 0.0);
        rightPoints[i-seedIndex] = point;
    }
    
    Streamline streamline(leftPoints, rightPoints, pointType, sink->getGrid3D().spacings(), as<bool>(_fixedSpacing));
    std::list<Streamline> list;
    list.push_back(streamline);
    sink->setup(1, list.begin(), list.end());
    sink->put(streamline);
    
    return R_NilValue;
END_RCPP
}

RcppExport SEXP trkClose (SEXP _sink)
{
    XPtr<BasicTrackvisDataSink> sinkPtr(_sink);
    BasicTrackvisDataSink *sink = sinkPtr;
    sink->done();
    sinkPtr.release();
    return R_NilValue;
}
