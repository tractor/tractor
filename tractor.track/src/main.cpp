#include <RcppEigen.h>

#include "Space.h"
#include "NiftiImage.h"
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

RcppExport SEXP track (SEXP _model, SEXP _seeds, SEXP _count, SEXP _maskPath, SEXP _targetInfo, SEXP _rightwardsVector, SEXP _maxSteps, SEXP _stepLength, SEXP _curvatureThreshold, SEXP _useLoopcheck, SEXP _terminateAtTargets, SEXP _minTargetHits, SEXP _minLength, SEXP _terminateOutsideMask, SEXP _mustLeaveMask, SEXP _jitter, SEXP _mapPath, SEXP _trkPath, SEXP _medianPath, SEXP _profileFunction, SEXP _debugLevel)
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
        rightwardsVector = as<Eigen::VectorXf>(_rightwardsVector);
    tracker.setRightwardsVector(rightwardsVector);
    
    tracker.setInnerProductThreshold(as<float>(_curvatureThreshold));
    tracker.setStepLength(as<float>(_stepLength));
    tracker.setMaxSteps(as<int>(_maxSteps));
    
    List targetInfo(_targetInfo);
    if (!Rf_isNull(targetInfo["path"]))
    {
        NiftiImage targets(as<std::string>(targetInfo["path"]));
        tracker.setTargets(&targets);
    }
    
    RNGScope scope;
    
    NumericMatrix seedsR(_seeds);
    Eigen::MatrixXf seeds(seedsR.rows(), seedsR.cols());
    std::transform(seedsR.begin(), seedsR.end(), seeds.data(), decrement<double,float>);
    TractographyDataSource dataSource(&tracker, seeds.array(), as<size_t>(_count), as<bool>(_jitter));
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
    MedianTrackvisDataSink *medianFile = NULL;
    ProfileMatrixDataSink *profile = NULL;
    Rcpp::Function *function = NULL;
    if (!Rf_isNull(_mapPath))
    {
        visitationMap = new VisitationMapDataSink(mask->getDimensions());
        pipeline.addSink(visitationMap);
    }
    if (!Rf_isNull(_trkPath))
    {
        if (!Rf_isNull(targetInfo["indices"]) && !Rf_isNull(targetInfo["labels"]))
        {
            IntegerVector indices = targetInfo["indices"];
            CharacterVector labels = targetInfo["labels"];
            std::map<int,std::string> labelDictionary;
            for (int i=0; i<std::min(indices.size(),labels.size()); i++)
                labelDictionary[indices[i]] = labels[i];
            trkFile = new LabelledTrackvisDataSink(as<std::string>(_trkPath), mask->getGrid3D(), labelDictionary);
        }
        else
            trkFile = new BasicTrackvisDataSink(as<std::string>(_trkPath), mask->getGrid3D());
        
        pipeline.addSink(trkFile);
    }
    if (!Rf_isNull(_medianPath))
    {
        medianFile = new MedianTrackvisDataSink(as<std::string>(_medianPath), mask->getGrid3D());
        pipeline.addSink(medianFile);
        
        // Pipeline must contain all streamlines at once for calculating median
        pipeline.setBlockSize(as<size_t>(_count));
    }
    if (!Rf_isNull(_profileFunction))
    {
        function = new Rcpp::Function(_profileFunction);
        profile = new ProfileMatrixDataSink(*function);
        pipeline.addSink(profile);
    }
    
    size_t nRetained = pipeline.run();
    
    if (visitationMap != NULL)
    {
        visitationMap->writeToNifti(*mask, as<std::string>(_mapPath));
        delete visitationMap;
    }
    
    delete hitFilter;
    delete lengthFilter;
    delete trkFile;
    delete medianFile;
    delete function;
    delete profile;
    delete mask;
    
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
    RCallbackDataSink sink(function);
    pipeline.addSink(&sink);
    
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

RcppExport SEXP trkFastMapAndLengths (SEXP _trkPath, SEXP _indices)
{
BEGIN_RCPP
    // A labelled source is used for speed, since it stores the streamline offsets for seeking
    LabelledTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    
    int_vector dims(3);
    const Grid<3> &grid = trkFile.getGrid3D();
    std::copy(grid.dimensions().data(), grid.dimensions().data()+3, dims.begin());
    VisitationMapDataSink map(dims);
    pipeline.addSink(&map);
    
    StreamlineLengthsDataSink lengths;
    pipeline.addSink(&lengths);
    
    pipeline.run();
    
    const Array<double> &array = map.getArray();
    NumericVector arrayR = wrap(array.getData());
    arrayR.attr("dim") = array.getDimensions();
    List result = List::create(Named("map")=arrayR, Named("lengths")=lengths.getLengths());
    return result;
END_RCPP
}

RcppExport SEXP trkFind (SEXP _trkPath, SEXP _labels)
{
BEGIN_RCPP
    StreamlineLabelList labelList(as<std::string>(_trkPath));
    std::vector<int> indices = labelList.find(as<int_vector>(_labels));
    std::transform(indices.begin(), indices.end(), indices.begin(), increment<int,int>);
    return wrap(indices);
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
    StreamlineLengthsDataSink sink;
    pipeline.addSink(&sink);
    
    pipeline.run();
    
    return wrap(sink.getLengths());
END_RCPP
}

RcppExport SEXP trkMap (SEXP _trkPath, SEXP _indices, SEXP _imagePath, SEXP _resultPath)
{
BEGIN_RCPP
    BasicTrackvisDataSource trkFile(as<std::string>(_trkPath));
    Pipeline<Streamline> pipeline(&trkFile);
    int_vector indices = as<int_vector>(_indices);
    std::transform(indices.begin(), indices.end(), indices.begin(), decrement<int,int>);
    pipeline.setSubset(indices);
    
    int_vector dims(3);
    const Grid<3> &grid = trkFile.getGrid3D();
    std::copy(grid.dimensions().data(), grid.dimensions().data()+3, dims.begin());
    VisitationMapDataSink map(dims);
    pipeline.addSink(&map);
    
    pipeline.run();
    
    NiftiImage reference(as<std::string>(_imagePath), false);
    map.writeToNifti(reference, as<std::string>(_resultPath));
    
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
    MedianTrackvisDataSink medianFile(as<std::string>(_resultPath), trkFile.getGrid3D(), as<double>(_quantile));
    pipeline.addSink(&medianFile);
    
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
    StreamlineTruncator truncator(as<double>(_leftLength), as<double>(_rightLength));
    pipeline.addManipulator(&truncator);
    BasicTrackvisDataSink resultFile(as<std::string>(_resultPath), trkFile.getGrid3D());
    pipeline.addSink(&resultFile);
    
    pipeline.run();
    
    return R_NilValue;
END_RCPP
}
