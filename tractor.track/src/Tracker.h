#ifndef _TRACKER_H_
#define _TRACKER_H_

#include <Rcpp.h>

#include "Image.h"
#include "DiffusionModel.h"
#include "Streamline.h"
#include "DataSource.h"
#include "Logger.h"

#define LOOPCHECK_RATIO 5.0

class Tracker
{
private:
    DiffusionModel *model = nullptr;
    
    Image<short,3> *maskData = nullptr;
    Image<int,3> *targetData = nullptr;
    std::map<int,std::string> dictionary;
    
    Image<ImageSpace::Vector,3> *loopcheck = nullptr;
    Image<bool,3> *visited = nullptr;
    
    std::map<std::string,bool> flags;
    
    ImageSpace::Point seed;
    ImageSpace::Vector rightwardsVector;
    float innerProductThreshold;
    float stepLength;
    int maxSteps;
    bool jitter;
    bool autoResetRightwardsVector;
    
    Logger logger;
    
public:
    Tracker () {}
    Tracker (DiffusionModel * const model)
        : model(model) {}
    
    ~Tracker ()
    {
        delete maskData;
        delete targetData;
        delete loopcheck;
        delete visited;
    }
    
    DiffusionModel * getModel () const { return model; }
    ImageSpace::Point getSeed () const { return seed; }
    ImageSpace::Vector getRightwardsVector () const { return rightwardsVector; }
    float getInnerProductThreshold () const { return innerProductThreshold; }
    float getStepLength () const { return stepLength; }
    
    void setMask (const RNifti::NiftiImage &mask)
    {
        delete maskData;
        maskData = new Image<short,3>(mask);
    }
    
    void setSeed (const ImageSpace::Point &seed, const bool jitter)
    {
        // An existing rightwards vector needs to be discarded when a new seed is used
        this->seed = seed;
        if (autoResetRightwardsVector)
            this->rightwardsVector = ImageSpace::zeroVector();
        this->jitter = jitter;
    }
    
    void setTargets (const RNifti::NiftiImage &targets)
    {
        delete targetData;
        targetData = new Image<int,3>(targets);
    }
    
    std::map<int,std::string> & labelDictionary () { return dictionary; }
    
    void setRightwardsVector (const ImageSpace::Vector &rightwardsVector)
    {
        // If the specified rightwards vector is nontrivial, don't clobber it when setting the seed
        this->rightwardsVector = rightwardsVector;
        this->autoResetRightwardsVector = (ImageSpace::norm(rightwardsVector) == 0.0);
    }
    
    void setInnerProductThreshold (const float innerProductThreshold) { this->innerProductThreshold = innerProductThreshold; }
    void setStepLength (const float stepLength) { this->stepLength = stepLength; }
    void setMaxSteps (const int maxSteps) { this->maxSteps = maxSteps; }
    
    void setFlag (const std::string &key, const bool value = true) { this->flags[key] = value; }
    void setFlags (const std::map<std::string,bool> &flags) { this->flags = flags; }
    
    void setDebugLevel (const int &level) { this->logger.setOutputLevel(level); }
    
    Streamline run ();
};

class TractographyDataSource : public DataSource<Streamline>
{
private:
    Tracker *tracker;
    std::vector<ImageSpace::Point> seeds;
    bool jitter;
    size_t streamlinesPerSeed, totalStreamlines, currentStreamline, currentSeed;
    
public:
    TractographyDataSource (Tracker * const tracker, const std::vector<ImageSpace::Point> &seeds, const size_t streamlinesPerSeed, const bool jitter)
        : tracker(tracker), seeds(seeds), streamlinesPerSeed(streamlinesPerSeed), jitter(jitter), currentStreamline(0), currentSeed(0)
    {
        this->totalStreamlines = seeds.size() * streamlinesPerSeed;
    }
    
    bool more () { return (currentStreamline < totalStreamlines); }
    
    void get (Streamline &data)
    {
        // We're not generating any more streamlines
        if (currentStreamline >= totalStreamlines)
            return;
        
        // We're moving on to the next seed
        if (currentStreamline % streamlinesPerSeed == 0)
        {
            if (currentStreamline > 0)
                currentSeed++;
            
            tracker->setSeed(seeds[currentSeed], jitter);
        }
        
        // Generate the streamline
        data = tracker->run();
        
        // Increment the main counter
        currentStreamline++;
    }
};

#endif
