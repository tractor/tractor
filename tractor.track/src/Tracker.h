#ifndef _TRACKER_H_
#define _TRACKER_H_

#include <RcppArmadillo.h>

#include "Space.h"
#include "NiftiImage.h"
#include "DiffusionModel.h"
#include "Streamline.h"
#include "DataSource.h"
#include "Logger.h"

#define LOOPCHECK_RATIO 5.0

class Tracker
{
private:
    DiffusionModel *dataSource;
    
    Array<short> *maskData;
    std::vector<int> spaceDims;
    std::vector<float> voxelDims;
    
    Array<Space<3>::Vector> *loopcheck;
    Array<bool> *visited;
    
    std::map<std::string,bool> flags;
    
    Space<3>::Point seed;
    Space<3>::Vector rightwardsVector;
    float innerProductThreshold;
    float stepLength;
    int maxSteps;
    bool jitter;
    
    Logger logger;
    
public:
    Tracker () {}
    Tracker (DiffusionModel * const dataSource)
        : dataSource(dataSource), maskData(NULL), loopcheck(NULL), visited(NULL) {}
    
    ~Tracker ()
    {
        delete maskData;
        delete loopcheck;
        delete visited;
    }
    
    Space<3>::Point getSeed () const { return seed; }
    Space<3>::Vector getRightwardsVector () const { return rightwardsVector; }
    float getInnerProductThreshold () const { return innerProductThreshold; }
    float getStepLength () const { return stepLength; }
    
    void setMask (NiftiImage * const mask)
    {
        delete maskData;
        maskData = mask->getData<short>();
        spaceDims = mask->getDimensions();
        voxelDims = mask->getVoxelDimensions();
    }
    
    void setSeed (const Space<3>::Point &seed, const bool jitter)
    {
        // An existing rightwards vector needs to be discarded when a new seed is used
        this->seed = seed;
        this->rightwardsVector = Space<3>::zeroVector();
        this->jitter = jitter;
    }
    
    void setRightwardsVector (const Space<3>::Vector &rightwardsVector) { this->rightwardsVector = rightwardsVector; }
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
    arma::fmat seeds;
    bool jitter;
    size_t streamlinesPerSeed, totalStreamlines, currentStreamline, currentSeed;
    
public:
    TractographyDataSource (Tracker * const tracker, const arma::fmat &seeds, const size_t streamlinesPerSeed, const bool jitter)
        : tracker(tracker), seeds(seeds), streamlinesPerSeed(streamlinesPerSeed), jitter(jitter), currentStreamline(0), currentSeed(0)
    {
        this->totalStreamlines = seeds.n_rows * streamlinesPerSeed;
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
            
            // Vectors are columns to Armadillo, so we have to transpose
            tracker->setSeed(seeds.row(currentSeed).t(), jitter);
        }
        
        // Generate the streamline
        data = tracker->run();
        
        // Increment the main counter
        currentStreamline++;
    }
};

#endif
