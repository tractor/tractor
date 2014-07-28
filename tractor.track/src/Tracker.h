#ifndef _TRACKER_H_
#define _TRACKER_H_

#include "Space.h"
#include "NiftiImage.h"
#include "DiffusionDataSource.h"
#include "Streamline.h"

class Tracker
{
private:
    DiffusionDataSource *dataSource;
    NiftiImage<short> *mask;
    
    Array<short> *loopcheck;
    Array<short> *visited;
    NiftiImage<int> *visitationMap;
    
    std::map<std::string,bool> flags;
    
    Space<3>::Point seed;
    Space<3>::Vector rightwardsVector;
    bool rightwardsVectorValid;
    float stepLength;
    
public:
    Tracker () {}
    Tracker (DiffusionDataSource * const dataSource, NiftiImage<short> * const mask)
        : dataSource(dataSource), mask(mask) {}
    
    ~Tracker ()
    {
        delete mask;
        delete loopcheck;
        delete visited;
        delete visitationMap;
    }
    
    Space<3>::Point getSeed () const { return seed; }
    Space<3>::Vector getRightwardsVector () const { return rightwardsVector; }
    float getStepLength () const { return stepLength; }
    
    void setSeed (const Space<3>::Point &seed) { this->seed = seed; }
    void setRightwardsVector (const Space<3>::Vector &rightwardsVector) { this->rightwardsVector = rightwardsVector; this->rightwardsVectorValid = true; }
    void setStepLength (const float stepLength) { this->stepLength = stepLength; }
    
    void setFlag (const std::string key, const bool value = true) { this->flags[key] = value; }
    
    void setMask (NiftiImage<short> * const mask)
    {
        this->mask = mask;
        if (visited == NULL)
            
    }
    
    Streamline run ();
};

#endif
