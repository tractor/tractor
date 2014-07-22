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
    
    NiftiImage<int> *visitationMap;
    
    Space<3>::Point seed;
    Space<3>::Vector rightwardsVector;
    float stepLength;
    
public:
    Tracker () {}
    Tracker (DiffusionDataSource * const dataSource)
        : dataSource(dataSource) {}
    
    ~Tracker ()
    {
        delete mask;
        delete visitationMap;
    }
    
    Space<3>::Point getSeed () const { return seed; }
    Space<3>::Vector getRightwardsVector () const { return rightwardsVector; }
    float getStepLength () const { return stepLength; }
    
    void setSeed (const Space<3>::Point &seed) { this->seed = seed; }
    void setRightwardsVector (const Space<3>::Vector &rightwardsVector) { this->rightwardsVector = rightwardsVector; }
    void setStepLength (const float stepLength) { this->stepLength = stepLength; }
    
    Streamline run ();
};

#endif
