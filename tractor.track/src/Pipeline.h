#ifndef _PIPELINE_H_
#define _PIPELINE_H_

#include "DataSource.h"

// Pipeline: a general blockwise processing structure
// If there are multiple manipulators then they are applied in sequence
// If there are multiple sinks then data are sent to all of them
template <class ElementType> class Pipeline
{
private:
    DataSource<ElementType> *source;
    std::vector<DataManipulator<ElementType>*> manipulators;
    std::vector<DataSink<ElementType>*> sinks;
    
    size_t blockSize;
    std::list<ElementType> workingSet;
    
    // Hide default constructor
    Pipeline () {}
    
public:
    Pipeline (DataSource<ElementType> * const source = NULL, const size_t blockSize = 1000)
        : source(source), blockSize(blockSize) {}
    
    void setBlockSize (const size_t blockSize) { this->blockSize = blockSize; }
    void setSource (DataSource<ElementType> * const source) { this->source = source; }
    void addManipulator (DataManipulator<ElementType> * const manipulator) { manipulators.push_back(manipulator); }
    void addSink (DataSink<ElementType> * const sink) { sinks.push_back(sink); }
    
    size_t run ();
};

#endif
