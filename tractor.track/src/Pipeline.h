#ifndef _PIPELINE_H_
#define _PIPELINE_H_

// Data source: responsible for reading or generating data elements
template <class ElementType> class DataSource
{    
public:
    virtual bool more () { return false; }
    virtual void get (ElementType &data) {}
};

// Data sink: responsible for exporting or writing data elements
template <class ElementType> class DataSink
{
protected:
    typedef typename std::list<ElementType>::size_type size_type;
    typedef typename std::list<ElementType>::const_iterator const_iterator;
    
public:
    virtual void setup (const size_type &count, const_iterator begin, const_iterator end) {}
    virtual void put (const ElementType &data) {}
};

// Data manipulator: responsible for transforming or removing data elements
template <class ElementType> class DataManipulator
{
public:
    // If the return value is false, the element will be removed
    virtual bool process (ElementType &data) { return true; }
};

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
    Pipeline (DataSource<ElementType> * const source = NULL, const size_t blockSize = 10000)
        : source(source), blockSize(blockSize) {}
    
    void setBlockSize (const size_t blockSize) { this->blockSize = blockSize; }
    void setSource (DataSource<ElementType> * const source) { this->source = source; }
    void addManipulator (DataManipulator<ElementType> * const manipulator) { manipulators.push_back(manipulator); }
    void addSink (DataSink<ElementType> * const sink) { sinks.push_back(sink); }
    
    void run ();
};

#endif
