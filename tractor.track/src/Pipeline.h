#ifndef _PIPELINE_H_
#define _PIPELINE_H_

// Data source: responsible for reading or generating data elements
template <class ElementType> class DataSource
{    
public:
    virtual bool more () const { return false; }
    virtual void get (ElementType &data) const {}
};

// Data sink: responsible for exporting or writing data elements
// The Blockwise template parameter determines whether "put" is by element or block
template <class ElementType, bool Blockwise> class DataSink
{
public:
    bool blockwise () const { return Blockwise; }
    virtual void put (const ElementType &data) {}
    virtual void put (const std::list<ElementType> &data) {}
};

// Data manipulator: responsible for transforming or removing data elements
template <class ElementType> class DataManipulator
{
public:
    // If the return value is false, the element will be removed
    virtual bool process (ElementType &data) { return true; }
};

// Pipeline: a general blockwise processing structure
template <class ElementType> class Pipeline
{
private:
    DataSource<ElementType> *source;
    DataSink<ElementType> *sink;
    DataManipulator<ElementType> *manipulator;
    
    size_t blockSize;
    std::list<ElementType> workingSet;
    
public:
    Pipeline ()
        : source(NULL), sink(NULL), manipulator(NULL), blockSize(10000) {}
    
    void setBlockSize (const size_t blockSize) { this->blockSize = blockSize; }
    void setSource (DataSource<ElementType> const *source) { this->source = source; }
    void setSink (DataSink<ElementType> const *sink) { this->sink = sink; }
    void setManipulator (DataManipulator<ElementType> const *manipulator) { this->manipulator = manipulator; }
    
    void run ();
};

#endif
