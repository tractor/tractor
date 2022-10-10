#ifndef _DATA_SOURCE_H_
#define _DATA_SOURCE_H_

// Data source: responsible for reading or generating data elements
template <class ElementType> class DataSource
{
public:
    virtual ~DataSource () {}
    
    // This is a bit of a violation of polymorphic principles, and reliance on
    // it to identify the subclass at hand should be kept to a minimum
    virtual std::string type () const { return "unknown"; }
    
    // The setup() function is called before the pipeline starts (or to request
    // the source to reset), count() should return the total number of elements
    // available from the source (or 0 if this is unknown), more() is used to
    // check if more elements are available, get() retrieves an element, seek()
    // moves to the nth element if seekable() returns true to say seeking is
    // allowed, and done() is called after the pipeline finishes
    virtual void setup () {}
    virtual size_t count () { return 0; }
    virtual bool more () { return false; }
    virtual void get (ElementType &data) {}
    virtual void seek (const size_t n) {}
    virtual bool seekable () { return false; }
    virtual void done () {}
};

// Data sink: responsible for exporting or writing data elements
template <class ElementType> class DataSink
{
public:
    virtual ~DataSink () {}
    
    // The setup() function is called at the start of each block, put() is
    // called once per element, finish() is called after each block, and done()
    // is called after all blocks are finished
    virtual void setup (const size_t &count) {}
    virtual void put (const ElementType &data) {}
    virtual void finish () {}
    virtual void done () {}
};

// Data manipulator: responsible for transforming or removing data elements
template <class ElementType> class DataManipulator
{
public:
    virtual ~DataManipulator () {}
    
    // If the return value is false, the element will be removed
    virtual void setup (const size_t &count) {}
    virtual bool process (ElementType &data) { return true; }
};

#endif
