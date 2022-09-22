#ifndef _PIPELINE_H_
#define _PIPELINE_H_

#include "DataSource.h"

// Pipeline: a general blockwise processing structure
// If there are multiple manipulators then they are applied in sequence
// If there are multiple sinks then data are sent to all of them
template <class ElementType> class Pipeline
{
private:
    DataSource<ElementType> *source = nullptr;
    std::vector<DataManipulator<ElementType>*> manipulators;
    std::vector<DataSink<ElementType>*> sinks;
    
    size_t blockSize;
    std::vector<size_t> subset;
    std::list<ElementType> workingSet;
    
public:
    Pipeline (DataSource<ElementType> * const source = nullptr, const size_t blockSize = 1000)
        : source(source), blockSize(blockSize) {}
    
    ~Pipeline ()
    {
        clearSinks();
        clearManipulators();
        delete source;
    }
    
    DataSource<ElementType> * dataSource () const { return source; }
    void setBlockSize (const size_t blockSize) { this->blockSize = blockSize; }
    
    template <typename VectorElementType>
    void setSubset (const std::vector<VectorElementType> &elements)
    {
        if (elements.size() == 0)
            subset.clear();
        else
        {
            subset = std::vector<size_t>(elements.begin(), elements.end());
            std::sort(subset.begin(), subset.end());
        }
    }
    
    void setSubset (SEXP _elements)
    {
        if (Rf_isNull(_elements))
            subset.clear();
        else
        {
            Rcpp::IntegerVector elements(_elements);
            if (elements.size() == 0)
                subset.clear();
            else
            {
                subset.resize(elements.size());
                std::transform(elements.begin(), elements.end(), subset.begin(), [](int x) { return size_t(x - 1); });
                std::sort(subset.begin(), subset.end());
            }
        }
    }
    
    void addManipulator (DataManipulator<ElementType> * const manipulator)
    {
        if (manipulator != nullptr)
            manipulators.push_back(manipulator);
    }
    
    void clearManipulators ()
    {
        for (size_t i=0; i<manipulators.size(); i++)
            delete manipulators[i];
        manipulators.clear();
    }
    
    void addSink (DataSink<ElementType> * const sink)
    {
        if (sink != nullptr)
            sinks.push_back(sink);
    }
    
    void clearSinks ()
    {
        for (size_t i=0; i<sinks.size(); i++)
            delete sinks[i];
        sinks.clear();
    }
    
    size_t run ();
};

#endif
