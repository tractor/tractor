#include <Rcpp.h>

#include "Streamline.h"
#include "Pipeline.h"

template <class ElementType>
size_t Pipeline<ElementType>::run ()
{
    size_t total = 0, subsetIndex = 0;
    const bool usingSubset = (subset.size() > 0);
    bool subsetFinished = false;
    
    // If there's no data source there's nothing to do
    if (source == nullptr)
        return 0;
    
    // Otherwise set up the source and empty the working set
    source->setup();
    workingSet.clear();
    
    while (source->more() && !subsetFinished)
    {
        Rcpp::checkUserInterrupt();
        
        // Skip forward to the next element in the subset if necessary
        // FIXME: What if we're using a subset and the source isn't seekable?
        if (usingSubset && source->seekable())
        {
            if (subsetIndex >= subset.size())
                subsetFinished = true;
            else
                source->seek(subset[subsetIndex]);
            
            subsetIndex++;
        }
        
        // Get the next element and insert it into the working set
        // If the subset is finished we don't want any more elements, so skip this
        if (!subsetFinished)
        {
            ElementType element;
            source->get(element);
            workingSet.push_back(element);
        }
        
        // Process the data when the working set is full or there's nothing more incoming
        if (workingSet.size() == blockSize || !source->more() || subsetFinished)
        {
            total += workingSet.size();
            
            // Apply the manipulator(s), if there are any
            for (int i=0; i<manipulators.size(); i++)
            {
                manipulators[i]->setup(workingSet.size());
                auto it = workingSet.begin();
                while (it != workingSet.end())
                {
                    bool keep = manipulators[i]->process(*it);
                    if (keep)
                        it++;
                    else
                    {
                        it = workingSet.erase(it);
                        total--;
                    }
                }
            }
            
            // If the manipulators have thrown out everything, there's nothing left to do
            if (workingSet.empty())
                continue;
            
            // Pass the remaining data to the sink(s)
            for (int i=0; i<sinks.size(); i++)
            {
                // Tell the sink how many elements are incoming
                sinks[i]->setup(workingSet.size());
                
                // Pass each element to the sink
                for (auto it=workingSet.cbegin(); it!=workingSet.cend(); it++)
                    sinks[i]->put(*it);
                
                sinks[i]->finish();
            }
            
            // Empty the working set again
            workingSet.clear();
        }
    }
    
    for (int i=0; i<sinks.size(); i++)
        sinks[i]->done();
    source->done();
    
    return total;
}

template class Pipeline<Streamline>;
