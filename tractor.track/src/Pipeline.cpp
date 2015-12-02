#include <RcppEigen.h>

#include "Streamline.h"
#include "Pipeline.h"

template <class ElementType>
std::vector<size_t> Pipeline<ElementType>::run ()
{
    size_t fastCounter = 0, slowCounter = 0, subsetIndex = 0;
    const bool usingSubset = (subset.size() > 0);
    bool subsetFinished = false;
    std::vector<size_t> keepList;
    
    // If there's no data source there's nothing to do
    if (source == NULL)
        return keepList;
    
    // Empty the working set
    workingSet.clear();
    
    while (source->more() && !subsetFinished)
    {
        // Don't bother even reading the element if the index is not in the subset
        if (usingSubset)
        {
            if (subsetIndex >= subset.size())
                subsetFinished = true;
            else if (fastCounter == subset[subsetIndex])
            {
                fastCounter++;
                subsetIndex++;
            }
            else
            {
                fastCounter++;
                source->skip();
                continue;
            }
        }
        
        // Get the next element and insert it into the working set
        ElementType element;
        source->get(element);
        workingSet.push_back(element);
        
        // Process the data when the working set is full or there's nothing more incoming
        if (workingSet.size() == blockSize || !source->more() || subsetFinished)
        {
            slowCounter += workingSet.size();
            
            // Apply the manipulator(s), if there are any
            for (int i=0; i<manipulators.size(); i++)
            {
                // Go back to the start of the working set
                slowCounter -= workingSet.size();
                
                typename std::list<ElementType>::iterator it = workingSet.begin();
                while (it != workingSet.end())
                {
                    bool keep = manipulators[i]->process(*it);
                    if (keep)
                    {
                        // Move on to the next element
                        it++;
                        
                        // If this is the last manipulator, add the index to the keep list
                        if (i == (manipulators.size() - 1))
                            keepList.push_back(slowCounter);
                    }
                    else
                        it = workingSet.erase(it);
                    
                    // Increment the slowCounter again
                    slowCounter++;
                }
            }
            
            // If the manipulators have thrown out everything, there's nothing left to do
            if (workingSet.empty())
                continue;
            
            // Pass the remaining data to the sink(s)
            for (int i=0; i<sinks.size(); i++)
            {
                // Tell the sink how many elements are incoming and provide an iterator
                sinks[i]->setup(workingSet.size(), workingSet.begin(), workingSet.end());
                
                // Pass each element to the sink
                for (typename std::list<ElementType>::const_iterator it=workingSet.begin(); it!=workingSet.end(); it++)
                    sinks[i]->put(*it);
                
                sinks[i]->finish();
            }
            
            // Empty the working set again
            workingSet.clear();
        }
    }
    
    for (int i=0; i<sinks.size(); i++)
        sinks[i]->done();
    
    return keepList;
}

template class Pipeline<Streamline>;
