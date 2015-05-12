#include <RcppArmadillo.h>

#include "Streamline.h"
#include "Pipeline.h"

template <class ElementType>
size_t Pipeline<ElementType>::run ()
{
    // If there's no data source there's nothing to do
    if (source == NULL)
        return 0;
    
    size_t total = 0;
    
    // Empty the working set
    workingSet.clear();
    
    while (source->more())
    {
        // Get the next element and insert it into the working set
        ElementType element;
        source->get(element);
        workingSet.push_back(element);
        
        // Process the data when the working set is full or there's nothing more incoming
        if (workingSet.size() == blockSize || !source->more())
        {
            total += workingSet.size();
            
            // Apply the manipulator(s), if there are any
            for (int i=0; i<manipulators.size(); i++)
            {
                typename std::list<ElementType>::iterator it = workingSet.begin();
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
            
            // Pass the remaining data to the sink(s)
            for (int i=0; i<sinks.size(); i++)
            {
                // Tell the sink how many elements are incoming and provide an iterator
                sinks[i]->setup(workingSet.size(), workingSet.begin(), workingSet.end());
                
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
    
    return total;
}

template class Pipeline<Streamline>;
