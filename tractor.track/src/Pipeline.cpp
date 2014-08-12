#include <RcppArmadillo.h>

#include "Pipeline.h"

template <class ElementType>
void Pipeline<ElementType>::run ()
{
    // If there's no data source there's nothing to do
    if (source == NULL)
        return;
    
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
            // Apply the manipulator(s), if there are any
            for (int i=0; i<manipulators.size(); i++)
            {
                for (typename std::list<ElementType>::iterator it=workingSet.begin(); it!=workingSet.end(); it++)
                {
                    bool keep = manipulators[i]->process(*it);
                    if (!keep)
                        workingSet.erase(it);
                }
            }
            
            // Pass the remaining data to the sink(s)
            for (int i=0; i<sinks.size(); i++)
            {
                // Tell the sink how many elements are incoming
                sinks[i]->notify(workingSet.size());
                
                for (typename std::list<ElementType>::const_iterator it=workingSet.begin(); it!=workingSet.end(); it++)
                    sinks[i]->put(*it);
            }
            
            // Empty the working set again
            workingSet.clear();
        }
    }
}
