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
        
        // When the working set is full, process it
        if (workingSet.size() == blockSize)
        {
            // Apply the manipulator if there is one
            if (manipulator != NULL)
            {
                for (std::list<ElementType>::iterator it=workingSet.begin(); it!=workingSet.end(); it++)
                {
                    bool keep = manipulator->process(*it);
                    if (!keep)
                        workingSet.erase(it);
                }
            }
            
            // Pass the remaining data to the sink
            if (sink != NULL)
            {
                if (sink->blockwise())
                    sink->put(workingSet);
                else
                {
                    for (std::list<ElementType>::const_iterator it=workingSet.begin(); it!=workingSet.end(); it++)
                        sink->put(*it);
                }
            }
            
            // Empty the working set again
            workingSet.clear();
        }
    }
}
