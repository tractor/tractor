#include <RcppEigen.h>

#include "Streamline.h"
#include "Pipeline.h"

template <class ElementType>
std::vector<size_t> Pipeline<ElementType>::run ()
{
    size_t counter = 0;
    std::vector<size_t> keepList;
    
    // If there's no data source there's nothing to do
    if (source == NULL)
        return keepList;
    
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
            counter += workingSet.size();
            
            // Apply the manipulator(s), if there are any
            for (int i=0; i<manipulators.size(); i++)
            {
                // Go back to the start of the working set
                counter -= workingSet.size();
                
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
                            keepList.push_back(counter);
                    }
                    else
                        it = workingSet.erase(it);
                    
                    // Increment the counter again
                    counter++;
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
