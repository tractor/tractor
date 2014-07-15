#ifndef _NIFTI_IMAGE_H_
#define _NIFTI_IMAGE_H_

#include "nifti1_io.h"

#include "Array.h"

template <typename DataType> class NiftiImage
{
private:
    nifti_image *info;
    Array<DataType> *data;
    std::vector<int> dims;
    
    template <typename StorageType> DataType convertValue (StorageType value) { return static_cast<DataType>(value); }
    template <typename StorageType> void moveData ();
    void convertData ();
    
public:
    NiftiImage () {};
    NiftiImage (const std::string &fileName, const bool readData = true)
    {
        info = nifti_image_read(fileName.c_str(), static_cast<int>(readData));
        if (info != NULL)
        {
            dims.resize(info->ndim);
            for (int i=0; i<info->ndim; i++)
                dims[i] = info->dim[i+1];
            
            if (readData)
                convertData();
            else
                data = NULL;
        }
    }
    
    ~NiftiImage ()
    {
        nifti_image_free(info);
        delete data;
    }
    
    const DataType & operator[] (size_t n) const { return data->at(n); }
    
    const DataType & operator[] (const std::vector<int> &loc) const { return data->at(loc); }
    
    Array<DataType> * getData () const { return data; }
    
    const std::vector<int> & getDimensions () const { return dims; }
};

#endif
