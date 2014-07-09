#ifndef _NIFTI_IMAGE_H_
#define _NIFTI_IMAGE_H_

#include "nifti1_io.h"

#include "Array.h"

template <typename DataType> class NiftiImage
{
private:
    nifti_image *info;
    Array<DataType> *data;
    
    template <typename StorageType> DataType convertValue (StorageType value) { return static_cast<DataType>(value); }
    template <typename StorageType> void copyData ();
    void convertData ();
    
public:
    NiftiImage () {};
    NiftiImage (const std::string &fileName, const bool readData = true)
    {
        info = nifti_image_read(fileName.c_str(), static_cast<int>(readData));
        
        if (readData)
            convertData();
        else
            data = NULL;
    }
    
    ~NiftiImage ()
    {
        nifti_image_free(info);
        delete data;
    }
};

#endif
