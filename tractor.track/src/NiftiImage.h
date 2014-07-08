#ifndef _NIFTI_IMAGE_H_
#define _NIFTI_IMAGE_H_

#include "nifti1_io.h"

template <typename StorageType> class NiftiImage
{
private:
    nifti_image *info;
    StorageType *data;
    
    template <typename FileStorageType> StorageType convertValue (FileStorageType value) { return static_cast<StorageType>(value); }
    template <typename FileStorageType> void copyData ();
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
