#include "Rcpp.h"
#include "nifti1.h"

#include "NiftiImage.h"

template <typename StorageType>
template <typename FileStorageType> void NiftiImage<StorageType>::copyData ()
{
    FileStorageType *original = static_cast<FileStorageType *>(info->data);
    std::transform(original, original + info->nvox, data, convertValue<FileStorageType>);
}

template <typename StorageType>
void NiftiImage<StorageType>::convertData ()
{
    if (info == NULL)
        return;
    
    data = new StorageType[info->nvox];
    
    switch (info->datatype)
    {
        case NIFTI_TYPE_UINT8:
        copyData<uint8_t>();
        break;
        
        case NIFTI_TYPE_INT16:
        copyData<int16_t>();
        break;
        
        case NIFTI_TYPE_INT32:
        copyData<int32_t>();
        break;
        
        case NIFTI_TYPE_FLOAT32:
        copyData<float>();
        break;
        
        case NIFTI_TYPE_FLOAT64:
        copyData<double>();
        break;
        
        case NIFTI_TYPE_INT8:
        copyData<int8_t>();
        break;
        
        case NIFTI_TYPE_UINT16:
        copyData<uint16_t>();
        break;
        
        case NIFTI_TYPE_UINT32:
        copyData<uint32_t>();
        break;
        
        case NIFTI_TYPE_INT64:
        copyData<int64_t>();
        break;
        
        case NIFTI_TYPE_UINT64:
        copyData<uint64_t>();
        break;
        
        default:
        throw new std::runtime_error("Unsupported data type (" + std::string(nifti_datatype_string(info->datatype)) + ") in file " + std::string(info->fname));
    }
}
