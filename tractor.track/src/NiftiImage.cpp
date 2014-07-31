#include <RcppArmadillo.h>
#include "nifti1.h"

#include "NiftiImage.h"

// Tell the compiler that we're going to need these specialisations (otherwise
// it won't generate the relevant code and we'll get a linker error)
template class NiftiImage<short>;
template class NiftiImage<float>;

template <typename DataType>
template <typename StorageType> void NiftiImage<DataType>::moveData ()
{
    if (info == NULL)
        return;
    
    StorageType *original = static_cast<StorageType *>(info->data);
    std::vector<DataType> values(info->nvox);
    std::transform(original, original + info->nvox, values.begin(), NiftiImage<DataType>::convertValue<StorageType>);
    
    data = new Array<DataType>(dims, values);
    nifti_image_unload(info);
}

template <typename DataType>
void NiftiImage<DataType>::convertData ()
{
    if (info == NULL)
        return;
    
    switch (info->datatype)
    {
        case NIFTI_TYPE_UINT8:
        moveData<uint8_t>();
        break;
        
        case NIFTI_TYPE_INT16:
        moveData<int16_t>();
        break;
        
        case NIFTI_TYPE_INT32:
        moveData<int32_t>();
        break;
        
        case NIFTI_TYPE_FLOAT32:
        moveData<float>();
        break;
        
        case NIFTI_TYPE_FLOAT64:
        moveData<double>();
        break;
        
        case NIFTI_TYPE_INT8:
        moveData<int8_t>();
        break;
        
        case NIFTI_TYPE_UINT16:
        moveData<uint16_t>();
        break;
        
        case NIFTI_TYPE_UINT32:
        moveData<uint32_t>();
        break;
        
        case NIFTI_TYPE_INT64:
        moveData<int64_t>();
        break;
        
        case NIFTI_TYPE_UINT64:
        moveData<uint64_t>();
        break;
        
        default:
        throw new std::runtime_error("Unsupported data type (" + std::string(nifti_datatype_string(info->datatype)) + ") in file " + std::string(info->fname));
    }
}
