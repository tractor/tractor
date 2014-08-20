#include <RcppArmadillo.h>
#include "nifti1.h"

#include "NiftiImage.h"

template <typename SourceType, typename TargetType>
Array<TargetType> * NiftiImage::convertToArray () const
{
    if (info == NULL)
        return NULL;
    
    SourceType *original = static_cast<SourceType *>(info->data);
    std::vector<TargetType> values(info->nvox);
    std::transform(original, original + info->nvox, values.begin(), NiftiImage::convertValue<SourceType,TargetType>);
    
    Array<TargetType> * data = new Array<TargetType>(dims, values);
    return data;
}

template <typename DataType>
Array<DataType> * NiftiImage::getData () const
{
    if (info == NULL)
        return NULL;
    
    switch (info->datatype)
    {
        case NIFTI_TYPE_UINT8:
        return convertToArray<uint8_t,DataType>();
        break;
        
        case NIFTI_TYPE_INT16:
        return convertToArray<int16_t,DataType>();
        break;
        
        case NIFTI_TYPE_INT32:
        return convertToArray<int32_t,DataType>();
        break;
        
        case NIFTI_TYPE_FLOAT32:
        return convertToArray<float,DataType>();
        break;
        
        case NIFTI_TYPE_FLOAT64:
        return convertToArray<double,DataType>();
        break;
        
        case NIFTI_TYPE_INT8:
        return convertToArray<int8_t,DataType>();
        break;
        
        case NIFTI_TYPE_UINT16:
        return convertToArray<uint16_t,DataType>();
        break;
        
        case NIFTI_TYPE_UINT32:
        return convertToArray<uint32_t,DataType>();
        break;
        
        case NIFTI_TYPE_INT64:
        return convertToArray<int64_t,DataType>();
        break;
        
        case NIFTI_TYPE_UINT64:
        return convertToArray<uint64_t,DataType>();
        break;
        
        default:
        throw std::runtime_error("Unsupported data type (" + std::string(nifti_datatype_string(info->datatype)) + ") in file " + std::string(info->fname));
    }
}

template Array<float> * NiftiImage::getData<float> () const;
template Array<short> * NiftiImage::getData<short> () const;
