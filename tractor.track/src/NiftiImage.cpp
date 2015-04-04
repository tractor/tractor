#include <RcppArmadillo.h>
#include "nifti1.h"

#include "NiftiImage.h"

void NiftiImage::extractMetadata ()
{
    dims.resize(info->ndim);
    voxelDims.resize(info->ndim);
    for (int i=0; i<info->ndim; i++)
    {
        dims[i] = info->dim[i+1];
        voxelDims[i] = fabs(info->pixdim[i+1]);
    }
    
    if (info->qform_code > 0)
        xform = arma::fmat44(*(info->qto_xyz.m)).t();
    else
        xform = arma::fmat44(*(info->sto_xyz.m)).t();
}

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

template <typename DataType> int NiftiImage::chooseDatatype (const Array<DataType> &data)
{
    // This function assumes that cal_min and cal_max have already been set correctly
    if (info->cal_min >= std::numeric_limits<uint8_t>::min() && info->cal_max <= std::numeric_limits<uint8_t>::max())
        return NIFTI_TYPE_UINT8;
    else if (info->cal_min >= std::numeric_limits<int16_t>::min() && info->cal_max <= std::numeric_limits<int16_t>::max())
        return NIFTI_TYPE_INT16;
    else if (info->cal_min >= std::numeric_limits<int32_t>::min() && info->cal_max <= std::numeric_limits<int32_t>::max())
        return NIFTI_TYPE_INT32;
    else
        throw std::runtime_error("No supported data type is appropriate for the specified array");
}

template <> int NiftiImage::chooseDatatype (const Array<float> &data)
{
    return NIFTI_TYPE_FLOAT32;
}

template <> int NiftiImage::chooseDatatype (const Array<double> &data)
{
    return NIFTI_TYPE_FLOAT64;
}

template <typename SourceType, typename TargetType>
void NiftiImage::convertFromArray (const Array<SourceType> &data)
{
    // Free existing memory
    nifti_image_unload(info);
    
    // Allocate new memory
    info->data = (void *) calloc(info->nvox, sizeof(TargetType));
    
    TargetType *final = static_cast<TargetType *>(info->data);
    std::transform(data.begin(), data.end(), final, NiftiImage::convertValue<SourceType,TargetType>);
}

template <typename DataType> void NiftiImage::setData (const Array<DataType> &data)
{
    if (!std::equal(dims.begin(), dims.end(), data.getDimensions().begin()))
        throw std::runtime_error("Data and metadata dimensions do not match");
    
    // Set slope, intercept, max and min
    info->scl_slope = 1.0;
    info->scl_inter = 0.0;
    info->cal_min = static_cast<float>(*std::min_element(data.begin(), data.end()));
    info->cal_max = static_cast<float>(*std::max_element(data.begin(), data.end()));
    
    int niftiDatatype = chooseDatatype(data);
    
    switch(niftiDatatype)
    {
        case NIFTI_TYPE_UINT8:
        convertFromArray<DataType,uint8_t>(data);
        break;
        
        case NIFTI_TYPE_INT16:
        convertFromArray<DataType,int16_t>(data);
        break;
        
        case NIFTI_TYPE_INT32:
        convertFromArray<DataType,int32_t>(data);
        break;
        
        case NIFTI_TYPE_FLOAT32:
        convertFromArray<DataType,float>(data);
        break;
        
        case NIFTI_TYPE_FLOAT64:
        convertFromArray<DataType,double>(data);
        break;
    }
    
    int bytesPerVoxel, swapSize;
    nifti_datatype_sizes(niftiDatatype, &bytesPerVoxel, &swapSize);
    info->datatype = niftiDatatype;
    info->nbyper = bytesPerVoxel;
    info->swapsize = swapSize;
}

template Array<float> * NiftiImage::getData<float> () const;
template Array<short> * NiftiImage::getData<short> () const;

template void NiftiImage::setData<double> (Array<double> const &);
