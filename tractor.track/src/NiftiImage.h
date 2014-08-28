#ifndef _NIFTI_IMAGE_H_
#define _NIFTI_IMAGE_H_

#include <RcppArmadillo.h>

#include "nifti1_io.h"

#include "Array.h"

class NiftiImage
{
private:
    nifti_image *info;
    std::vector<int> dims;
    std::vector<float> voxelDims;
    arma::fmat44 xform;
    
    void extractMetadata ();
    
    template <typename DataType> int chooseDatatype (const Array<DataType> &data);
    
    template <typename SourceType, typename TargetType> static TargetType convertValue (SourceType value) { return static_cast<TargetType>(value); }
    template <typename SourceType, typename TargetType> void convertFromArray (const Array<SourceType> &data);
    template <typename SourceType, typename TargetType> Array<TargetType> * convertToArray () const;
    
public:
    NiftiImage () {}
    NiftiImage (const std::string &fileName, const bool readData = true)
    {
        info = nifti_image_read(fileName.c_str(), static_cast<int>(readData));
        if (info == NULL)
            throw std::runtime_error("Cannot read NIfTI file " + fileName);
        else
            extractMetadata();
    }
    NiftiImage (const NiftiImage &source)
    {
        info = nifti_copy_nim_info(source.info);
        extractMetadata();
    }
    
    ~NiftiImage ()
    {
        nifti_image_free(info);
    }
    
    int getDimensionality () const { return dims.size(); }
    const std::vector<int> & getDimensions () const { return dims; }
    const std::vector<float> & getVoxelDimensions () const { return voxelDims; }
    const arma::fmat44 & getXformMatrix () const { return xform; }
    const ::mat44 & getXformStruct () const
    {
        if (info->qform_code > 0)
            return info->qto_xyz;
        else
            return info->sto_xyz;
    }
    
    template <typename DataType> Array<DataType> * getData () const;
    template <typename DataType> void setData (const Array<DataType> &data);
    
    void dropData () { nifti_image_unload(info); }
    
    void writeToFile (const std::string &fileName)
    {
        nifti_set_filenames(info, fileName.c_str(), 0, 0);
        nifti_image_write(info);
    }
};

#endif
