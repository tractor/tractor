#include <Rcpp.h>

#include "Image.h"

ImageSpace::Point ImageSpace::toVoxel (const Point &point, const PointType type, const RoundingType round) const
{
    Point result;
    
    switch (type)
    {
        case PointType::Voxel:
        result = point;
        break;
        
        case PointType::Scaled:
        for (int i=0; i<3; i++)
            result[i] = point[i] / fabs(pixdim[i]);
        break;
        
        case PointType::World:
        RNifti::NiftiImage::Xform::Vector4 padded(1.0);
        for (int i=0; i<3; i++)
            padded[i] = point[i];
        const RNifti::NiftiImage::Xform::Vector4 product = transform.multiply(padded);
        for (int i=0; i<3; i++)
            result[i] = product[i];
        break;
    }
    
    switch (round)
    {
        case RoundingType::None:
        break;
        
        case RoundingType::Conventional:
        for (int i=0; i<3; i++)
            result[i] = std::round(result[i]);
        break;
        
        case RoundingType::Probabilistic:
        for (int i=0; i<3; i++)
        {
            const Element ceiling = std::ceil(result[i]);
            const Element floor = std::floor(result[i]);
            const Element distance = result[i] - floor;
            
            // Sample in proportion to proximity, unless we're off the end of the image
            const Element uniformSample = static_cast<Element>(R::unif_rand());
            const bool chooseFloor = (uniformSample > distance && floor >= 0.0) || ceiling >= static_cast<Element>(dim[i]);
            result[i] = chooseFloor ? floor : ceiling;
        }
        break;
    }
    
    return result;
}
