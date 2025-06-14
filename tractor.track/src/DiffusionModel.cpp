#include <Rcpp.h>

#include "DiffusionModel.h"

DiffusionTensorModel::DiffusionTensorModel (const std::string &pdFile)
{
    RNifti::NiftiImage image(pdFile);
    image.reorient("LAS");
    principalDirections = new Image<ImageSpace::Vector,3>(image);
    space = principalDirections->imageSpace();
}

ImageSpace::Vector DiffusionTensorModel::sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
{
    return principalDirections->at(point, PointType::Voxel, RoundingType::Probabilistic);
}

BedpostModel::BedpostModel (const std::vector<std::string> &avfFiles, const std::vector<std::string> &thetaFiles, const std::vector<std::string> &phiFiles)
    : avfThreshold(0.05)
{
    if (avfFiles.size() == 0)
        throw std::invalid_argument("Vectors of BEDPOSTX filenames should not have length zero");
    if (avfFiles.size() != thetaFiles.size() || thetaFiles.size() != phiFiles.size())
        throw std::invalid_argument("Vectors of BEDPOSTX filenames should all have equal length");
    
    nCompartments = avfFiles.size();
    avf.resize(nCompartments);
    theta.resize(nCompartments);
    phi.resize(nCompartments);
    
    for (int i=0; i<nCompartments; i++)
    {
        RNifti::NiftiImage avfImage(avfFiles[i]);
        avf[i] = new Image<float,4>(avfImage.reorient("LAS"));
        RNifti::NiftiImage thetaImage(thetaFiles[i]);
        theta[i] = new Image<float,4>(thetaImage.reorient("LAS"));
        RNifti::NiftiImage phiImage(phiFiles[i]);
        phi[i] = new Image<float,4>(phiImage.reorient("LAS"));
    }
    
    space = avf[0]->imageSpace();
    nSamples = avf[0]->dim()[3];
}

ImageSpace::Vector BedpostModel::sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
{
    // Round the point location and convert to array index
    ImageSpace::Point roundedPoint = space->toVoxel(point, PointType::Voxel, RoundingType::Probabilistic);
    Image<float,4>::ArrayIndex loc;
    for (int i=0; i<3; i++)
        loc[i] = static_cast<size_t>(roundedPoint[i]);
    
    // Randomly choose a sample number
    loc[3] = static_cast<size_t>(round(R::unif_rand() * (nSamples-1)));
    
    // NB: Currently assuming always at least one anisotropic compartment
    ImageSpace::Vector sphericalCoordsStep(1.0);
    int closestIndex = 0;
    float highestInnerProd = -1.0;
    for (int i=0; i<nCompartments; i++)
    {
        // Check AVF is above threshold
        const float currentAvfSample = avf[i]->at(loc);
        if (i == 0 || currentAvfSample >= avfThreshold)
        {
            sphericalCoordsStep[1] = theta[i]->at(loc);
            sphericalCoordsStep[2] = phi[i]->at(loc);
            ImageSpace::Vector stepVector = ImageSpace::sphericalToCartesian(sphericalCoordsStep);
            
            // Use AVF to choose population on first step
            float innerProd;
            if (ImageSpace::norm(referenceDirection) == 0.0)
                innerProd = currentAvfSample;
            else
                innerProd = static_cast<float>(fabs(ImageSpace::dot(stepVector, referenceDirection)));
            
            // If this direction is closer to the reference direction, choose it
            if (innerProd > highestInnerProd && (sphericalCoordsStep[1] != 0.0 || sphericalCoordsStep[2] != 0.0))
            {
                highestInnerProd = innerProd;
                closestIndex = i;
            }
        }
    }
    
    sphericalCoordsStep[1] = theta[closestIndex]->at(loc);
    sphericalCoordsStep[2] = phi[closestIndex]->at(loc);
    
    if (sphericalCoordsStep[1] == 0.0 && sphericalCoordsStep[2] == 0.0)
        return ImageSpace::zeroVector();
    else
        return ImageSpace::sphericalToCartesian(sphericalCoordsStep);
}
