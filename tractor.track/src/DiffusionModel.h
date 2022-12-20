#ifndef _DIFFUSION_MODEL_H_
#define _DIFFUSION_MODEL_H_

#include "Image.h"

class DiffusionModel : public ImageSpaceEmbedded
{
public:
    virtual ~DiffusionModel () {}
    
    virtual ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
    {
        return ImageSpace::zeroVector();
    }
};

class DiffusionTensorModel : public DiffusionModel
{
private:
    Image<ImageSpace::Vector,3> *principalDirections;
    
public:
    DiffusionTensorModel ()
        : principalDirections(NULL) {}
    
    DiffusionTensorModel (const std::string &pdFile);
    
    ~DiffusionTensorModel ()
    {
        delete principalDirections;
    }
    
    ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const override;
};

class BedpostModel : public DiffusionModel
{
private:
    std::vector<Image<float,4>*> avf, theta, phi;
    int nCompartments = 0;
    int nSamples = 0;
    float avfThreshold = 0.0;
    
public:
    BedpostModel () {}
    
    BedpostModel (const std::vector<std::string> &avfFiles, const std::vector<std::string> &thetaFiles, const std::vector<std::string> &phiFiles);
    
    ~BedpostModel ()
    {
        for (int i=0; i<nCompartments; i++)
        {
            delete avf[i];
            delete theta[i];
            delete phi[i];
        }
    }
    
    int getNCompartments () const { return nCompartments; }
    int getNSamples () const { return nSamples; }
    float getAvfThreshold () const { return avfThreshold; }
    
    void setAvfThreshold (const float avfThreshold) { this->avfThreshold = avfThreshold; }
    
    ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const override;
};

#endif
