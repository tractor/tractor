#ifndef _DIFFUSION_MODEL_H_
#define _DIFFUSION_MODEL_H_

#include "Image.h"

class DiffusionModel : public Griddable3D
{
protected:
    ImageSpace grid;
    
    std::vector<int> probabilisticRound (const ImageSpace::Point &point, const int size = 3) const;
    
public:
    virtual ~DiffusionModel () {}
    
    virtual ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const
    {
        return ImageSpace::zeroVector();
    }
    
    ImageSpace getGrid3D () const { return grid; }
};

class DiffusionTensorModel : public DiffusionModel
{
private:
    Image<float> *principalDirections;
    
public:
    DiffusionTensorModel ()
        : principalDirections(NULL) {}
    
    DiffusionTensorModel (const std::string &pdFile);
    
    ~DiffusionTensorModel ()
    {
        delete principalDirections;
    }
    
    ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const;
};

class BedpostModel : public DiffusionModel
{
private:
    std::vector<Image<float>*> avf, theta, phi;
    int nCompartments;
    int nSamples;
    float avfThreshold;
    
public:
    BedpostModel ()
        : nCompartments(0), nSamples(0) {}
    
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
    
    ImageSpace::Vector sampleDirection (const ImageSpace::Point &point, const ImageSpace::Vector &referenceDirection) const;
};

#endif
