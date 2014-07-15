#ifndef _DIFFUSION_DATA_SOURCE_H_
#define _DIFFUSION_DATA_SOURCE_H_

#include "Space.h"
#include "NiftiImage.h"

class DiffusionDataSource
{
public:
    virtual void sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection, float &thetaSample, float &phiSample)
    {
        thetaSample = 0.0;
        phiSample = 0.0;
    }
};

class BedpostDataSource : public DiffusionDataSource
{
private:
    std::vector<NiftiImage<float> *> avf, theta, phi;
    int nCompartments;
    int nSamples;
    float avfThreshold;
    
public:
    BedpostDataSource () { nCompartments = 0; }
    BedpostDataSource (const std::vector<std::string> &avfFiles, const std::vector<std::string> &thetaFiles, const std::vector<std::string> &phiFiles)
    {
        if (avfFiles.size() != thetaFiles.size() || thetaFiles.size() != phiFiles.size())
            throw new std::invalid_argument("Vectors of BEDPOSTX filenames should all have equal length");
        
        nCompartments = avfFiles.size();
        avf.resize(nCompartments);
        theta.resize(nCompartments);
        phi.resize(nCompartments);
        
        for (int i=0; i<nCompartments; i++)
        {
            avf[i] = new NiftiImage<float>(avfFiles[i]);
            theta[i] = new NiftiImage<float>(thetaFiles[i]);
            phi[i] = new NiftiImage<float>(phiFiles[i]);
        }
    }
    
    void sampleDirection (const Space<3>::Point &point, const Space<3>::Vector &referenceDirection, float &thetaSample, float &phiSample);
};

#endif
