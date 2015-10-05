#ifndef _R_CALLBACK_H_
#define _R_CALLBACK_H_

#include <RcppEigen.h>

#include "DataSource.h"
#include "Streamline.h"

class RCallbackDataSink : public DataSink<Streamline>
{
private:
    Rcpp::Function function;
    Eigen::ArrayX3f points;
    Eigen::Array<unsigned int,Eigen::Dynamic,1> startIndices, seedIndices;
    size_t currentIndex, currentStart, nTotalPoints;
    
public:
    RCallbackDataSink (const Rcpp::Function &function)
        : function(function) {}
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void finish ();
};

class ProfileMatrixDataSink : public DataSink<Streamline>
{
private:
    Rcpp::Function function;
    std::map<int,size_t> counts;
    
public:
    ProfileMatrixDataSink (const Rcpp::Function &function)
        : function(function) {}
    
    void put (const Streamline &data);
    void done ();
};

#endif
