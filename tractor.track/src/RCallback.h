#ifndef _R_CALLBACK_H_
#define _R_CALLBACK_H_

#include <RcppEigen.h>

#include "DataSource.h"
#include "Streamline.h"

class RCallbackDataSink : public DataSink<Streamline>
{
private:
    Rcpp::Function function;
    
public:
    RCallbackDataSink (const Rcpp::Function &function)
        : function(function) {}
    
    void put (const Streamline &data);
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
