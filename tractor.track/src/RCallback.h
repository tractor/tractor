#ifndef _R_CALLBACK_H_
#define _R_CALLBACK_H_

#include <RcppArmadillo.h>

#include "DataSource.h"
#include "Streamline.h"

class RCallbackDataSink : public DataSink<Streamline>
{
private:
    Rcpp::Function function;
    arma::fmat points;
    arma::uvec startIndices, seedIndices;
    size_t currentIndex, currentStart, nTotalPoints;
    
public:
    RCallbackDataSink (const Rcpp::Function &function)
        : function(function) {}
    
    void setup (const size_type &count, const_iterator begin, const_iterator end);
    void put (const Streamline &data);
    void finish ();
};

#endif
