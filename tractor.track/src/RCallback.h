#ifndef _R_CALLBACK_H_
#define _R_CALLBACK_H_

#include <Rcpp.h>

#include "DataSource.h"
#include "Streamline.h"

class RListDataSink : public DataSink<Streamline>
{
private:
    SEXP constructor;
    Rcpp::List list;
    size_t currentStreamline;

public:
    RListDataSink (SEXP constructor)
        : constructor(constructor) {}
    
    void setup (const size_type &count, const_iterator begin, const_iterator end)
    {
        list = Rcpp::List(count);
        currentStreamline = 0;
    }
    
    void put (const Streamline &data);
    
    Rcpp::List getList () const { return list; }
};

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
