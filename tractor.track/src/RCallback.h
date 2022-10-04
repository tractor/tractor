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
    
    void setup (const size_t &count)
    {
        list = Rcpp::List(count);
        currentStreamline = 0;
    }
    
    void put (const Streamline &data);
    
    Rcpp::List getList () const { return list; }
};

class LabelProfileDataSink : public DataSink<Streamline>
{
private:
    std::map<int,size_t> counts;
    std::map<int,std::string> dictionary;
    Rcpp::IntegerVector profile;
    
public:
    std::map<int,std::string> & labelDictionary () { return dictionary; }
    
    void put (const Streamline &data);
    void done ();
    
    Rcpp::IntegerVector getProfile () const { return profile; }
};

#endif
