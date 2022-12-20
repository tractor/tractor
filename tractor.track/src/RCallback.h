#ifndef _R_CALLBACK_H_
#define _R_CALLBACK_H_

#include <Rcpp.h>

#include "DataSource.h"
#include "Streamline.h"

class RListDataSource : public DataSource<Streamline>, public ImageSpaceEmbedded
{
private:
    Rcpp::List list;
    size_t currentStreamline, totalStreamlines;
    
    Rcpp::Reference getElement (const size_t n);
    
public:
    RListDataSource (SEXP list);
    
    std::string type () const override { return "list"; }
    
    void setup () override { currentStreamline = 0; }
    size_t count () override { return totalStreamlines; }
    bool more () override { return currentStreamline < totalStreamlines; }
    void get (Streamline &data) override;
    void seek (const size_t n) override { currentStreamline = n; }
    bool seekable () override { return true; }
};

class RListDataSink : public DataSink<Streamline>
{
private:
    SEXP constructor;
    Rcpp::List list;
    size_t currentStreamline = 0, totalStreamlines = 0;

public:
    RListDataSink (SEXP constructor)
        : constructor(constructor) {}
    
    void setup (const size_t &count) override;
    void put (const Streamline &data) override;
    
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
    
    void put (const Streamline &data) override;
    void done () override;
    
    Rcpp::IntegerVector getProfile () const { return profile; }
};

#endif
