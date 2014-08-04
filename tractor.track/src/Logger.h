#ifndef _LOGGER_H_
#define _LOGGER_H_

#include <RcppArmadillo.h>

// Function pointer typedef for stream manipulators like "endl"
typedef std::ostream& (*streamManipulator)(std::ostream&);

class LoggerStream
{
private:
    int myLevel;
    int *outputLevel;
    
public:
    LoggerStream(int * const outputLevel, const int myLevel)
        : outputLevel(outputLevel), myLevel(myLevel) {}
    
    LoggerStream & indent ()
    {
        if (*outputLevel >= myLevel)
        {
            for (int i=0; i<myLevel; i++)
                Rcpp::Rcout << ": ";
        }
        
        return *this;
    }
    
    template<typename DataType>
    LoggerStream & operator<< (const std::vector<DataType> &arg)
    {
        if (*outputLevel >= myLevel)
        {
            Rcpp::Rcout << "(";
            for (int i=0; i<arg.size(); i++)
                Rcpp::Rcout << "\x1b[36m" << arg[i] << "\x1b[0m, ";
            Rcpp::Rcout << "\b\b)";
        }
        
        return *this;
    }
    
    template<typename DataType>
    LoggerStream & operator<< (const arma::Col<DataType> &arg)
    {
        if (*outputLevel >= myLevel)
        {
            Rcpp::Rcout << "(";
            for (int i=0; i<arg.size(); i++)
                Rcpp::Rcout << "\x1b[36m" << arg[i] << "\x1b[0m, ";
            Rcpp::Rcout << "\b\b)";
        }
        
        return *this;
    }
    
    LoggerStream & operator<< (const double &arg)
    {
        if (*outputLevel >= myLevel)
            Rcpp::Rcout << "\x1b[33m" << arg << "\x1b[0m";
        
        return *this;
    }
    
    LoggerStream & operator<< (const float &arg)
    {
        if (*outputLevel >= myLevel)
            Rcpp::Rcout << "\x1b[33m" << arg << "\x1b[0m";
        
        return *this;
    }
    
    LoggerStream & operator<< (const int &arg)
    {
        if (*outputLevel >= myLevel)
            Rcpp::Rcout << "\x1b[33m" << arg << "\x1b[0m";
        
        return *this;
    }
    
    LoggerStream & operator<< (const std::string &arg)
    {
        if (*outputLevel >= myLevel)
            Rcpp::Rcout << arg;
        
        return *this;
    }
    
//     template<typename ArgType>
//     LoggerStream & operator<< (const ArgType &arg)
//     {
//         if (*outputLevel >= myLevel)
//             Rcpp::Rcout << arg;
//
//         return *this;
//     }
    
    LoggerStream & operator<< (streamManipulator fun)
    {
        if (*outputLevel >= myLevel)
            fun(Rcpp::Rcout);
        
        return *this;
    }
};

class Logger
{
private:
    int outputLevel;
    
public:
    LoggerStream debug1, debug2, debug3;
    
    Logger()
        : debug1(&outputLevel,1), debug2(&outputLevel,2), debug3(&outputLevel,3) {}
    
    int getOutputLevel () { return outputLevel; }
    void setOutputLevel (const int outputLevel) { this->outputLevel = outputLevel; }
};

#endif
