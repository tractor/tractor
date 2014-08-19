#ifndef _BINARY_STREAM_H_
#define _BINARY_STREAM_H_

#include <RcppArmadillo.h>

class BinaryStream
{
protected:
    bool swapEndian;
    
    template <typename Type> void swap (Type *value);
    
public:
    void swapEndianness (bool value) { swapEndian = value; }
};

class BinaryInputStream : public BinaryStream
{
private:
    std::ifstream *stream;
    
public:
    BinaryInputStream () {}
    BinaryInputStream (std::ifstream *stream)
        : stream(stream) {}
    
    void attach (std::ifstream *stream) { this->stream = stream; }
    
    template <typename SourceType> SourceType readValue ();
    template <typename SourceType, typename FinalType> void readValues (std::vector<FinalType> &values, size_t n);
    template <typename SourceType, typename FinalType> void readValues (arma::Col<FinalType> &values, size_t n);
    std::string readString (size_t n);
};

#endif
