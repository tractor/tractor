#include <RcppArmadillo.h>

#include "BinaryStream.h"

template <typename Type>
void BinaryStream::swap (Type *pointer)
{
    int size = sizeof(Type);
    if (size == 1)
        return;
    
    char *start, *end;
    char temp;
    
    start = (char *) pointer;
    end = start + size - 1;
    
    // NB: This assumes that "size" is always even
    for (int i=0; i<(size/2); i++)
    {
        temp = *start;
        *start = *end;
        *end = temp;
        start++;
        end--;
    }
}

template <typename SourceType>
SourceType BinaryInputStream::readValue ()
{
    SourceType value;
    stream->read((char *) &value, sizeof(SourceType));
    if (swapEndian)
        swap(&value);
    return value;
}

template <typename SourceType, typename FinalType>
void BinaryInputStream::readValues (std::vector<FinalType> &values, size_t n)
{
    values.resize(n);
    SourceType value;
    for (size_t i=0; i<n; i++)
    {
        stream->read((char *) &value, sizeof(SourceType));
        if (swapEndian)
            swap(&value);
        values[i] = static_cast<FinalType>(value);
    }
}

template <typename SourceType, typename FinalType>
void BinaryInputStream::readValues (arma::Col<FinalType> &values, size_t n)
{
    values.set_size(n);
    SourceType value;
    for (size_t i=0; i<n; i++)
    {
        stream->read((char *) &value, sizeof(SourceType));
        if (swapEndian)
            swap(&value);
        values[i] = static_cast<FinalType>(value);
    }
}

std::string BinaryInputStream::readString (size_t n)
{
    char *value = new char[n];
    stream->read(value, n);
    std::string finalValue(value, n);
    delete[] value;
    return finalValue;
}

// Explicit declarations to ensure code is generated
template int16_t BinaryInputStream::readValue<int16_t> ();
template int32_t BinaryInputStream::readValue<int32_t> ();

template void BinaryInputStream::readValues<float,float> (std::vector<float> &values, size_t n);
template void BinaryInputStream::readValues<int16_t,int> (std::vector<int> &values, size_t n);

template void BinaryInputStream::readValues<float,float> (arma::Col<float> &values, size_t n);
