#include <RcppEigen.h>

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
void BinaryInputStream::readVector (std::vector<FinalType> &values, size_t n)
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

template <typename SourceType, typename FinalType, int Rows>
void BinaryInputStream::readVector (Eigen::Matrix<FinalType,Rows,1> &values, size_t n)
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

template <typename SourceType, typename FinalType, int Rows>
void BinaryInputStream::readVector (Eigen::Array<FinalType,Rows,1> &values, size_t n)
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

std::string BinaryInputStream::readString (size_t n)
{
    char *value = new char[n];
    stream->read(value, n);
    std::string finalValue(value, n);
    delete[] value;
    return finalValue;
}

template <typename TargetType>
void BinaryOutputStream::writeValue (const TargetType value)
{
    if (swapEndian)
        swap(&value);
    stream->write((const char *) &value, sizeof(TargetType));
}

template <typename TargetType>
void BinaryOutputStream::writeValues (const TargetType value, size_t n)
{
    if (swapEndian)
        swap(&value);
    for (size_t i=0; i<n; i++)
        stream->write((const char *) &value, sizeof(TargetType));
}

template <typename TargetType>
void BinaryOutputStream::writeArray (TargetType * const pointer, size_t n)
{
    for (TargetType *movingPointer=pointer; movingPointer<(pointer+n); movingPointer++)
    {
        if (swapEndian)
            swap(movingPointer);
        stream->write((const char *) movingPointer, sizeof(TargetType));
    }
}

template <typename TargetType, typename OriginalType>
void BinaryOutputStream::writeVector (const std::vector<OriginalType> &values, size_t n)
{
    if (n == 0)
        n = values.size();
    
    TargetType value;
    for (size_t i=0; i<n; i++)
    {
        value = static_cast<TargetType>(values[i]);
        if (swapEndian)
            swap(&value);
        stream->write((const char *) &value, sizeof(TargetType));
    }
}

template <typename TargetType, typename OriginalType, int Rows>
void BinaryOutputStream::writeVector (const Eigen::Matrix<OriginalType,Rows,1> &values, size_t n)
{
    if (n == 0)
        n = values.size();
    
    TargetType value;
    for (size_t i=0; i<n; i++)
    {
        value = static_cast<TargetType>(values[i]);
        if (swapEndian)
            swap(&value);
        stream->write((const char *) &value, sizeof(TargetType));
    }
}

template <typename TargetType, typename OriginalType, int Rows>
void BinaryOutputStream::writeVector (const Eigen::Array<OriginalType,Rows,1> &values, size_t n)
{
    if (n == 0)
        n = values.size();
    
    TargetType value;
    for (size_t i=0; i<n; i++)
    {
        value = static_cast<TargetType>(values[i]);
        if (swapEndian)
            swap(&value);
        stream->write((const char *) &value, sizeof(TargetType));
    }
}

void BinaryOutputStream::writeString (const std::string &value)
{
    stream->write(value.data(), value.length());
}

// Explicit declarations to ensure code is generated
template int16_t BinaryInputStream::readValue<int16_t> ();
template int32_t BinaryInputStream::readValue<int32_t> ();

template void BinaryInputStream::readVector<float,float> (std::vector<float> &values, size_t n);
template void BinaryInputStream::readVector<int16_t,int> (std::vector<int> &values, size_t n);

template void BinaryInputStream::readVector<float> (Eigen::Vector3f &values, size_t n);
template void BinaryInputStream::readVector<float> (Eigen::Array3f &values, size_t n);

template void BinaryOutputStream::writeValue<char> (char value);
template void BinaryOutputStream::writeValue<float> (float value);
template void BinaryOutputStream::writeValue<int16_t> (int16_t value);
template void BinaryOutputStream::writeValue<int32_t> (int32_t value);

template void BinaryOutputStream::writeValues<char> (char value, size_t n);
template void BinaryOutputStream::writeValues<float> (float value, size_t n);
template void BinaryOutputStream::writeValues<int32_t> (int32_t value, size_t n);

template void BinaryOutputStream::writeArray<float> (float * const pointer, size_t n);

template void BinaryOutputStream::writeVector<float,float>(const std::vector<float> &values, size_t n);
template void BinaryOutputStream::writeVector<int16_t,int>(const std::vector<int> &values, size_t n);

template void BinaryOutputStream::writeVector<float>(const Eigen::Vector3f &values, size_t n);
template void BinaryOutputStream::writeVector<float>(const Eigen::Array3f &values, size_t n);
