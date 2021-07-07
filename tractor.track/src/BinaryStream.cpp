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

std::string BinaryStream::nativeEndianness () const
{
    uint32_t value = 1;
    return ((*((char *) &value) == 1) ? "little" : "big");
}

void BinaryStream::setEndianness (const std::string &endianness)
{
    if (endianness == "native")
        swapEndian = false;
    else if (endianness == "swapped")
        swapEndian = true;
    else if (endianness == "little" || endianness == "big")
        swapEndian = (endianness == nativeEndianness());
    // NB: invalid values are silently ignored
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
    if (n == 0)
        n = values.size();
    else
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

template <typename SourceType, typename FinalType, int Rows, int Cols>
void BinaryInputStream::readMatrix (Eigen::Matrix<FinalType,Rows,Cols> &values, size_t n, size_t m)
{
    values.resize(n, m);
    SourceType value;
    for (size_t i=0; i<n; i++)
    {
        for (size_t j=0; j<m; j++)
        {
            stream->read((char *) &value, sizeof(SourceType));
            if (swapEndian)
                swap(&value);
            values(i, j) = static_cast<FinalType>(value);
        }
    }
}

std::string BinaryInputStream::readString (const std::string delim)
{
    std::string finalValue;
    std::getline(*stream, finalValue, delim[0]);
    return finalValue;
}

std::string BinaryInputStream::readString (const size_t n)
{
    if (n == 0)
    {
        std::string finalValue;
        std::getline(*stream, finalValue, '\0');
        return finalValue;
    }
    else
    {
        char *value = new char[n];
        stream->read(value, n);
        std::string finalValue(value, std::min(n,strlen(value)));
        delete[] value;
        return finalValue;
    }
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
    TargetType value;
    for (size_t i=0; i<n; i++)
    {
        value = static_cast<TargetType>(values[i]);
        if (swapEndian)
            swap(&value);
        stream->write((const char *) &value, sizeof(TargetType));
    }
}

template <typename TargetType, typename OriginalType, int Rows, int Cols>
void BinaryOutputStream::writeMatrix (const Eigen::Matrix<OriginalType,Rows,Cols> &values, size_t n, size_t m)
{
    TargetType value;
    for (size_t i=0; i<n; i++)
    {
        for (size_t j=0; j<m; j++)
        {
            value = static_cast<TargetType>(values(i, j));
            if (swapEndian)
                swap(&value);
            stream->write((const char *) &value, sizeof(TargetType));
        }
    }
}

void BinaryOutputStream::writeString (const std::string &value)
{
    stream->write(value.data(), value.length());
}

// Explicit declarations to ensure code is generated
template float BinaryInputStream::readValue<float> ();
template int16_t BinaryInputStream::readValue<int16_t> ();
template int32_t BinaryInputStream::readValue<int32_t> ();
template uint64_t BinaryInputStream::readValue<uint64_t> ();

template void BinaryInputStream::readVector<float,float> (std::vector<float> &values, size_t n);
template void BinaryInputStream::readVector<int16_t,int> (std::vector<int> &values, size_t n);

template void BinaryInputStream::readVector<float> (Eigen::Vector3f &values, size_t n);
template void BinaryInputStream::readVector<float> (Eigen::Array3f &values, size_t n);
template void BinaryInputStream::readVector<double> (Eigen::Array3f &values, size_t n);
template void BinaryInputStream::readVector<short> (Eigen::Array3i &values, size_t n);

template void BinaryInputStream::readMatrix<float> (Eigen::Matrix4f &values, size_t n, size_t m);

template void BinaryOutputStream::writeValue<char> (char value);
template void BinaryOutputStream::writeValue<float> (float value);
template void BinaryOutputStream::writeValue<int16_t> (int16_t value);
template void BinaryOutputStream::writeValue<int32_t> (int32_t value);
template void BinaryOutputStream::writeValue<uint64_t> (uint64_t value);

template void BinaryOutputStream::writeValues<char> (char value, size_t n);
template void BinaryOutputStream::writeValues<float> (float value, size_t n);
template void BinaryOutputStream::writeValues<int32_t> (int32_t value, size_t n);

template void BinaryOutputStream::writeArray<float> (float * const pointer, size_t n);

template void BinaryOutputStream::writeVector<float,float>(const std::vector<float> &values, size_t n);
template void BinaryOutputStream::writeVector<int16_t,int>(const std::vector<int> &values, size_t n);

template void BinaryOutputStream::writeVector<float>(const Eigen::Vector3f &values, size_t n);
template void BinaryOutputStream::writeVector<float>(const Eigen::Array3f &values, size_t n);
template void BinaryOutputStream::writeVector<short> (const Eigen::Array3i &values, size_t n);

template void BinaryOutputStream::writeMatrix<float> (const Eigen::Matrix4f &values, size_t n, size_t m);
