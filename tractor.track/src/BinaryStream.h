#ifndef _BINARY_STREAM_H_
#define _BINARY_STREAM_H_

#include "Image.h"

#include <Rcpp.h>

#include <fstream>

class BinaryStream
{
protected:
    bool streamsOwned = false;
    bool swapEndian = false;
    std::ifstream *inputStream = nullptr;
    std::ofstream *outputStream = nullptr;
    
    template <typename SourceType, typename FinalType> void read (FinalType *values, const size_t n = 1);
    template <typename TargetType, typename OriginalType> void write (const OriginalType *values, const size_t n = 1);
    
public:
    virtual ~BinaryStream ()
    {
        if (inputStream != nullptr && inputStream->is_open())
            inputStream->close();
        if (outputStream != nullptr && outputStream->is_open())
            outputStream->close();
        if (streamsOwned)
        {
            delete inputStream;
            delete outputStream;
        }
    }
    
    template <typename Type> static void swap (Type &value);
    
    void setEndianness (const std::string &endianness);
};

class BinaryInputStream : public BinaryStream
{
public:
    BinaryInputStream ()                        { }
    BinaryInputStream (std::ifstream *stream)   { attach(stream); }
    BinaryInputStream (const std::string &path) { attach(path); }
    
    void attach (std::ifstream *stream) { this->inputStream = stream; }
    void attach (const std::string &path)
    {
        this->inputStream = new std::ifstream(path, std::ios::in | std::ios::binary);
        if (!inputStream)
            throw std::runtime_error("Failed to open file " + path);
        this->streamsOwned = true;
    }
    
    template <typename SourceType, typename FinalType = SourceType> FinalType readValue ();
    template <typename SourceType, typename FinalType> void readValue (FinalType &value);
    template <typename SourceType, typename FinalType, size_t N> void readArray (std::array<FinalType,N> &values);
    template <typename SourceType, typename FinalType> void readVector (std::vector<FinalType> &values, size_t n = 0);
    template <typename SourceType> void readPoint (ImageSpace::Point &value);
    std::string readString (const std::string &delim = "\0");
    std::string readString (const size_t n);
    
    // Allow pass-through calls to the underlying ifstream via the arrow operator
    const std::ifstream * operator-> () const   { return inputStream; }
    std::ifstream * operator-> ()               { return inputStream; }
};

class BinaryOutputStream : public BinaryStream
{
public:
    BinaryOutputStream ()                           { }
    BinaryOutputStream (std::ofstream *stream)      { attach(stream); }
    BinaryOutputStream (const std::string &path)    { attach(path); }
    
    void attach (std::ofstream *stream) { this->outputStream = stream; }
    void attach (const std::string &path)
    {
        this->outputStream = new std::ofstream(path, std::ios::out | std::ios::binary);
        if (!outputStream)
            throw std::runtime_error("Failed to open file " + path);
        this->streamsOwned = true;
    }
    
    template <typename TargetType> void writeValue (const TargetType &value);
    template <typename TargetType> void writeValues (const TargetType &value, size_t n);
    template <typename TargetType> void writeArray (TargetType * const pointer, size_t n);
    template <typename TargetType, typename OriginalType, size_t N> void writeArray (const std::array<OriginalType,N> &values);
    template <typename TargetType, typename OriginalType> void writeVector (const std::vector<OriginalType> &values, size_t n = 0);
    template <typename TargetType> void writePoint (const ImageSpace::Point &value);
    void writeString (const std::string &value, const bool terminate = true);
    
    // Allow pass-through calls to the underlying ofstream via the arrow operator
    const std::ofstream * operator-> () const   { return outputStream; }
    std::ofstream * operator-> ()               { return outputStream; }
};

template <typename Type>
inline void BinaryStream::swap (Type &value)
{
    union {
        Type value;
        std::array<unsigned char, sizeof(Type)> bytes;
    } original, swapped;
    
    original.value = value;
    std::reverse_copy(original.bytes.begin(), original.bytes.end(), swapped.bytes.begin());
    value = swapped.value;
}

// Single-byte types obviously don't need swapping
template <>
inline void BinaryStream::swap<char> (char &value) {}

template <typename SourceType, typename FinalType>
inline void BinaryStream::read (FinalType *values, const size_t n)
{
    if (inputStream == nullptr)
        throw std::runtime_error("No input stream is attached");
    
    SourceType *originalValues;
    if (std::is_same<SourceType,FinalType>::value)
    {
        // The cast should be unnecessary, because the branch will not exist
        // when the types don't match, but Clang, at least, produces an error
        // when instantiated with SourceType and FinalType not matching
        originalValues = (SourceType *) values;
    }
    else
        originalValues = new SourceType[n];
    
    inputStream->read((char *) originalValues, sizeof(SourceType) * n);
    if (inputStream->fail())
        throw std::runtime_error("Failed to read data from file");
    if (swapEndian && sizeof(SourceType) > 1)
        std::for_each(originalValues, originalValues + n, BinaryStream::swap<SourceType>);
    
    if (!std::is_same<SourceType,FinalType>::value)
    {
        std::transform(originalValues, originalValues + n, values, [](const SourceType &val) { return static_cast<FinalType>(val); });
        delete[] originalValues;
    }
}

template <typename TargetType, typename OriginalType>
inline void BinaryStream::write (const OriginalType *values, const size_t n)
{
    if (outputStream == nullptr)
        throw std::runtime_error("No output stream is attached");
    
    TargetType *finalValues;
    if (std::is_same<TargetType,OriginalType>::value && !swapEndian)
        finalValues = (TargetType *) values;
    else
    {
        finalValues = new TargetType[n];
        if (std::is_same<TargetType,OriginalType>::value)
            std::copy(values, values + n, finalValues);
        else
            std::transform(values, values + n, finalValues, [](const OriginalType &val) { return static_cast<TargetType>(val); });
        if (swapEndian && sizeof(TargetType) > 1)
            std::for_each(finalValues, finalValues + n, BinaryStream::swap<TargetType>);
    }
    
    outputStream->write((const char *) finalValues, sizeof(TargetType) * n);
    if (outputStream->fail())
        throw std::runtime_error("Failed to write data to file");
    
    if (!std::is_same<TargetType,OriginalType>::value || swapEndian)
        delete[] finalValues;
}

template <typename SourceType, typename FinalType>
inline FinalType BinaryInputStream::readValue ()
{
    FinalType value;
    read<SourceType>(&value);
    return value;
}

template <typename SourceType, typename FinalType>
inline void BinaryInputStream::readValue (FinalType &value)
{
    read<SourceType>(&value);
}

template <typename SourceType, typename FinalType, size_t N>
inline void BinaryInputStream::readArray (std::array<FinalType,N> &values)
{
    read<SourceType>(values.data(), N);
}

template <typename SourceType, typename FinalType>
inline void BinaryInputStream::readVector (std::vector<FinalType> &values, size_t n)
{
    if (n == 0)
        n = values.size();
    else
        values.resize(n);
    read<SourceType>(values.data(), n);
}

template <typename SourceType>
inline void BinaryInputStream::readPoint (ImageSpace::Point &value)
{
    ImageSpace::Element elements[3];
    read<SourceType>(elements, 3);
    value = ImageSpace::Point(elements);
}

template <typename TargetType>
inline void BinaryOutputStream::writeValue (const TargetType &value)
{
    write<TargetType,TargetType>(&value);
}

template <typename TargetType>
inline void BinaryOutputStream::writeValues (const TargetType &value, size_t n)
{
    const std::vector<TargetType> values(n, value);
    write<TargetType,TargetType>(values.data(), n);
}

template <typename TargetType>
inline void BinaryOutputStream::writeArray (TargetType * const pointer, size_t n)
{
    write<TargetType,TargetType>(pointer, n);
}

template <typename TargetType, typename OriginalType, size_t N>
inline void BinaryOutputStream::writeArray (const std::array<OriginalType,N> &values)
{
    write<TargetType,OriginalType>(values.data(), N);
}

template <typename TargetType, typename OriginalType>
inline void BinaryOutputStream::writeVector (const std::vector<OriginalType> &values, size_t n)
{
    if (n == 0)
        n = values.size();
    write<TargetType,OriginalType>(values.data(), n);
}

template <typename TargetType>
inline void BinaryOutputStream::writePoint (const ImageSpace::Point &value)
{
    ImageSpace::Element elements[3] = { value[0], value[1], value[2] };
    write<TargetType,ImageSpace::Element>(elements, 3);
}

#endif
