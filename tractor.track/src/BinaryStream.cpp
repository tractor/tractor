#include <Rcpp.h>

#include "BinaryStream.h"

// NB: templated methods are defined inline in the header

void BinaryStream::setEndianness (const std::string &endianness)
{
    if (endianness == "native")
        swapEndian = false;
    else if (endianness == "swapped")
        swapEndian = true;
    else if (endianness == "little" || endianness == "big")
    {
        uint32_t value = 1;
        swapEndian = ((*((char *) &value) == 1) ^ (endianness == "little"));
    }
    else
        throw std::runtime_error("Specified endianness is invalid");
}

std::string BinaryInputStream::readString (const std::string &delim)
{
    std::string finalValue;
    std::getline(*inputStream, finalValue, delim[0]);
    return finalValue;
}

std::string BinaryInputStream::readString (const size_t n)
{
    if (n == 0)
    {
        std::string finalValue;
        std::getline(*inputStream, finalValue, '\0');
        return finalValue;
    }
    else
    {
        char *value = new char[n];
        read<char>(value, n);
        std::string finalValue(value, std::min(n,strlen(value)));
        delete[] value;
        return finalValue;
    }
}

void BinaryOutputStream::writeString (const std::string &value, const bool terminate)
{
    write<char>(value.data(), value.length());
    if (terminate)
    {
        const char nul = '\0';
        write<char>(&nul);
    }
}
