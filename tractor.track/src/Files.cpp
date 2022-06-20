#include <Rcpp.h>

#include "BinaryStream.h"
#include "Files.h"

void StreamlineFileSource::readLabels (const std::string &path)
{
    BinaryInputStream inputStream(path);
    
    if (inputStream.readString(8) != "TRKLABEL")
        throw std::runtime_error("Track label file does not seem to have a valid magic number");
    
    const int version = inputStream.readValue<int32_t>();
    inputStream.setEndianness(version < 0 || version > 0xffff ? "swapped" : "native");
    
    const int nStreamlines = inputStream.readValue<int32_t>();
    const int nLabels = inputStream.readValue<int32_t>();
    inputStream->seekg(32);
    
    for (int i=0; i<nLabels; i++)
    {
        inputStream.readValue<int32_t>();
        inputStream.readString();
    }
    
    offsets.clear();
    labels.clear();
    for (int j=0; j<nStreamlines; j++)
    {
        offsets.push_back(inputStream.readValue<uint64_t,size_t>());
        const int currentCount = inputStream.readValue<int32_t>();
        std::set<int> currentLabels;
        for (int i=0; i<currentCount; i++)
            currentLabels.insert(inputStream.readValue<int32_t>());
        labels.push_back(currentLabels);
    }
    
    haveLabels = true;
}

void StreamlineFileSource::seek (const size_t n)
{
    if (currentStreamline == n)
        return;
    else if (haveLabels && offsets.size() > n)
        source->seek(offsets[n]);
    else
    {
        if (n < currentStreamline)
        {
            source->seek(source->dataOffset());
            currentStreamline = 0;
        }
        source->skip(n - currentStreamline);
    }
    
    currentStreamline = n;
}

void StreamlineFileSink::writeLabels (const std::string &path)
{
    BinaryOutputStream outputStream(path);
    
    // Magic number (unterminated)
    outputStream.writeString("TRKLABEL", false);
    
    // File version number (offset 8)
    outputStream.writeValue<int32_t>(1);
    
    // Number of streamlines, unknown at this point (offset 12)
    outputStream.writeValue<int32_t>(0);
    
    // Number of labels (offset 16)
    outputStream.writeValue<int32_t>(dictionary.size());
    
    // 12 bytes' padding for future versions (offset 20)
    outputStream.writeValues<int32_t>(0, 3);
    
    // Write out label dictionary (offset 32)
    for (auto it=dictionary.begin(); it!=dictionary.end(); it++)
    {
        const std::pair<int,std::string> &element = *it;
        outputStream.writeValue<int32_t>(element.first);
        outputStream.writeString(element.second);
        outputStream.writeValue<char>(0);
    }
}
