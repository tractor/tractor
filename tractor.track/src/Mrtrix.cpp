#include <Rcpp.h>

#include "Streamline.h"
#include "BinaryStream.h"
#include "Mrtrix.h"

void MrtrixSourceFileAdapter::open (StreamlineFileMetadata &metadata)
{
    inputStream->seekg(0);
    
    const std::string magic = inputStream.readString("\n");
    if (magic.compare(0,13,"mrtrix tracks") != 0)
        throw std::runtime_error("File " + path + " does not contain an MRtrix magic number");
    
    metadata.dataOffset = 0;
    while (true)
    {
        const std::string str = inputStream.readString("\n");
        if (inputStream->eof() || str == "END")
            break;
        else if (str.compare(0,8,"file: . ") == 0)
            metadata.dataOffset = static_cast<size_t>(atol(str.substr(8).c_str()));
        else if (str.compare(0,7,"count: ") == 0)
            metadata.count = static_cast<size_t>(atol(str.substr(7).c_str()));
        else if (str.compare(0,10,"datatype: ") == 0)
        {
            const std::string datatypeString = str.substr(10);
            if (datatypeString == "Float32BE")      { datatype = "float";   inputStream.setEndianness("big");       }
            else if (datatypeString == "Float32LE") { datatype = "float";   inputStream.setEndianness("little");    }
            else if (datatypeString == "Float64BE") { datatype = "double";  inputStream.setEndianness("big");       }
            else if (datatypeString == "Float64LE") { datatype = "double";  inputStream.setEndianness("little");    }
            else throw std::runtime_error("MRtrix track file datatype is invalid");
        }
    }
    
    if (metadata.dataOffset == 0)
        throw std::runtime_error("File " + path + " does not seem to contain a valid MRtrix header");
    if (metadata.count == 0)
        throw std::runtime_error("Streamline count not stored in MRtrix track file header");
    
    inputStream->seekg(metadata.dataOffset);
}

void MrtrixSourceFileAdapter::read (Streamline &data)
{
    std::vector<ImageSpace::Point> points;
    while (true)
    {
        ImageSpace::Point point;
        if (datatype == "float")
            inputStream.readPoint<float>(point);
        else if (datatype == "double")
            inputStream.readPoint<double>(point);
        
        if (inputStream->eof())
            break;
        else if (ISNAN(point[0]) && ISNAN(point[1]) && ISNAN(point[2]))
            break;
        else if (point[0] == R_PosInf && point[1] == R_PosInf && point[2] == R_PosInf)
            break;
        else
            points.push_back(point);
    }
    
    if (points.size() > 0)
    {
        data = Streamline(std::vector<ImageSpace::Point>(points.begin(), points.begin()+1),
                          points,
                          PointType::World,
                          { 0, 0, 0},
                          true);
    }
}
