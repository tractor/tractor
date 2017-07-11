#include <RcppEigen.h>

#include "Streamline.h"
#include "BinaryStream.h"
#include "Mrtrix.h"

using namespace std;

void MrtrixDataSource::readStreamline (Streamline &data)
{
    vector<Space<3>::Point> points;
    while (true)
    {
        Space<3>::Point point;
        binaryStream.readVector<float>(point, 3);
        
        if (ISNAN(point[0]) && ISNAN(point[1]) && ISNAN(point[2]))
            break;
        else if (point[0] == R_PosInf && point[1] == R_PosInf && point[2] == R_PosInf)
            break;
        else
            points.push_back(point);
    }
    
    data = Streamline(vector<Space<3>::Point>(points.begin(), points.begin()+1),
                      points,
                      Streamline::WorldPointType,
                      grid.spacings(),
                      true);
    
    currentStreamline++;
}

void MrtrixDataSource::attach (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open((fileStem + ".tck").c_str(), ios::binary);
    
    size_t dataOffset = 0;
    while (true)
    {
        const string str = binaryStream.readString("\n");
        if (str == "END")
            break;
        else if (str.compare(0,8,"file: . ") == 0)
            dataOffset = static_cast<size_t>(atol(str.substr(8).c_str()));
        else if (str.compare(0,7,"count: ") == 0)
            totalStreamlines = static_cast<size_t>(atol(str.substr(7).c_str()));
    }
    
    fileStream.seekg(dataOffset);
    currentStreamline = 0;
}
