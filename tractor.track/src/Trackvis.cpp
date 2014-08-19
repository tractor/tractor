#include <RcppArmadillo.h>

#include "Streamline.h"
#include "BinaryStream.h"
#include "Trackvis.h"

using namespace std;

// Name.........................Data type........Bytes....Comment..........................................................
// 
// id_string[6]                 char             6        ID string for track file. The first 5 characters must be "TRACK".
// dim[3]                       short int        6        Dimension of the image volume.
// voxel_size[3]                float            12       Voxel size of the image volume.
// origin[3]                    float            12       Origin of the image volume. This field is not yet being used by TrackVis. That means the origin is always (0, 0, 0).
// n_scalars                    short int        2        Number of scalars saved at each track point (besides x, y and z coordinates).
// scalar_name[10][20]          char             200      Name of each scalar. Can not be longer than 20 characters each. Can only store up to 10 names.
// n_properties                 short int        2        Number of properties saved at each track.
// property_name[10][20]        char             200      Name of each property. Can not be longer than 20 characters each. Can only store up to 10 names.
// vox_to_ras[4][4]             float            64        4x4 matrix for voxel to RAS (crs to xyz) transformation. If vox_to_ras[3][3] is 0, it means the matrix is not recorded. This field is added from version 2.
// reserved[444]                char             444      Reserved space for future version.
// voxel_order[4]               char             4        Storing order of the original image data. Explained here.
// pad2[4]                      char             4        Paddings.
// image_orientation_patient[6] float            24       Image orientation of the original image. As defined in the DICOM header.
// pad1[2]                      char             2        Paddings.
// invert_x                     unsigned char    1        Inversion/rotation flags used to generate this track file. For internal use only.
// invert_y                     unsigned char    1        As above.
// invert_x                     unsigned char    1        As above.
// swap_xy                      unsigned char    1        As above.
// swap_yz                      unsigned char    1        As above.
// swap_zx                      unsigned char    1        As above.
// n_count                      int              4        Number of tracks stored in this track file. 0 means the number was NOT stored.
// version                      int              4        Version number. Current version is 2.
// hdr_size                     int              4        Size of the header. Used to determine byte swap. Should be 1000.
// 
// Source: Trackvis documentation (http://www.trackvis.org/docs/?subsect=fileformat)

void TrackvisDataSource::setup (const std::string &fileName)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open(fileName.c_str(), ios::binary);
    
    int32_t headerSize;
    fileStream.seekg(996);
    fileStream.read((char *) &headerSize, sizeof(int32_t));
    
    binaryStream.swapEndianness(headerSize != 1000);
    fileStream.seekg(996);
    headerSize = binaryStream.readValue<int32_t>();
    if (headerSize != 1000)
        throw runtime_error("Trackvis file does not declare the expected header size");
    
    fileStream.seekg(0);
    if (binaryStream.readString(5) != "TRACK")
        throw runtime_error("Trackvis file does not seem to have a valid magic number");
    
    vector<int> imageDims;
    binaryStream.readValues<int16_t>(imageDims, 3);
    vector<float> voxelDims;
    binaryStream.readValues<float>(voxelDims, 3);
    
    fileStream.seekg(12, ios::cur);
    nScalars = binaryStream.readValue<int16_t>();
    fileStream.seekg(200, ios::cur);
    nProperties = binaryStream.readValue<int16_t>();
    fileStream.seekg(988, ios::beg);
    totalStreamlines = binaryStream.readValue<int32_t>();
    
    fileStream.seekg(1000);
    currentStreamline = 0;
}

bool TrackvisDataSource::more ()
{
    return (currentStreamline < totalStreamlines);
}

void TrackvisDataSource::get (Streamline &data)
{
    int32_t nPoints = binaryStream.readValue<int32_t>();
    vector<Space<3>::Point> leftPoints, rightPoints;
    for (int32_t i=0; i<nPoints; i++)
    {
        Space<3>::Point point;
        binaryStream.readValues<float>(point, 3);
        leftPoints.push_back(point);
        if (nScalars > 0)
            fileStream.seekg(4 * nScalars, ios::cur);
    }
    
    data = Streamline(leftPoints, rightPoints, Streamline::WorldPointType, false);
    
    if (nProperties > 0)
        fileStream.seekg(4 * nProperties, ios::cur);
    currentStreamline++;
}
