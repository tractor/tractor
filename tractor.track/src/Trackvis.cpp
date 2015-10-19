#include <RcppEigen.h>

#include "nifti1_io.h"

#include "NiftiImage.h"
#include "Streamline.h"
#include "BinaryStream.h"
#include "Trackvis.h"

using namespace std;

std::map<int,char> TrackvisDataSink::orientationCodeMap = TrackvisDataSink::createOrientationCodeMap();

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
// vox_to_ras[4][4]             float            64       4x4 matrix for voxel to RAS (crs to xyz) transformation. If vox_to_ras[3][3] is 0, it means the matrix is not recorded. This field is added from version 2.
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

void TrackvisDataSource::attach (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open(fileStem + ".trk", ios::binary);
    
    // Must be -1 if there is no seed property, for get()
    seedProperty = -1;
    
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
    binaryStream.readVector<int16_t>(imageDims, 3);
    binaryStream.readVector<float>(voxelDims, 3);
    
    fileStream.seekg(12, ios::cur);
    nScalars = binaryStream.readValue<int16_t>();
    fileStream.seekg(200, ios::cur);
    nProperties = binaryStream.readValue<int16_t>();
    for (int i=0; i<nProperties; i++)
    {
        if (binaryStream.readString(20) == "seed")
        {
            seedProperty = i;
            break;
        }
    }
    fileStream.seekg(988, ios::beg);
    totalStreamlines = binaryStream.readValue<int32_t>();
    
    fileStream.seekg(1000);
    currentStreamline = 0;
}

void AugmentedTrackvisDataSource::attach (const std::string &fileStem)
{
    TrackvisDataSource::attach(fileStem);
    
    if (auxFileStream.is_open())
        auxFileStream.close();
    
    auxFileStream.open(fileStem + ".trka", ios::binary);
}

bool TrackvisDataSource::more ()
{
    return (currentStreamline < totalStreamlines);
}

void TrackvisDataSource::get (Streamline &data)
{
    int32_t nPoints = binaryStream.readValue<int32_t>();
    if (nPoints == 0)
        return;
    
    vector<Space<3>::Point> points;
    int seed = 0;
    for (int32_t i=0; i<nPoints; i++)
    {
        Space<3>::Point point;
        binaryStream.readVector<float>(point, 3);
        // TrackVis indexes from the left edge of each voxel
        points.push_back(point / voxelDims - 0.5);
        
        if (seedProperty >= 0)
        {
            fileStream.seekg(4 * seedProperty, ios::cur);
            seed = static_cast<int>(binaryStream.readValue<float>());
        }
        if (nScalars > 0)
            fileStream.seekg(4 * (nScalars-seedProperty-1), ios::cur);
    }
    
    data = Streamline(vector<Space<3>::Point>(points.rend()-seed-1, points.rend()),
                      vector<Space<3>::Point>(points.begin()+seed, points.end()),
                      Streamline::VoxelPointType,
                      voxelDims,
                      false);
    
    if (nProperties > 0)
        fileStream.seekg(4 * nProperties, ios::cur);
    currentStreamline++;
}

void TrackvisDataSink::attach (const std::string &fileStem, const NiftiImage &image)
{
    if (image.getDimensionality() < 3)
        throw std::invalid_argument("Specified image has less than three dimensions");
    
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open(fileStem + ".trk", ios::binary);
    
    char magicNumber[6] = { 'T','R','A','C','K','\0' };
    fileStream.seekp(0);
    fileStream.write(magicNumber, 6);
    
    binaryStream.writeVector<int16_t>(image.getDimensions(), 3);
    binaryStream.writeVector<float>(image.getVoxelDimensions(), 3);
    binaryStream.writeValues<float>(0.0, 3);
    
    binaryStream.writeValue<int16_t>(0);
    binaryStream.writeValues<char>(0, 200);
    binaryStream.writeValue<int16_t>(1);
    fileStream.write("seed", 4);
    binaryStream.writeValues<char>(0, 196);
    
    ::mat44 xform = image.getXformStruct();
    binaryStream.writeArray<float>(xform.m[0], 16);
    binaryStream.writeValues<char>(0, 444);
    
    int icode, jcode, kcode;
    nifti_mat44_to_orientation(xform, &icode, &jcode, &kcode);
    char orientation[4];
    orientation[0] = TrackvisDataSink::orientationCodeMap[icode];
    orientation[1] = TrackvisDataSink::orientationCodeMap[jcode];
    orientation[2] = TrackvisDataSink::orientationCodeMap[kcode];
    orientation[3] = '\0';
    fileStream.write(orientation, 4);
    binaryStream.writeValues<char>(0, 4);
    
    float qb, qc, qd, qfac;
    nifti_mat44_to_quatern(xform, &qb, &qc, &qd, NULL, NULL, NULL, NULL, NULL, NULL, &qfac);
    ::mat44 rotationMatrix = nifti_quatern_to_mat44(qb, qc, qd, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, qfac);
    binaryStream.writeArray<float>(rotationMatrix.m[0], 3);
    binaryStream.writeArray<float>(rotationMatrix.m[1], 3);
    binaryStream.writeValues<char>(0, 8);
    
    binaryStream.writeValue<int32_t>(0);
    binaryStream.writeValue<int32_t>(2);
    binaryStream.writeValue<int32_t>(1000);
    
    totalStreamlines = 0;
    std::copy(image.getVoxelDimensions().begin(), image.getVoxelDimensions().begin()+3, voxelDims.data());
}

void AugmentedTrackvisDataSink::attach (const std::string &fileStem, const NiftiImage &image)
{
    TrackvisDataSink::attach(fileStem, image);
    
    if (auxFileStream.is_open())
        auxFileStream.close();
    
    auxFileStream.open(fileStem + ".trka", ios::binary);
    
    // File version number
    auxBinaryStream.writeValue<int32_t>(1);
    
    // Number of streamlines, unknown at this point
    auxBinaryStream.writeValue<int32_t>(0);
    
    // Number of labels
    auxBinaryStream.writeValue<int32_t>(labelDictionary.size());
    
    // 20 bytes' padding for future versions
    auxBinaryStream.writeValues<int32_t>(0, 5);
    
    // Write out label dictionary
    for (std::map<int,std::string>::const_iterator it=labelDictionary.begin(); it!=labelDictionary.end(); it++)
    {
        const std::pair<int,std::string> &element = *it;
        auxBinaryStream.writeValue<int32_t>(element.first);
        auxBinaryStream.writeString(element.second);
        auxBinaryStream.writeValue<char>(0);
    }
}

void TrackvisDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
    if (totalStreamlines > std::numeric_limits<int32_t>::max())
        throw std::runtime_error("Total streamline count exceeds the storage capacity of the Trackvis format");
}

void TrackvisDataSink::put (const Streamline &data)
{
    int nPoints = data.nPoints();
    Eigen::ArrayX3f points;
    
    data.concatenatePoints(points);
    
    binaryStream.writeValue<int32_t>(nPoints);
    for (int i=0; i<nPoints; i++)
    {
        // TrackVis indexes from the left edge of each voxel
        Eigen::Array3f row = (points.row(i) + 0.5) * voxelDims.transpose();
        binaryStream.writeVector<float>(row);
    }
    
    // In practice, we should be able to squeeze the seed index into a float, but check
    const size_t seedIndex = data.getSeedIndex();
    if (seedIndex > 16777216)
        Rf_warning("Seed index %lu is not representable exactly as a 32-bit floating point value\n", seedIndex);
    binaryStream.writeValue<float>(seedIndex);
}

void AugmentedTrackvisDataSink::put (const Streamline &data)
{
    TrackvisDataSink::put(data);
    
    auxBinaryStream.writeValue<int32_t>(data.nPoints());
    auxBinaryStream.writeValue<int32_t>(data.getSeedIndex());
    
    const std::set<int> &labels = data.getLabels();
    auxBinaryStream.writeValue<int32_t>(labels.size());
    for (std::set<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
        auxBinaryStream.writeValue<int32_t>(*it);
}

void TrackvisDataSink::done ()
{
    fileStream.seekp(988);
    binaryStream.writeValue<int32_t>(totalStreamlines);
}

void AugmentedTrackvisDataSink::done ()
{
    TrackvisDataSink::done();
    
    auxFileStream.seekp(4);
    auxBinaryStream.writeValue<int32_t>(totalStreamlines);
}
