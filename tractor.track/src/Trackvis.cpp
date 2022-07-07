#include <Rcpp.h>

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

void TrackvisSourceFileAdapter::open ()
{
    // Must be -1 if there is no seed property
    seedProperty = -1;
    
    // Read header size and check endianness
    inputStream->seekg(996);
    int32_t headerSize = inputStream.readValue<int32_t>();
    if (headerSize != 1000)
    {
        BinaryStream::swap(headerSize);
        if (headerSize == 1000)
            inputStream.setEndianness("swapped");
        else
            throw runtime_error("Trackvis file does not declare the expected header size");
    }
    
    inputStream->seekg(0);
    if (inputStream.readString(6).compare(0,5,"TRACK") != 0)
        throw runtime_error("Trackvis file does not seem to have a valid magic number");
    
    space = new ImageSpace;
    
    inputStream.readArray<int16_t>(space->dim);
    inputStream.readArray<float>(space->pixdim);
    
    inputStream->seekg(12, ios::cur);
    nScalars = inputStream.readValue<int16_t>();
    inputStream->seekg(200, ios::cur);
    const int nProperties = inputStream.readValue<int16_t>();
    for (int i=0; i<std::min(nProperties,10); i++)
    {
        const std::string propertyName = inputStream.readString(20);
        properties.push_back(propertyName);
        if (propertyName == "seed")
            seedProperty = i;
    }
    
    array<ImageSpace::Element,16> elements;
    inputStream->seekg(440, ios::beg);
    inputStream.readArray<float>(elements);
    space->transform = ImageSpace::Transform(elements.data());
    
    inputStream->seekg(988, ios::beg);
    count = inputStream.readValue<int32_t>();
    
    inputStream->seekg(1000);
}

void TrackvisSourceFileAdapter::read (Streamline &data)
{
    int32_t nPoints = inputStream.readValue<int32_t>();
    if (nPoints > 0)
    {
        vector<ImageSpace::Point> points;
        ImageSpace::Point point;
        int seed = 0;
        for (int32_t i=0; i<nPoints; i++)
        {
            inputStream.readPoint<float>(point);
            
            // TrackVis indexes from the left edge of each voxel
            for (int i=0; i<3; i++)
                point[i] = point[i] / space->pixdim[i] - 0.5;
            
            if (nScalars > 0)
                inputStream->seekg(4 * nScalars, ios::cur);
        }
        
        if (seedProperty >= 0)
        {
            inputStream->seekg(4 * seedProperty, ios::cur);
            seed = static_cast<int>(inputStream.readValue<float>());
        }
        if (nProperties > 0)
            inputStream->seekg(4 * (nProperties-seedProperty-1), ios::cur);
        
        data = Streamline(vector<ImageSpace::Point>(points.rend()-seed-1, points.rend()),
                          vector<ImageSpace::Point>(points.begin()+seed, points.end()),
                          ImageSpace::VoxelPointType,
                          space->pixdim,
                          false);
    }
    else
    {
        if (nProperties > 0)
            inputStream->seekg(4 * nProperties, ios::cur);
    }
    
    currentStreamline++;
}

void TrackvisSourceFileAdapter::skip (const size_t n)
{
    for (size_t i=0; i<n; i++)
    {
        const int nPoints = binaryStream.readValue<int32_t>();
        inputStream->seekg(4 * ((3+nScalars) * nPoints + nProperties()), ios::cur);
    }
}

size_t TrackvisSinkFileAdapter::open (const bool append)
{
    if (append)
    {
        size_t nStreamlines;
        BinaryInputStream inputStream(path);
        inputStream->seekg(988, ios::beg);
        inputStream.readValue<int32_t>(nStreamlines);
        
        outputStream->seekp(0, ios::end);
        return nStreamlines;
    }
    
    outputStream->seekp(0);
    outputStream.writeString("TRACK");
    
    outputStream.writeArray<int16_t>(space->dim);
    outputStream.writeArray<float>(space->pixdim);
    outputStream.writeValues<float>(0.0, 3);
    
    outputStream.writeValue<int16_t>(0);
    outputStream.writeValues<char>(0, 200);
    outputStream.writeValue<int16_t>(3);
    outputStream.writeString("seed", false);
    outputStream.writeValues<char>(0, 16);
    outputStream.writeString("Ltermcode", false);
    outputStream.writeValues<char>(0, 11);
    outputStream.writeString("Rtermcode", false);
    outputStream.writeValues<char>(0, 151);
    
    outputStream.writeArray(space->transform.begin(), 16);
    outputStream.writeValues<char>(0, 444);
    
    RNifti::NiftiImage::Xform xform(space->transform);
    outputStream.writeString(xform.orientation());
    outputStream.writeValues<char>(0, 4);
    
    RNifti::NiftiImage::Xform::Submatrix rotationMatrix = xform.rotation();
    outputStream.writeValue<float>(-rotationMatrix(0,0));
    outputStream.writeValue<float>(-rotationMatrix(0,1));
    outputStream.writeValue<float>(rotationMatrix(0,2));
    outputStream.writeValue<float>(-rotationMatrix(1,0));
    outputStream.writeValue<float>(-rotationMatrix(1,1));
    outputStream.writeValue<float>(rotationMatrix(1,2));
    outputStream.writeValues<char>(0, 8);
    
    outputStream.writeValue<int32_t>(0);
    outputStream.writeValue<int32_t>(2);
    outputStream.writeValue<int32_t>(1000);
    
    return 0;
}

size_t TrackvisSinkFileAdapter::write (const Streamline &data)
{
    const size_t offset = outputStream->tellp();
    
    const size_t nPoints = data.nPoints();
    std::vector<ImageSpace::Point> points = data.getPoints();
    
    outputStream.writeValue<int32_t>(nPoints);
    for (size_t i=0; i<nPoints; i++)
    {
        // TrackVis indexes from the left edge of each voxel
        ImageSpace::Point trkPoint = points[i];
        for (int j=0; j<3; j++)
            trkPoint[j] = (trkPoint[j] + 0.5) * space->pixdim[j];
        outputStream.writePoint<float>(trkPoint);
    }
    
    // In practice, we should be able to squeeze the seed index into a float, but check
    const size_t seedIndex = data.getSeedIndex();
    if (seedIndex > 16777216)
        Rf_warning("Seed index %lu is not representable exactly as a 32-bit floating point value\n", seedIndex);
    outputStream.writeValue<float>(seedIndex);
    
    // Store termination reasons
    outputStream.writeValue(static_cast<float>(data.getLeftTerminationReason()));
    outputStream.writeValue(static_cast<float>(data.getRightTerminationReason()));
}

void TrackvisSinkFileAdapter::close (const size_t &count)
{
    outputStream->seekp(988);
    outputStream.writeValue<int32_t>(count);
}
