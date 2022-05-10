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

template <typename ElementType>
static ElementType getNthElement (std::vector<ElementType> vec, size_t n)
{
    std::nth_element(vec.begin(), vec.begin()+n, vec.end());
    return *(vec.begin()+n);
}

void TrackvisDataSource::readStreamline (Streamline &data)
{
    int32_t nPoints = binaryStream.readValue<int32_t>();
    if (nPoints > 0)
    {
        vector<ImageSpace::Point> points;
        int seed = 0;
        for (int32_t i=0; i<nPoints; i++)
        {
            ImageSpace::Point point;
            binaryStream.readPoint<float>(point);
            
            // TrackVis indexes from the left edge of each voxel
            for (int i=0; i<3; i++)
                point[i] = point[i] / space->pixdim[i] - 0.5;
            
            if (nScalars > 0)
                fileStream.seekg(4 * nScalars, ios::cur);
        }
        
        if (seedProperty >= 0)
        {
            binaryStream->seekg(4 * seedProperty, ios::cur);
            seed = static_cast<int>(binaryStream.readValue<float>());
        }
        if (nProperties > 0)
            binaryStream->seekg(4 * (nProperties-seedProperty-1), ios::cur);
        
        data = Streamline(vector<ImageSpace::Point>(points.rend()-seed-1, points.rend()),
                          vector<ImageSpace::Point>(points.begin()+seed, points.end()),
                          ImageSpace::VoxelPointType,
                          space->pixdim,
                          false);
    }
    else
    {
        if (nProperties > 0)
            binaryStream->seekg(4 * nProperties, ios::cur);
    }
    
    currentStreamline++;
}

void TrackvisDataSource::setup ()
{
    // If the file is already open, we assume the setup is done
    if (fileStream.is_open())
        return;
    
    fileStream.open((fileStem + ".trk").c_str(), ios::binary);
    
    // Must be -1 if there is no seed property, for get()
    seedProperty = -1;
    
    int32_t headerSize;
    fileStream.seekg(996);
    fileStream.read((char *) &headerSize, sizeof(int32_t));
    
    binaryStream.setEndianness(headerSize != 1000 ? "swapped" : "native");
    fileStream.seekg(996);
    headerSize = binaryStream.readValue<int32_t>();
    if (headerSize != 1000)
        throw runtime_error("Trackvis file does not declare the expected header size");
    
    fileStream.seekg(0);
    if (binaryStream.readString(6).compare(0,5,"TRACK") != 0)
        throw runtime_error("Trackvis file does not seem to have a valid magic number");
    
    binaryStream.readArray<int16_t>(space->dim);
    binaryStream.readArray<float>(space->pixdim);
    
    fileStream.seekg(12, ios::cur);
    nScalars = binaryStream.readValue<int16_t>();
    fileStream.seekg(200, ios::cur);
    nProperties = binaryStream.readValue<int16_t>();
    for (int i=0; i<std::min(nProperties,10); i++)
    {
        const std::string propertyName = binaryStream.readString(20);
        propertyNames.push_back(propertyName);
        if (propertyName.compare(0,4,"seed") == 0)
            seedProperty = i;
    }
    
    array<ImageSpace::Element,16> elements;
    fileStream.seekg(440, ios::beg);
    binaryStream.readArray<float>(elements);
    space->transform = ImageSpace::Transform(elements.data());
    
    fileStream.seekg(988, ios::beg);
    totalStreamlines = binaryStream.readValue<int32_t>();
    
    fileStream.seekg(1000);
    currentStreamline = 0;
}

void TrackvisDataSource::get (Streamline &data)
{
    // This increments currentStreamline, so we subtract 1 below
    readStreamline(data);
    if (hasLabels())
        data.setLabels(labelList->getLabels(currentStreamline-1));
}

void TrackvisDataSource::seek (const int n)
{
    if (hasLabels())
    {
        fileStream.seekg(labelList->getOffset(n));
        currentStreamline = n;
        return;
    }
    else if (n < currentStreamline)
    {
        // FIXME: This is perfectly possible, by rewinding the file pointer first
        throw std::runtime_error("Cannot seek backwards");
    }
    
    while (currentStreamline < n)
    {
        const int nPoints = binaryStream.readValue<int32_t>();
        fileStream.seekg(4 * ((3+nScalars) * nPoints + nProperties), ios::cur);
        currentStreamline++;
    }
}

void TrackvisDataSink::writeStreamline (const Streamline &data)
{
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

void TrackvisDataSink::attach (const std::string &fileStem)
{
    outputStream.attach(fileStem + ".trk");
    
    if (append)
    {
        BinaryInputStream inputStream(fileStem + ".trk");
        inputStream->seekg(988, ios::beg);
        inputStream.readValue<int32_t>(totalStreamlines);
        
        outputStream->seekp(0, ios::end);
        return;
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
    
    totalStreamlines = 0;
}
/*
void LabelledTrackvisDataSink::attach (const std::string &fileStem)
{
    TrackvisDataSink::attach(fileStem);
    
    if (auxFileStream.is_open())
        auxFileStream.close();
    
    auxFileStream.open((fileStem + ".trkl").c_str(), ios::binary);
    
    char magicNumber[8] = { 'T', 'R', 'K', 'L', 'A', 'B', 'E', 'L' };
    auxFileStream.seekp(0);
    auxFileStream.write(magicNumber, 8);
    
    // File version number
    auxBinaryStream.writeValue<int32_t>(1);
    
    // Number of streamlines, unknown at this point
    auxBinaryStream.writeValue<int32_t>(0);
    
    // Number of labels
    auxBinaryStream.writeValue<int32_t>(labelDictionary.size());
    
    // 12 bytes' padding for future versions
    auxBinaryStream.writeValues<int32_t>(0, 3);
    
    // Write out label dictionary
    for (std::map<int,std::string>::const_iterator it=labelDictionary.begin(); it!=labelDictionary.end(); it++)
    {
        const std::pair<int,std::string> &element = *it;
        auxBinaryStream.writeValue<int32_t>(element.first);
        auxBinaryStream.writeString(element.second);
        auxBinaryStream.writeValue<char>(0);
    }
}*/

void TrackvisDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
    if (totalStreamlines > std::numeric_limits<int32_t>::max())
        throw std::runtime_error("Total streamline count exceeds the storage capacity of the Trackvis format");
}
/*
void MedianTrackvisDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    size_t i;
    const_iterator it;
    vector<int> leftLengths(count), rightLengths(count);
    const Streamline::PointType pointType = begin->getPointType();
    
    // First pass: find lengths
    for (it=begin, i=0; it!=end; it++, i++)
    {
        leftLengths[i] = it->getLeftPoints().size();
        rightLengths[i] = it->getRightPoints().size();
        
        if (it->getPointType() != pointType)
            throw std::runtime_error("Point types do not match across streamlines, so median will make no sense");
    }
    
    const int lengthIndex = static_cast<int>(floor((count-1) * quantile));
    const int leftLength = getNthElement(leftLengths, lengthIndex);
    const int rightLength = getNthElement(rightLengths, lengthIndex);
    
    // Second pass: left points
    vector<ImageSpace::Point> leftPoints(leftLength);
    for (int j=0; j<leftLength; j++)
    {
        vector<float> x, y, z;
        
        for (it=begin, i=0; it!=end; it++, i++)
        {
            // Skip over this streamline if it is too short
            if (leftLengths[i] > j)
            {
                const ImageSpace::Point point = it->getLeftPoints()[j];
                x.push_back(point[0]);
                y.push_back(point[1]);
                z.push_back(point[2]);
            }
        }
        
        const int medianIndex = static_cast<int>(round(x.size()/2.0));
        leftPoints[j][0] = getNthElement(x, medianIndex);
        leftPoints[j][1] = getNthElement(y, medianIndex);
        leftPoints[j][2] = getNthElement(z, medianIndex);
    }
    
    // Third pass: right points
    vector<ImageSpace::Point> rightPoints(rightLength);
    for (int j=0; j<rightLength; j++)
    {
        vector<float> x, y, z;
        
        for (it=begin, i=0; it!=end; it++, i++)
        {
            // Skip over this streamline if it is too short
            if (rightLengths[i] > j)
            {
                const ImageSpace::Point point = it->getRightPoints()[j];
                x.push_back(point[0]);
                y.push_back(point[1]);
                z.push_back(point[2]);
            }
        }
        
        const int medianIndex = static_cast<int>(round(x.size()/2.0));
        rightPoints[j][0] = getNthElement(x, medianIndex);
        rightPoints[j][1] = getNthElement(y, medianIndex);
        rightPoints[j][2] = getNthElement(z, medianIndex);
    }
    
    // Fixed spacing won't be preserved in the median
    median = Streamline(leftPoints, rightPoints, pointType, grid.spacings(), false);
}

void LabelledTrackvisDataSink::put (const Streamline &data)
{
    // Write the offset of this streamline into the file
    auxBinaryStream.writeValue<uint64_t>(fileStream.tellp());
    
    writeStreamline(data);
    
    const std::set<int> &labels = data.getLabels();
    auxBinaryStream.writeValue<int32_t>(labels.size());
    for (std::set<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
        auxBinaryStream.writeValue<int32_t>(*it);
}*/

void TrackvisDataSink::done ()
{
    outputStream->seekp(988);
    outputStream.writeValue<int32_t>(totalStreamlines);
}
/*
void LabelledTrackvisDataSink::done ()
{
    TrackvisDataSink::done();
    
    auxFileStream.seekp(12);
    auxBinaryStream.writeValue<int32_t>(totalStreamlines);
}

void MedianTrackvisDataSink::done ()
{
    fileStream.seekp(988);
    binaryStream.writeValue<int32_t>(1);
    
    fileStream.seekp(1000);
    writeStreamline(median);
}*/
