#include <RcppEigen.h>

#include "RNiftiAPI.h"
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

template <typename ElementType>
ElementType getNthElement (std::vector<ElementType> vec, size_t n)
{
    std::nth_element(vec.begin(), vec.begin()+n, vec.end());
    return *(vec.begin()+n);
}

void TrackvisDataSource::readStreamline (Streamline &data)
{
    int32_t nPoints = binaryStream.readValue<int32_t>();
    if (nPoints > 0)
    {
        vector<Space<3>::Point> points;
        int seed = 0;
        for (int32_t i=0; i<nPoints; i++)
        {
            Space<3>::Point point;
            binaryStream.readVector<float>(point, 3);
            // TrackVis indexes from the left edge of each voxel
            points.push_back(point / grid.spacings() - 0.5);
        
            if (nScalars > 0)
                fileStream.seekg(4 * nScalars, ios::cur);
        }
        
        if (seedProperty >= 0)
        {
            fileStream.seekg(4 * seedProperty, ios::cur);
            seed = static_cast<int>(binaryStream.readValue<float>());
        }
        if (nProperties > 0)
            fileStream.seekg(4 * (nProperties-seedProperty-1), ios::cur);
        
        data = Streamline(vector<Space<3>::Point>(points.rend()-seed-1, points.rend()),
                          vector<Space<3>::Point>(points.begin()+seed, points.end()),
                          Streamline::VoxelPointType,
                          grid.spacings(),
                          false);
    }
    else
    {
        if (nProperties > 0)
            fileStream.seekg(4 * nProperties, ios::cur);
    }
    
    currentStreamline++;
}

void TrackvisDataSource::attach (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open((fileStem + ".trk").c_str(), ios::binary);
    
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
    if (binaryStream.readString(6).compare(0,5,"TRACK") != 0)
        throw runtime_error("Trackvis file does not seem to have a valid magic number");
    
    binaryStream.readVector<int16_t>(grid.dimensions());
    binaryStream.readVector<float>(grid.spacings());
    
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
    fileStream.seekg(440, ios::beg);
    binaryStream.readMatrix<float>(grid.transform());
    fileStream.seekg(988, ios::beg);
    totalStreamlines = binaryStream.readValue<int32_t>();
    
    fileStream.seekg(1000);
    currentStreamline = 0;
}

void LabelledTrackvisDataSource::attach (const std::string &fileStem)
{
    TrackvisDataSource::attach(fileStem);
    if (!externalLabelList)
        labelList = new StreamlineLabelList(fileStem);
    if (labelList->size() != totalStreamlines)
        throw runtime_error("Number of streamlines in .trk and .trkl files do not match");
}

void LabelledTrackvisDataSource::get (Streamline &data)
{
    // This increments currentStreamline, so we subtract 1 below
    readStreamline(data);
    data.setLabels(labelList->getLabels(currentStreamline-1));
}

void MedianTrackvisDataSource::get (Streamline &data)
{
    if (seedProperty < 0)
        throw runtime_error("A meaningful median can't be recovered without knowing seed indices");
    
    const Eigen::Array3f voxelDims = grid.spacings();
    vector<int> leftLengths(totalStreamlines), rightLengths(totalStreamlines);
    
    // First pass: find lengths
    for (size_t i=0; i<totalStreamlines; i++)
    {
        const int32_t nPoints = binaryStream.readValue<int32_t>();
        fileStream.seekg(4 * (nPoints*(3+nScalars) + seedProperty), ios::cur);
        leftLengths[i] = static_cast<int>(binaryStream.readValue<float>()) + 1;
        rightLengths[i] = nPoints - leftLengths[i] + 1;
        fileStream.seekg(4 * (nProperties-seedProperty-1), ios::cur);
    }
    
    const int lengthIndex = static_cast<int>(floor((totalStreamlines-1) * quantile));
    const int leftLength = getNthElement(leftLengths, lengthIndex);
    const int rightLength = getNthElement(rightLengths, lengthIndex);
    
    // Second pass: left points
    vector<Space<3>::Point> leftPoints(leftLength);
    for (int j=0; j<leftLength; j++)
    {
        vector<float> x, y, z;
        fileStream.seekg(1000);
        
        for (size_t i=0; i<totalStreamlines; i++)
        {
            // Skip over this streamline if it is too short
            if (leftLengths[i] <= j)
                fileStream.seekg(4 * (1 + (leftLengths[i]+rightLengths[i]-1)*(3+nScalars) + nProperties), ios::cur);
            else
            {
                fileStream.seekg(4 * (1 + (leftLengths[i]-1-j)*(3+nScalars)), ios::cur);
                x.push_back(binaryStream.readValue<float>());
                y.push_back(binaryStream.readValue<float>());
                z.push_back(binaryStream.readValue<float>());
                fileStream.seekg(4 * (nScalars + (j+rightLengths[i]-1)*(3+nScalars) + nProperties), ios::cur);
            }
        }
        
        const int medianIndex = static_cast<int>(round(x.size()/2.0));
        leftPoints[j][0] = getNthElement(x, medianIndex) / voxelDims[0] - 0.5;
        leftPoints[j][1] = getNthElement(y, medianIndex) / voxelDims[1] - 0.5;
        leftPoints[j][2] = getNthElement(z, medianIndex) / voxelDims[2] - 0.5;
    }
    
    // Third pass: right points
    vector<Space<3>::Point> rightPoints(rightLength);
    for (int j=0; j<rightLength; j++)
    {
        vector<float> x, y, z;
        fileStream.seekg(1000);
        
        for (size_t i=0; i<totalStreamlines; i++)
        {
            // Skip over this streamline if it is too short
            if (rightLengths[i] <= j)
                fileStream.seekg(4 * (1 + (leftLengths[i]+rightLengths[i]-1)*(3+nScalars) + nProperties), ios::cur);
            else
            {
                fileStream.seekg(4 * (1 + (leftLengths[i]+j-1)*(3+nScalars)), ios::cur);
                x.push_back(binaryStream.readValue<float>());
                y.push_back(binaryStream.readValue<float>());
                z.push_back(binaryStream.readValue<float>());
                fileStream.seekg(4 * (nScalars + (rightLengths[i]-j-1)*(3+nScalars) + nProperties), ios::cur);
            }
        }
        
        const int medianIndex = static_cast<int>(round(x.size()/2.0));
        rightPoints[j][0] = getNthElement(x, medianIndex) / voxelDims[0] - 0.5;
        rightPoints[j][1] = getNthElement(y, medianIndex) / voxelDims[1] - 0.5;
        rightPoints[j][2] = getNthElement(z, medianIndex) / voxelDims[2] - 0.5;
    }
    
    data = Streamline(leftPoints, rightPoints, Streamline::VoxelPointType, voxelDims, false);
    
    read = true;
}

void BasicTrackvisDataSource::seek (const int n)
{
    if (n < currentStreamline)
        throw std::runtime_error("Cannot seek backwards");
    
    while (currentStreamline < n)
    {
        const int nPoints = binaryStream.readValue<int32_t>();
        fileStream.seekg(4 * ((3+nScalars) * nPoints + nProperties), ios::cur);
        currentStreamline++;
    }
}

void LabelledTrackvisDataSource::seek (const int n)
{
    fileStream.seekg(labelList->getOffset(n));
    currentStreamline = n;
}

void TrackvisDataSink::writeStreamline (const Streamline &data)
{
    int nPoints = data.nPoints();
    Eigen::ArrayX3f points;
    
    data.concatenatePoints(points);
    
    binaryStream.writeValue<int32_t>(nPoints);
    for (int i=0; i<nPoints; i++)
    {
        // TrackVis indexes from the left edge of each voxel
        Eigen::Array3f row;
        if (data.getPointType() == Streamline::VoxelPointType)
            row = (points.row(i) + 0.5) * grid.spacings().transpose();
        else
            row = points.row(i) + (0.5 * grid.spacings().transpose());
        binaryStream.writeVector<float>(row);
    }
    
    // In practice, we should be able to squeeze the seed index into a float, but check
    const size_t seedIndex = data.getSeedIndex();
    if (seedIndex > 16777216)
        Rf_warning("Seed index %lu is not representable exactly as a 32-bit floating point value\n", seedIndex);
    binaryStream.writeValue<float>(seedIndex);
    
    // Store termination reasons
    binaryStream.writeValue(static_cast<float>(data.getLeftTerminationReason()));
    binaryStream.writeValue(static_cast<float>(data.getRightTerminationReason()));
}

void TrackvisDataSink::attach (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    if (append)
    {
        fileStream.open((fileStem + ".trk").c_str(), ios::in | ios::out | ios::binary);
        fileStream.seekg(988, ios::beg);
        int32_t existingStreamlines;
        fileStream.read((char *) &existingStreamlines, sizeof(int32_t));
        totalStreamlines = existingStreamlines;
        fileStream.seekp(0, ios::end);
        return;
    }
    
    fileStream.open((fileStem + ".trk").c_str(), ios::out | ios::binary | ios::trunc);
    
    char magicNumber[6] = { 'T','R','A','C','K','\0' };
    fileStream.seekp(0);
    fileStream.write(magicNumber, 6);
    
    binaryStream.writeVector<int16_t>(grid.dimensions(), 3);
    binaryStream.writeVector<float>(grid.spacings(), 3);
    binaryStream.writeValues<float>(0.0, 3);
    
    binaryStream.writeValue<int16_t>(0);
    binaryStream.writeValues<char>(0, 200);
    binaryStream.writeValue<int16_t>(3);
    fileStream.write("seed", 4);
    binaryStream.writeValues<char>(0, 16);
    fileStream.write("Ltermcode", 9);
    binaryStream.writeValues<char>(0, 11);
    fileStream.write("Rtermcode", 9);
    binaryStream.writeValues<char>(0, 151);
    
    const Eigen::Matrix4f xform = grid.transform();
    binaryStream.writeMatrix<float>(xform);
    binaryStream.writeValues<char>(0, 444);
    
    ::mat44 xformStruct;
    for (int i=0; i<4; i++)
    {
        for (int j=0; j<4; j++)
            xformStruct.m[i][j] = xform(i, j);
    }
    
    int icode, jcode, kcode;
    nifti_mat44_to_orientation(xformStruct, &icode, &jcode, &kcode);
    char orientation[4];
    orientation[0] = TrackvisDataSink::orientationCodeMap[icode];
    orientation[1] = TrackvisDataSink::orientationCodeMap[jcode];
    orientation[2] = TrackvisDataSink::orientationCodeMap[kcode];
    orientation[3] = '\0';
    fileStream.write(orientation, 4);
    binaryStream.writeValues<char>(0, 4);
    
    float qb, qc, qd, qfac;
    nifti_mat44_to_quatern(xformStruct, &qb, &qc, &qd, NULL, NULL, NULL, NULL, NULL, NULL, &qfac);
    ::mat44 rotationMatrix = nifti_quatern_to_mat44(qb, qc, qd, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, qfac);
    binaryStream.writeValue<float>(-rotationMatrix.m[0][0]);
    binaryStream.writeValue<float>(-rotationMatrix.m[0][1]);
    binaryStream.writeValue<float>(rotationMatrix.m[0][2]);
    binaryStream.writeValue<float>(-rotationMatrix.m[1][0]);
    binaryStream.writeValue<float>(-rotationMatrix.m[1][1]);
    binaryStream.writeValue<float>(rotationMatrix.m[1][2]);
    binaryStream.writeValues<char>(0, 8);
    
    binaryStream.writeValue<int32_t>(0);
    binaryStream.writeValue<int32_t>(2);
    binaryStream.writeValue<int32_t>(1000);
    
    totalStreamlines = 0;
}

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
}

void TrackvisDataSink::setup (const size_type &count, const_iterator begin, const_iterator end)
{
    totalStreamlines += count;
    if (totalStreamlines > std::numeric_limits<int32_t>::max())
        throw std::runtime_error("Total streamline count exceeds the storage capacity of the Trackvis format");
}

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
    vector<Space<3>::Point> leftPoints(leftLength);
    for (int j=0; j<leftLength; j++)
    {
        vector<float> x, y, z;
        
        for (it=begin, i=0; it!=end; it++, i++)
        {
            // Skip over this streamline if it is too short
            if (leftLengths[i] > j)
            {
                const Space<3>::Point point = it->getLeftPoints()[j];
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
    vector<Space<3>::Point> rightPoints(rightLength);
    for (int j=0; j<rightLength; j++)
    {
        vector<float> x, y, z;
        
        for (it=begin, i=0; it!=end; it++, i++)
        {
            // Skip over this streamline if it is too short
            if (rightLengths[i] > j)
            {
                const Space<3>::Point point = it->getRightPoints()[j];
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
}

void TrackvisDataSink::done ()
{
    fileStream.seekp(988);
    binaryStream.writeValue<int32_t>(totalStreamlines);
}

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
}

void StreamlineLabelList::read (const std::string &fileStem)
{
    if (fileStream.is_open())
        fileStream.close();
    
    fileStream.open((fileStem + ".trkl").c_str(), ios::binary);
    fileStream.seekg(0);
    if (binaryStream.readString(8).compare("TRKLABEL") != 0)
        throw runtime_error("Track label file does not seem to have a valid magic number");
    
    const int version = binaryStream.readValue<int32_t>();
    binaryStream.swapEndianness(version > 0xffff);
    
    const int nStreamlines = binaryStream.readValue<int32_t>();
    const int nLabels = binaryStream.readValue<int32_t>();
    fileStream.seekg(32);
    
    for (int i=0; i<nLabels; i++)
    {
        binaryStream.readValue<int32_t>();
        binaryStream.readString();
    }
    
    labelList.clear();
    for (int j=0; j<nStreamlines; j++)
    {
        offsetList.push_back(static_cast<size_t>(binaryStream.readValue<uint64_t>()));
        const int currentCount = binaryStream.readValue<int32_t>();
        std::set<int> currentLabels;
        for (int i=0; i<currentCount; i++)
            currentLabels.insert(binaryStream.readValue<int32_t>());
        labelList.push_back(currentLabels);
    }
}

const std::vector<int> StreamlineLabelList::find (const std::vector<int> &labels)
{
    std::vector<int> indices;
    for (int i=0; i<labelList.size(); i++)
    {
        bool allPresent = true;
        for (std::vector<int>::const_iterator it=labels.begin(); it!=labels.end(); it++)
        {
            if (labelList[i].count(*it) == 0)
                allPresent = false;
        }
        
        if (allPresent)
            indices.push_back(i);
    }
    
    return indices;
}
