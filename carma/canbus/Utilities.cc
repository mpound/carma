/** 
 * Definitions for carma::canbus utilities functions.
 *  
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.17 $
 * $Date: 2012/09/05 20:33:19 $
 * $Id: Utilities.cc,v 1.17 2012/09/05 20:33:19 abeard Exp $
 */

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"

#include "carma/util/compileTimeCheck.h"

#include <stdint.h>

using namespace std;
using namespace carma::canbus;
using namespace carma::util;

const long NANOSECS_PER_SEC = 1000000000;
const long NANOSECS_PER_HALF_SEC = 500000000;

// -----------------------------------------------------------------------------
modeType carma::canbus::getMode(idType id) 
{
    return (modeType) ((id >> 27)& 0x00000001);
}

// -----------------------------------------------------------------------------
bool carma::canbus::isToHost(idType id) 
{
    return (bool) ((id >> 28)& 0x00000001);
}

// -----------------------------------------------------------------------------
idType carma::canbus::createId( const bool host, 
                                const apiType api, 
                                const nodeType node, 
                                const msgType mid) 
{
    if ( api > 0xFF ) {
        ostringstream oss;
        oss << "canbus::createId - Api (" << api << ") out of range (0-255).";
        throw CARMA_ERROR( oss.str() );
    }

    if ( node > 0x1FF ) {
        ostringstream oss;
        oss << "canbus::createId - Node (" << node << ") out of range (0-511).";
        throw CARMA_ERROR( oss.str() );
    }

    if ( mid > 0x3FF ) {
        ostringstream oss;
        oss << "canbus::createId - Mid (" << mid << ") out of range (0-1023).";
        throw CARMA_ERROR( oss.str() );
    }
    
    idType tmp = 0x00000000;

    tmp = (host << 28) | (mid << 17) | (api << 9) | node;
    return tmp;
}

// -----------------------------------------------------------------------------
idType carma::canbus::createEngId(bool host, boardType board, 
		serialNumberType serialNumber, msgType mid) 
{
    idType tmp = 0x00000000;

    tmp = (host << 28) | (1 << 27) | (mid << 17) | (board << 9) | serialNumber;
    return tmp;
}

// -----------------------------------------------------------------------------
void carma::canbus::fromId(apiType &api, nodeType &node, msgType &mid, idType id) 
{
	node = (id & 0x01ff);
    api = ((id >> 9) & 0x0ff);
	mid = ((id >> 17) & 0x03ff);
}

// -----------------------------------------------------------------------------
void carma::canbus::fromEngId(boardType &bt, serialNumberType &sn, 
        msgType &mid, idType engId) 
{
	sn = (engId & 0x01ff);
    bt = ((engId >> 9) & 0x0ff);
	mid = ((engId >> 17) & 0x03ff);
}

// -----------------------------------------------------------------------------
void carma::canbus::uByteToData(vector<byteType> &data, unsigned char value) 
{
    if ((data.size() + 1) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "uByteToData() - Resulting data will contain > 8 bytes!");
    }
    data.insert(data.end(), 1, value);
}

// -----------------------------------------------------------------------------
void carma::canbus::uShortToData(vector<byteType> &data, unsigned short value) 
{	
    const unsigned int nBytes = 2;
    const unsigned int ogSize = data.size();  // Original size of vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "uShortToData() - Resulting data will contain > 8 bytes!");
    }
   
    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);
    
    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    
    // Stuff the value into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes -1 - i];
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::uLongToData(vector<byteType> &data, unsigned long value) 
{	
    const unsigned int nBytes = 4;
    const unsigned int ogSize = data.size();  // Original size of vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "uLongToData() - Resulting data will contain > 8 bytes!");
    }
   
    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);
    
    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    
    // Stuff the value into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes -1 - i];
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::sShortToData(vector<byteType> &data, short value) 
{	
    const unsigned int nBytes = 2; 
    const unsigned int ogSize = data.size();  // Original size of vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "sShortToData() - Resulting data will contain > 8 bytes!");
    }
   
    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);
    
    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    
    // Stuff the value into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes -1 - i];
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::sLongToData(vector<byteType> &data, long value) 
{	
    const unsigned int nBytes = 4; 
    const unsigned int ogSize = data.size();  // Original size of vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "sLongToData() - Resulting data will contain > 8 bytes!");
    }
   
    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);
    
    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    
    // Stuff the value into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes -1 - i];
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::floatToData(vector<byteType> &data, float value) 
{
    const unsigned int nBytes = 4;
    const unsigned int ogSize = data.size();  // Original size of data vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "floatToData() - Resulting data will contain > 8 bytes!");
    }

    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);

    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);

    // Stuff the vlue into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes - 1 - i];
    }
}

// -----------------------------------------------------------------------------
void carma::canbus::doubleToData(vector<byteType> &data, double value) 
{
    const unsigned int nBytes = 8;
    const unsigned int ogSize = data.size();  // Original size of data vector.
    if ((ogSize + nBytes) > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "doubleToData() - Resulting data will contain > 8 bytes!");
    }

    // Resize the vector to accomodate new bytes.
    data.resize(ogSize + nBytes);

    // Point to the raw bytes of value
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);

    // Stuff the vlue into the vector in network byte order (opposite Intel).
    for (unsigned int i = 0; i < nBytes; i++) {
        data[(ogSize) + i] = byteptr[nBytes - 1 - i];
    }
}

// -----------------------------------------------------------------------------
unsigned char carma::canbus::dataToUbyte(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( unsigned char ) == 1 >( );

	if (data.size() < 1) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToUbyte() - Data must be at least one byte long!");
    }
    unsigned char value = data[0];
    data.erase(data.begin());
    return value;
}

// -----------------------------------------------------------------------------
unsigned short carma::canbus::dataToUshort(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( unsigned short ) == 2 >( );

	if (data.size() < 2) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToUshort() - Data must be at least 2 bytes long");
    }
    unsigned short value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 2;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return value;
}

// -----------------------------------------------------------------------------
unsigned long carma::canbus::dataToUlong(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( uint32_t ) == 4 >( );
    if (data.size() < 4) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToUlong() - Data must be at least 4 bytes long!");
    }
    uint32_t value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 4;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return static_cast< unsigned long >( value );
}

// -----------------------------------------------------------------------------
short carma::canbus::dataToShort(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( short ) == 2 >( );

	if (data.size() < 2) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToShort() - Data must be at least 2 bytes long");
    }
    short value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 2;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return value;
}

// -----------------------------------------------------------------------------
long carma::canbus::dataToLong(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( int32_t ) == 4 >( );
    
	if (data.size() < 4) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToLong() - Data must be at least 4 bytes long");
    }
    int32_t value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 4;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return static_cast< long >( value );
}

// -----------------------------------------------------------------------------
float carma::canbus::dataToFloat(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( float ) == 4 >( );

	if (data.size() < 4) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToFloat() - Data must be at least 4 bytes long");
    }
    float value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 4;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return value;
}

// -----------------------------------------------------------------------------
double carma::canbus::dataToDouble(vector<byteType> &data) 
{
    compileTimeCheck< sizeof( double ) == 8 >( );

	if (data.size() < 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "dataToDouble() - Data must be at least 8 bytes long");
    }
    double value;
    unsigned char *byteptr = reinterpret_cast<unsigned char *>(&value);
    const unsigned int nBytes = 8;
    for (unsigned int i = 0; i < nBytes; i++) {
	    byteptr[nBytes - 1 - i] = data[i];
    }
    data.erase(data.begin(), data.begin() + nBytes);
    return value;
}

// -----------------------------------------------------------------------------
void carma::canbus::padWithZeros(vector<byteType> &data) 
{
    if (data.size() > 8) {
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "padWithZeros() - Data exceeds 8 bytes.");
    }

	if (data.size() <=  7) {
		data.insert(data.end(), 8 - data.size(), '\000');
	}
}

// -----------------------------------------------------------------------------
timespec carma::canbus::calculateTimeToNextSlowBoundary() 
{
    timespec now, then;
    clock_gettime(CLOCK_REALTIME, &now);

    then.tv_sec = 4 - (now.tv_sec % 5);
    then.tv_nsec = NANOSECS_PER_SEC - 
        (now.tv_nsec % NANOSECS_PER_SEC);
    return then;
}
    
// -----------------------------------------------------------------------------
timespec carma::canbus::calculateTimeToNextHalfSec()
{
    timespec now, then;
    clock_gettime(CLOCK_REALTIME, &now);
    
    then.tv_sec = 0;
    then.tv_nsec = NANOSECS_PER_HALF_SEC - 
        (now.tv_nsec % NANOSECS_PER_HALF_SEC);
    return then;
}

// -----------------------------------------------------------------------------
busIdType carma::canbus::extractBusId(string filename)
{
    // This routine assumes a filename format of dpm_XX where 
    // XX is the Modulbus number and slot of the device.  The
    // modulbus number must be in the range 0x00 - 0x0f and the
    // slot must be either a 0 or a 1.  Anything else is an 
    // exceptional situation.
    busIdType busId = 0;
    int d1, d2;  // First and second digits in name.
    string tmp;
    string::size_type i = filename.find_last_of("_");
    
    if (i != string::npos) {
        // Extract last charachters after '_'
        tmp = filename.substr(i+1);
    } else {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
                "extractBusId() - Invalid filename, "
                "filename must be of the format dpm_XX where XX is the "
                "modulbus number (0x0 - 0xf) and slot (0 or 1).");
    }
    
    if (tmp.size() == 2) {
        // Check that the first digit is in range 0-f and second 0-1.
        if ((tmp.substr(0, 1).find_first_of("0123456789abcdef") == string::npos)
            || (tmp.substr(1, 1).find_first_of("01") == string::npos)) {
            throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
                "extractBusId() - Invalid filename, "
                "filename must be of the format dpm_XX where XX is the "
                "modulbus number (0x0 - 0xf) and slot (0 or 1).");
        }
                
        // Extract the first digit...
        if (tmp.find_first_of("abcdef") != string::npos) {
            // First digit is hex, convert it to a dec integer.
            // Ascii character set definition allows this (see man ascii)
            d1 = (tmp[0] - 'a') + 10;
            d2 = (tmp[1] - '0');
        } else {
            d1 = (tmp[0] - '0');
            d2 = (tmp[1] - '0');
        }
            
        busId = (d1 * 2) + d2;
    } else {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
                "extractBusId() - Invalid filename, "
                "filename must be of the format dpm_XX where XX is the "
                "modulbus number (0x0 - 0xf) and slot (0 or 1).");
    }
        
    return busId;
}

// -----------------------------------------------------------------------------
int carma::canbus::validateModulbusNo(int modulbusNo) {
    if (modulbusNo > 15 || modulbusNo < 0) {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Invalid modulbusNo, must be in range "
            "of 0-15 (0x0 - 0xf).");
    }
    return modulbusNo;
}

// -----------------------------------------------------------------------------
int carma::canbus::validateSlotNo(int slotNo) {
    if (slotNo > 1 || slotNo < 0) {
        throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Invalid slot number, must be either 0 or 1.");
    }
    return slotNo;
}
