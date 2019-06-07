/** @file
 * Definition of carma::canbus::Message class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2012/07/13 18:55:26 $
 * $Id: JanzMessage.cc,v 1.1 2012/07/13 18:55:26 abeard Exp $
 */

#include "carma/canbus/JanzMessage.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"

#include <iomanip>

using namespace std;
using namespace carma::canbus;

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage(
		carma::canbus::JanzMessage::messageTypeType msgType ) 
{
	vector<byteType> data;
	setMessageType(msgType);
	setId(0);
	setData(data);
	busId_ = 0;
    rxMjd_ = 0.0;
    simulated_ = false;
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage(idType id, busIdType busId) 
{
	vector<byteType> data;
	setMessageType(EXTENDED);
	setId(id);
	setData(data);
	busId_ = busId;
    rxMjd_ = 0.0;
    simulated_ = false;
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage(idType id, 
		const vector<byteType> &data, busIdType busId) 
{
	setMessageType(EXTENDED);
	setId(id);
	setData(data);
	busId_ = busId;
    rxMjd_ = 0.0;
    simulated_ = false;
   
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage(::FastMessage fmsg, busIdType busId) 
{
	rawMsg_ = fmsg;
	busId_ = busId;
    rxMjd_ = 0.0;
    simulated_ = false;
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage(const carma::canbus::JanzMessage &other) 
{
    rawMsg_ = other.rawMsg_;
	busId_ = other.busId_;
    rxMjd_ = other.rxMjd_;
    simulated_ = other.simulated_;
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::JanzMessage( const carma::canbus::Message & other ) 
{
	setMessageType(EXTENDED);
    setId( other.getId() );
    setData( other.getData() );
	busId_ = other.getBusId();
    rxMjd_ = other.getRxMjd();
    simulated_ = other.isSimulated();
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage::messageTypeType 
carma::canbus::JanzMessage::getMessageType() const 
{
	return (messageTypeType)(rawMsg_.cmd & 0x0f);
}

// -----------------------------------------------------------------------------
carma::canbus::idType carma::canbus::JanzMessage::getId() const 
{    
	messageTypeType msgType = getMessageType();
	idType id = 0;
	
	switch (msgType) {
		case STANDARD :
			id  = rawMsg_.data[0] << 3;
			id += rawMsg_.data[1] >> 5;
			break;
		case EXTENDED :
			id  = rawMsg_.data[2] << 21;
			id += rawMsg_.data[3] << 13;
			id += rawMsg_.data[4] << 5;
			id += rawMsg_.data[5] >> 3;
			break;
		default:
			break;
	}
	return id;
}
			
// -----------------------------------------------------------------------------
carma::canbus::busIdType carma::canbus::JanzMessage::getBusId() const 
{
	return busId_;
}

// -----------------------------------------------------------------------------
vector<carma::canbus::byteType> carma::canbus::JanzMessage::getData() const 
{
	messageTypeType msgType = getMessageType();
	int offset = 0; // Offset to the start of the data bytes
	
	switch (msgType) {
		case STANDARD:
			offset += 2;
			break;
		case EXTENDED:
			offset += 6;
			break;
		default:
			break;
	}

    // TODO: My guess is that there is some sort of automatic type conversion
    // occurring here which converts the const char  * into iterators.  
    // This can all be much more efficient.
    const char *memoryIn = (const char*)(&(rawMsg_.data[offset]));
    return vector<byteType>(memoryIn, memoryIn+getDataLength());
}

// -----------------------------------------------------------------------------
carma::canbus::portType carma::canbus::JanzMessage::getPort() const 
{
	portType port = 0;
	port = (rawMsg_.cmd>>4)&0x07;
	return port;
}
	
// -----------------------------------------------------------------------------
::FastMessage carma::canbus::JanzMessage::getRawMessage() const 
{
	return rawMsg_;
}

// -----------------------------------------------------------------------------
double carma::canbus::JanzMessage::getRxMjd() const
{
    return rxMjd_;
}

// -----------------------------------------------------------------------------
bool carma::canbus::JanzMessage::isSimulated() const
{
    return simulated_;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setId(carma::canbus::idType id) 
{
	bool valid = validateId(id);
	messageTypeType msgType = getMessageType();
    
	if (!valid) {
		throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
            "Message;:setId() - Illegal message id.");
    }
	
	switch (msgType) {
		case STANDARD :
			rawMsg_.data[0] = id >> 3;
			rawMsg_.data[1] = ((id & 0x07) << 5);
			break;
		case EXTENDED :
			rawMsg_.data[0] = (1 << 7);
			rawMsg_.data[1] = 0;
			rawMsg_.data[2] = id >> 21;
			rawMsg_.data[3] = (id >> 13) & 0x000000ff;
			rawMsg_.data[4] = (id >> 5) & 0x000000ff;
			rawMsg_.data[5] = (id & 0x000001f) << 3;
			break;
		default:
			break;
	}
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setBusId(busIdType busId) 
{
	busId_ = busId;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setData(const vector<byteType> &data) 
{
	short length = data.size();             // Length of data packet
    int offset = 0;                         // Offset within FastMessage
	messageTypeType msgType = getMessageType(); 
	
    if (length > 8) {
		throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "Message::setData() - invalid data length.");
    }
	
    switch (msgType) {
		case STANDARD :
			offset = 2;
			// Set the new data length (DLC)
			rawMsg_.data[1] = (rawMsg_.data[1] & 0xf0) + length;
			break;
		case EXTENDED:
			offset += 6;
			// Set the new data length (DLC)
			rawMsg_.data[0] = (rawMsg_.data[0] & 0xf0) + length;
			break;
		default:
			break;
	}
	
    // Set the data.
	// If the dataLength is 0 the data pointer might be null.
    if (length > 0) {
		for (int i = 0; i < length; i++) {
			rawMsg_.data[offset + i] = data[i];
		}
	}
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setPort(portType port) 
{
	switch (port) {
		case 0:
			rawMsg_.cmd &= 0x8f;
			break;
		case 1:
			rawMsg_.cmd |= 0x10;
			break;
		default:
			throw CARMA_EXCEPTION(carma::canbus::BadParameterException,
                "Message::setPort() - Undefined port.");
			break;
	}
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setRxMjd(double mjd)
{
   rxMjd_ = mjd;
} 

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setSimFlag(bool sim) 
{
    simulated_ = sim;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::disableTxRetry()
{
   rawMsg_.data[1] = (0x1<<1) | rawMsg_.data[1];
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::enableEcho()
{
    rawMsg_.data[1] = (0x1<<4) | rawMsg_.data[1];
}

// -----------------------------------------------------------------------------
carma::canbus::JanzMessage &carma::canbus::JanzMessage::operator=(
		const carma::canbus::JanzMessage &other) 
{
    if (this != &other) {
		rawMsg_ = other.rawMsg_;
		busId_ = other.busId_;
        rxMjd_ = other.rxMjd_;
        simulated_ = other.simulated_;
    }
    return *this;
}

// -----------------------------------------------------------------------------
bool carma::canbus::JanzMessage::operator==( 
    const carma::canbus::JanzMessage & other ) const
{
    if (getMessageType() != other.getMessageType() ||
            getId() != other.getId()               ||
            getBusId() != other.getBusId()         ||
            getData() != other.getData()           ||
            getRxMjd() != other.getRxMjd()) {
        return false;
    } else {
        return true;
    }
}

// -----------------------------------------------------------------------------
bool carma::canbus::JanzMessage::operator!=( 
    const carma::canbus::JanzMessage & other ) const
{
    return !(this->operator==(other));
}

// -----------------------------------------------------------------------------
int carma::canbus::JanzMessage::getDataLength() const 
{	
	messageTypeType msgType = getMessageType();
	int length = 0;	

	switch (msgType) {
		case STANDARD :
			length = rawMsg_.data[1]&0x0f;
			break;
		case EXTENDED:
			length = rawMsg_.data[0]&0x0f;
			break;
		default:
			break;
	}
	return length;
}

// -----------------------------------------------------------------------------
void carma::canbus::JanzMessage::setMessageType(
		carma::canbus::JanzMessage::messageTypeType messageType) 
{	
	switch (messageType) {
		case STANDARD:
			rawMsg_.cmd = 0x00;
			break;
		case EXTENDED:
			rawMsg_.cmd = 0x01;
			rawMsg_.data[0] |= 0x70;
			break;
		default:
			break;
	}
}
	
// -----------------------------------------------------------------------------
bool carma::canbus::JanzMessage::validateId(carma::canbus::idType id) const 
{
    bool result = false;
	messageTypeType msgType = getMessageType();

	switch (msgType) {
		case EXTENDED :
			result = (id <= MAX_EXTENDED_ID);
			break;
		case STANDARD :
			result = (id <= MAX_STANDARD_ID);
			break;
		default:
			break;
	}
    return result;
}
