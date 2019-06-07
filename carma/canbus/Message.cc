/** @file
 * Definition of carma::canbus::Message class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.22 $
 * $Date: 2012/09/14 00:14:35 $
 * $Id: Message.cc,v 1.22 2012/09/14 00:14:35 abeard Exp $
 */

#include "carma/canbus/Message.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"

#include <iomanip>

using namespace std;
using namespace carma::canbus;

namespace {

    const idType MAX_CAN_ID = 0x1FFFFFFF;

    idType validateId( const idType id ) 
    {
        if ( id > MAX_CAN_ID ) {
		    throw CARMA_EXCEPTION( carma::canbus::BadParameterException,
                                   "Illegal message id.");
        }

        return id;
    }

    const DataVector & validateDataSize( const DataVector & data ) 
    {
        if ( data.size() > 8 ) {
		    throw CARMA_EXCEPTION( carma::canbus::BadParameterException,
                                   "Data size is > 8.");
        }

        return data;
    }

} // namespace < unnamed >

carma::canbus::Message::Message( )  :
    id_( 0 ),
    busId_( 0 ),
    data_( ),
    dlc_( 0 ),
    simulated_( false ),
    rxMjd_( 0.0 )
{
    // Nothing
}

carma::canbus::Message::Message( const idType id, 
                                 const busIdType busId ) :
    id_( validateId( id ) ),
    busId_( busId ),
    data_( ),
    dlc_( 0 ),
    simulated_( false ),
    rxMjd_( 0.0 )
{
    // Nothing
}

carma::canbus::Message::Message( const idType id, 
                                 const DataVector & data,
                                 const busIdType busId ) :
    id_( validateId( id ) ),
    busId_( busId ),
    dlc_( validateDataSize( data ).size() ),
    simulated_( false ),
    rxMjd_( 0.0 )
{
    memcpy( &data_[0], &data[0], data.size() );
}

carma::canbus::Message::Message( const carma::canbus::Message & other ) :
    id_( other.id_ ),
    busId_( other.busId_ ),
    dlc_( other.dlc_ ),
    simulated_( other.simulated_ ),
    rxMjd_( other.rxMjd_ )
{
    memcpy( &data_[0], &other.data_[0], other.dlc_ );
}

carma::canbus::idType 
carma::canbus::Message::getId( ) const 
{    
    return id_;
}

carma::canbus::busIdType 
carma::canbus::Message::getBusId( ) const 
{
	return busId_;
}

carma::canbus::DataVector
carma::canbus::Message::getData( ) const 
{
    return DataVector( &data_[0], &data_[dlc_] );
}

double 
carma::canbus::Message::getRxMjd() const
{
    return rxMjd_;
}

bool 
carma::canbus::Message::isSimulated() const
{
    return simulated_;
}

void 
carma::canbus::Message::setId( const carma::canbus::idType id ) 
{
    id_ = validateId( id );
}

void 
carma::canbus::Message::setBusId( const busIdType busId ) 
{
	busId_ = busId;
}

void 
carma::canbus::Message::setData( const vector<byteType> & data ) 
{
    validateDataSize( data );
    memcpy( &data_[0], &data[0], data.size() );
    dlc_ = data.size();
}

void 
carma::canbus::Message::setRxMjd( const double mjd )
{
   rxMjd_ = mjd;
} 

void 
carma::canbus::Message::setSimFlag( const bool sim ) 
{
    simulated_ = sim;
}

carma::canbus::Message &
carma::canbus::Message::operator=( const carma::canbus::Message & other ) 
{
    if ( this == &other ) 
        return *this;

    id_ = other.id_;
	busId_ = other.busId_;
    memcpy( &data_[0], &(other.data_[0]), other.dlc_ );
    dlc_ = other.dlc_;
    simulated_ = other.simulated_;
    rxMjd_ = other.rxMjd_;

    return *this;
}

bool 
carma::canbus::Message::operator==( const carma::canbus::Message & other ) 
{
    if ( this == &other ) return true;

    return ( id_ == other.id_ && 
             busId_ == other.busId_ &&
             dlc_ == other.dlc_ &&
             memcmp( &data_, &(other.data_), dlc_ ) == 0 );
}

bool 
carma::canbus::Message::operator!=( const carma::canbus::Message & other ) 
{
	return !(this->operator==(other));
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const unsigned char rvalue )
{
    DataVector data = getData(); 
    uByteToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const unsigned short rvalue )
{
    DataVector data = getData(); 
    uShortToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const short rvalue )
{
    DataVector data = getData(); 
    sShortToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const unsigned long int rvalue )
{
    DataVector data = getData(); 
    uLongToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const long int rvalue )
{
    DataVector data = getData(); 
    sLongToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const float rvalue )
{
    DataVector data = getData(); 
    floatToData( data, rvalue );
    setData( data );
    return *this;
}

carma::canbus::Message & 
carma::canbus::Message::operator<<( const double rvalue )
{
    DataVector data = getData(); 
    doubleToData( data, rvalue );
    setData( data );
    return *this;
}

::std::string 
carma::canbus::Message::dump( const bool abbreviated, const bool ascii ) const
{
    ::std::ostringstream os;
        
    DataVector data = getData();

    if ( !abbreviated ) {
        apiType api;
        nodeType node;
        msgType mid;
        modeType mode = getMode( getId() );
        busIdType busId = getBusId();

        fromId( api, node, mid, getId() );

        os << "Bus " << busId << ", " 
            << (mode == APPLICATION ? "api " : "bt  ") 
            << setw(3) << api << ", "
            << (mode == APPLICATION ? "node " : "sn   ") 
            << setw(3) << node << ", "
            << "mid 0x" << hex << setw(3) << setfill('0') << mid << ", "
            << "to " << ( isToHost(getId()) ? "host.  " : "nodes. ")
            << "Data: ";
    }

    if ( ascii ) {
        os << "'";
        for ( DataVector::iterator i = data.begin(); i != data.end(); ++i )
        {
            const char c = static_cast<char>(*i);
            os << ( ::isprint( c ) ? c : '?' );
        }
        os << "'";
    } else {
        for ( DataVector::iterator i = data.begin(); i != data.end(); ++i )
            os << hex << setw(2) << setfill('0') << static_cast<int>(*i) << " ";
    }

    return os.str();
}
