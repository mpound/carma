#include "carma/correlator/obsRecord2/CorbaCorrConsumerStats.h"

using namespace carma::correlator::obsRecord2;
using namespace std;

namespace {

string
getCorbaObjectName( const string & ncName ) 
{
    const string astroband( "astroband" );
    const string::size_type abpos = ncName.find( astroband );
    const string::size_type abend = ncName.find_last_of( '.' );
    if ( abpos == string::npos ) {
        return ncName.substr( ncName.find_last_of( '.' ) + 1 );
    } else if ( abend != string::npos && abend > abpos ) {
        return ncName.substr( abpos, abend - abpos );
    } else {
        return "error";
    }
}

} // namespace < unnamed >

CorbaCorrConsumerStats::CorbaCorrConsumerStats( ) :
    active( false ),
    notificationChannelName( "NONE" ),
    deserializationErrorCount( 0 ),
    errorOnLastDeserialization( false ),
    assemblyLatencyInMs( 0.0 ),
    transmitLatencyInMs( 0.0 ),
    receiveLatencyInMs( 0.0 ),
    corbaDemarshalingTimeInMs( 0.0 ),
    deserializationTimeInMs( 0.0 ),
    totalProcTimeInMs( 0.0 )
{
    // Deliberately empty
}

CorbaCorrConsumerStats::CorbaCorrConsumerStats( const std::string & ncName ) :
    active( false ),
    notificationChannelName( getCorbaObjectName( ncName ) ),
    deserializationErrorCount( 0 ),
    errorOnLastDeserialization( false ),
    assemblyLatencyInMs( 0.0 ),
    transmitLatencyInMs( 0.0 ),
    receiveLatencyInMs( 0.0 ),
    corbaDemarshalingTimeInMs( 0.0 ),
    deserializationTimeInMs( 0.0 ),
    totalProcTimeInMs( 0.0 )
{
    // Deliberately empty
}

CorbaCorrConsumerStats::CorbaCorrConsumerStats( 
    const CorbaCorrConsumerStats & rhs ) 
{
    if ( this == &rhs ) return;
    this->active = rhs.active;
    this->notificationChannelName = rhs.notificationChannelName;
    this->deserializationErrorCount = rhs.deserializationErrorCount;
    this->errorOnLastDeserialization = rhs.errorOnLastDeserialization;
    this->assemblyLatencyInMs = rhs.assemblyLatencyInMs;
    this->transmitLatencyInMs = rhs.transmitLatencyInMs;
    this->receiveLatencyInMs = rhs.receiveLatencyInMs;
    this->corbaDemarshalingTimeInMs = rhs.corbaDemarshalingTimeInMs;
    this->deserializationTimeInMs = rhs.deserializationTimeInMs;
    this->totalProcTimeInMs = rhs.totalProcTimeInMs;
}

CorbaCorrConsumerStats & 
CorbaCorrConsumerStats::operator=( const CorbaCorrConsumerStats & rhs )
{
    if ( this == &rhs ) return *this;
    this->active = rhs.active;
    this->notificationChannelName = rhs.notificationChannelName;
    this->deserializationErrorCount = rhs.deserializationErrorCount;
    this->errorOnLastDeserialization = rhs.errorOnLastDeserialization;
    this->assemblyLatencyInMs = rhs.assemblyLatencyInMs;
    this->transmitLatencyInMs = rhs.transmitLatencyInMs;
    this->receiveLatencyInMs = rhs.receiveLatencyInMs;
    this->corbaDemarshalingTimeInMs = rhs.corbaDemarshalingTimeInMs;
    this->deserializationTimeInMs = rhs.deserializationTimeInMs;
    this->totalProcTimeInMs = rhs.totalProcTimeInMs;
    return *this;
}
