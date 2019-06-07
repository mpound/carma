#include "carma/canbus/CanIo.h"

#include <map>

using namespace carma::canbus;
using namespace std;

CanIo::~CanIo( )
{
    // Nothing
}

BusStatusMap
CanIo::getBusStatus( ) const
{
   BusStatusMap empty;
    return empty;
}

void 
CanIo::echoAll( bool enable )
{
    // Nothing 
}

void 
CanIo::clearReadQueue( )
{
    // Nothing
}

void 
CanIo::setTimestampEchoLatency( int tsLatency, busIdType busId )
{
    // Nothing
}

void
CanIo::queueMessage( const Message & msg )
{
    // Nothing
}
