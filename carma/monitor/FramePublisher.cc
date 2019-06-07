#include "carma/monitor/FramePublisher.h"

#include "carma/corba/corba.h"
#include "carma/corba/Client.h"
#include "carma/monitor/monitorframe.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

namespace { 

const bool debug_( false );

} // namespace < unnamed >

FramePublisher::FramePublisher( const std::string& channelName, 
                                const std::string& publisherName,
                                carma::corba::Client & client ) :
    channelName_( channelName ),
    publisherName_( publisherName ),
    client_( client )
{
    // Nothin'
}

FramePublisher::~FramePublisher( )
{
    // Nothin'
}

void
FramePublisher::dumpFrame( const TransportSubsystemFrame & f )
{
    cout
        << " numSamples=" << f.numSamples
        << " numMonitorPoints=" << f.numMonitorPoints
        << " ssID=" << f.subsystemID
        << " status=" << hex << f.statusFlags << dec
        << " frameCount=" << f.frameCount
        << " num of transported samples "
        << f.monitorValues.dataType.length()
        << " num of single sample points " << f.numSingleSamplePoints
        << endl;
    cout << " metadata size: " << f.monitorValues.dataType.length() << endl;
    cout << " char size: " << f.monitorValues.charValues.length() << endl;
    cout << " short size: " << f.monitorValues.shortValues.length() << endl;
    cout << " long size: " << f.monitorValues.longValues.length() << endl;
    cout << " bool size: " << f.monitorValues.boolValues.length() << endl;
    cout << " float size: " << f.monitorValues.floatValues.length() << endl;
    cout << " double size: " << f.monitorValues.doubleValues.length() << endl;
    cout << " complex size: " << f.monitorValues.complexValues.length() << endl;
    cout << " string size: " << f.monitorValues.stringValues.length() << endl;
    cout << " sn size: " << f.monitorValues.serialNumberValues.length() << endl;
}

void
FramePublisher::dispatchNotification( const TransportSubsystemFrame & frame )
{
    ScopedLogNdc ndc( "FramePublisher::dispatchNotification()" );

    if ( debug_ )
        dumpFrame(frame);

    client_.sendNotification< TransportSubsystemFrame >( channelName_,
                                                         publisherName_,
                                                         frame );
}
