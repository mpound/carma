#include "carma/canbus/SocketCan.h"

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/posixErrors.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/foreach.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <cerrno>
#include <cstdlib>
#include <iostream>
#include <libsocketcan.h>
#include <fstream>
#include <set>
#include <sstream>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/uio.h>
#include <net/if.h>

#include <linux/can.h> // Order dependency - must follow sys inclusions above.
#include <linux/can/raw.h>

using namespace boost::posix_time;
using namespace carma::canbus;
using namespace carma::util;
using namespace std;

namespace {

const Trace::TraceLevel TRACE_CTOR_DTOR( Trace::TRACE3 );
const Trace::TraceLevel TRACE_MSG_HANDLING( Trace::TRACE4 );
const Trace::TraceLevel TRACE_METADATA( Trace::TRACE4 );
const Trace::TraceLevel TRACE_THREADING( Trace::TRACE3 );

union sockaddr_union {
    struct sockaddr_can sa_can;
    struct sockaddr sa;
};

union ancillary_data_union {
    struct timeval * timestamp;
    __u32 * rxQueueOverflows;
    unsigned char * raw;
};

} // namespace < unnamed >

SocketCan::BusInfo::BusInfo( const string & devName, 
                             const busIdType busId,
                             const int writeSocketFd ) :
   name( devName ),
   id( busId ),
   wsfd( writeSocketFd ),
   rxCount( 0 ),
   txCount( 0 ),
   oneMinRxCount( 0 ),
   oneMinTxCount( 0 ),
   lastUpdateMJD( Time::MJD() ),
   lastOneMinUpdateMJD( Time::MJD() ),
   status( )
{
    // Nothing
}

void
SocketCan::BusInfo::updateBusStatus( )
{
    CARMA_CPTRACE( TRACE_METADATA, "Updating bus status for " << name 
                   << " bus " << id << "." );

    // Calculate message rates.
    const double now( Time::MJD() );

    const double halfSecDiffSecs = 
        ( now - lastUpdateMJD ) * Time::SECONDS_PER_DAY;
    const double oneMinDiffSecs = 
        ( now - lastOneMinUpdateMJD ) * Time::SECONDS_PER_DAY;

    status.rxMsgRate = rxCount / halfSecDiffSecs;
    status.txMsgRate = txCount / halfSecDiffSecs;
    rxCount = 0;
    txCount = 0;
    lastUpdateMJD = now;

    if ( oneMinDiffSecs >= 60. ) {
        status.oneMinRxMsgRate = oneMinRxCount / oneMinDiffSecs;
        status.oneMinTxMsgRate = oneMinTxCount / oneMinDiffSecs;
        oneMinRxCount = 0;
        oneMinTxCount = 0;
        lastOneMinUpdateMJD = now;
    }

    // Update the bus state using libsocketcan
    struct can_berr_counter berrs;
    const int result = can_get_berr_counter( name.c_str(), &berrs );
    if ( result == -1 ) 
        throwPosixError( errno, "can_get_berr_counter fail for " + name + ".");

    status.rxErrors = berrs.rxerr;
    status.txErrors = berrs.txerr;

    if ( status.rxErrors == 0 && status.txErrors == 0 ) {
        status.state = NO_ERRORS;
    } else if ( status.rxErrors <= 127 && status.txErrors <= 127 ) {
        status.state = ERROR_ACTIVE;
    } else if ( status.rxErrors >= 127 || ( status.txErrors >= 127 &&
                status.txErrors <= 255 ) ) {
        status.state = ERROR_PASSIVE;
    } else if ( status.txErrors > 255 ) {
        status.state = BUS_OFF;
    } else {
        throw CARMA_ERROR( "Error setting bus state." );
    }

    status.slowMsgsLost = 0; // Socketcan driver has no 'slow' message interface
}

SocketCan::SocketCan( ) :
    fileDescriptors_( ),
    maxFileDescriptor_( ),
    busses_( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan::SocketCan()" );

    using namespace boost::filesystem;

    vector< string > canIfs; // can interfaces

    const string canIfPrefix( "can" );

    // Scan /sys/class/net for any files named 'can#' 
    const path netDir( "/sys/class/net" );
    if ( !exists( netDir ) || !is_directory( netDir ) )
        throw CARMA_ERROR( netDir.string() + 
                           " is either not a directory or non-existant." );

    directory_iterator di( netDir ), diend;
    BOOST_FOREACH( const path & p, make_pair( di, diend ) )
    {
        const string filename = p.filename().string();

        CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan() candidate " + filename );
        
        if ( filename.substr( 0, canIfPrefix.size() ) == canIfPrefix ) 
            canIfs.push_back( filename );
    }
    
    commonConstruction( canIfs );

    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan::SocketCan() - Looking good." );
}

SocketCan::SocketCan( const ::std::vector< ::std::string > & devNames ) :
    fileDescriptors_( ),
    maxFileDescriptor_( -1 ),
    busses_( )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan::SocketCan()" );

    commonConstruction( devNames );
    
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan::SocketCan() - Looking good." );
}

void
SocketCan::commonConstruction( const ::std::vector< ::std::string > & devNames )
{
    if ( devNames.empty() )
        throw CARMA_ERROR( "No interfaces found or specified." );

    BOOST_FOREACH( const string & canDevice, devNames ) 
    {
        CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan() - Adding " + canDevice );

        // Create the socket - note errno is thread local now, no need to save.
        const int sd = ::socket( PF_CAN, SOCK_RAW, CAN_RAW );
        if ( sd == -1 ) {
            throwPosixError( errno, 
                             "Unable to create socket for " + canDevice + "." );
        }

        // Retrieve the 'interface index' using ioctl
        struct ifreq ifr;
        ::strcpy( ifr.ifr_name, canDevice.c_str() );
        if ( ::ioctl( sd, SIOCGIFINDEX, &ifr ) == -1 ) {
            throwPosixError( errno,
                             "Unable to get if index for " + canDevice + "." );
        }

        if ( ifr.ifr_ifindex == 0 ) {
            const string err( "SocketCan does not support the any interface - " 
                              "Use the default constructor instead." ); 
            throw CARMA_ERROR( err );
        }

        // Bind the socket to the local can address
        sockaddr_union addr;
        addr.sa_can.can_family = AF_CAN;
        addr.sa_can.can_ifindex = ifr.ifr_ifindex;

        const int br = ::bind( sd, &addr.sa, sizeof( addr ) );
        if ( br != 0 ) 
            throwPosixError( errno, 
                             "Unable to bind socket for " + canDevice + "." );

        // Set socket options we might want 
        int sockoptresult = 0;
        int loopback = 1;
        sockoptresult = setsockopt( sd, SOL_CAN_RAW, CAN_RAW_LOOPBACK, 
                                    &loopback, sizeof( loopback ) );
        if ( sockoptresult != 0 ) 
            throwPosixError( errno, "Error setting socket option." );

        // Turn drop monitor on.
        const int dropmonitor = 1;
        sockoptresult = setsockopt( sd, SOL_SOCKET, SO_RXQ_OVFL, 
                                    &dropmonitor, sizeof( dropmonitor ) );
        if ( sockoptresult != 0 ) 
            throwPosixError( errno, "Error setting socket option." );
        
        // Turn timestamping on (ms resolution).
        const int timestamp = 1;
        sockoptresult = setsockopt( sd, SOL_SOCKET, SO_TIMESTAMP, 
                                    &timestamp, sizeof( timestamp ) );
        if ( sockoptresult != 0 ) 
            throwPosixError( errno, "Error setting socket option." );

        // Add this socket descriptor to our canonical sd_set.
        fileDescriptors_.push_back( sd );
        if ( sd > maxFileDescriptor_ ) 
            maxFileDescriptor_ = sd;

        // Things are looking good - add this bus to our bus map.
        busIdType busId = parseBusId( canDevice );
        BusInfo bus( canDevice, busId, sd );
        busses_.insert( make_pair( ifr.ifr_ifindex, bus ) );

        busIdToIf_[ busId ] = ifr.ifr_ifindex;
    }

    // Startup thread
    updateThread_ = boost::thread( &SocketCan::updateThread, this );
}

SocketCan::~SocketCan( )
{
    // Terminate the thread.
    updateThread_.interrupt();
    updateThread_.join();

    // Shutdown sockets.
    BOOST_FOREACH( const int sd, fileDescriptors_ ) 
        shutdown( sd, SHUT_RDWR ); 
    
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SocketCan::~SocketCan() - Done!" );
}

carma::canbus::Message
SocketCan::getMessage( ) 
{
    // We implement a timeout in order to check for thread termination.
    struct timeval timeout; 
    ::fd_set readfds;

    int result = 0;

    for ( ; ; ) {
    
        timeout.tv_sec = 0;
        timeout.tv_usec = 50000; // 50ms

        FD_ZERO( &readfds );
        {
            BOOST_FOREACH( int fd, fileDescriptors_ ) {
                FD_SET( fd, &readfds );
            }
        }

        result = select( maxFileDescriptor_ + 1, &readfds, 0, 0, &timeout);

        if ( result < 0 ) {
            if ( errno == EINTR ) 
                continue;
            else 
                throwPosixError( errno, "SocketCan::getMessage() select fail.");
        } else if ( result == 0 ) {
            boost::this_thread::interruption_point();
            carma::util::ThreadQuitTestSelf();
            continue; 
        } else {
            break;
        }
    }

    // Determine which file descriptor is set
    int setfd = -1; 
    BOOST_FOREACH( setfd, fileDescriptors_ )
    {
        // Pick the first fd if multiple are set.
        if ( FD_ISSET( setfd, &readfds ) ) 
            break;
    }

    if ( setfd == -1 )
        throw CARMA_ERROR( "SocketCan::getMessage() - No file descriptor?!");
    
    // Now get the message
    // We use the recvmsg interface in order to have access to ancillary
    // data (queue overflows and timestamps) from the can socket layer. 
    struct sockaddr_can addr;
    char ctrlmsg[ CMSG_SPACE(sizeof(struct timeval)) + 
                  CMSG_SPACE(sizeof(__u32))];
    struct can_frame frame;

    struct msghdr msg;
    struct cmsghdr * cmsg;
    struct iovec iov;
    iov.iov_base = &frame;
    iov.iov_len = sizeof( frame );
    msg.msg_name = &addr;
    msg.msg_namelen = sizeof( addr );
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;
    msg.msg_control = &ctrlmsg;
    msg.msg_controllen = sizeof(ctrlmsg);
    msg.msg_flags = 0;

    // Note I set MSG_DONTWAIT because blocking implies that another thread
    // has serviced this socket.  This class is not threadsafe and thus
    // this implies a threading error by the user.
    ssize_t nbytes = recvmsg( setfd, &msg, MSG_DONTWAIT );

    if ( nbytes == 0 ) 
        throw CARMA_ERROR( "SocketCan::getMessage() - Interface shutdown." );
    else if ( nbytes < 0 ) 
        throwPosixError( errno, "SocketCan::getMessage() - Error." );
    else if ( nbytes < static_cast< ssize_t >( sizeof( struct can_frame ) ) )
        throw CARMA_ERROR( "SocketCan::getMessage() - Incomplete CAN read." );

    // Do we have this interface?  If not, create it and add it to busses_.
    boost::lock_guard< boost::mutex > guard( mutex_ );
    BusInfoMap::iterator busIt = busses_.find( addr.can_ifindex );
    if ( busIt == busses_.end() )
        throw CARMA_ERROR( "SocketCan::getMessage - bus not found." );

    // Check for ancillary data containing timestamp and/or rx queue overflows.
    double mjd( Time::MJD() );
    for ( cmsg = CMSG_FIRSTHDR(&msg); 
          cmsg && ( cmsg->cmsg_level == SOL_SOCKET );
          cmsg = CMSG_NXTHDR( &msg,cmsg ) ) 
    {
        union ancillary_data_union ad;
        ad.raw = CMSG_DATA( cmsg );

        switch ( cmsg->cmsg_type ) {
            case SO_TIMESTAMP:
                mjd = Time::timeval2MJD( *ad.timestamp );
                break;
            case SO_RXQ_OVFL:
                busIt->second.status.fastMsgsLost = *ad.rxQueueOverflows;
                break;
            default:
                break;
        }
    }

    // Copy can message to carma::canbus::Message.
    DataVector data( frame.data, frame.data + frame.can_dlc );

    if ( frame.can_id & ( CAN_ERR_FLAG | CAN_RTR_FLAG ) ) 
        throw CARMA_ERROR( "Received CAN Error Frame!" );
        
    const idType id = frame.can_id & CAN_EFF_MASK;

    carma::canbus::Message canmsg( id, data, busIt->second.id );
    canmsg.setRxMjd( mjd );

    busIt->second.rxCount++;
    busIt->second.oneMinRxCount++;

    return canmsg;
}

void 
SocketCan::postMessage( const carma::canbus::Message & msg,
                        carma::canbus::txPriorityType priority )
{
    // Use course locking for now.  Can refine later if overly contentious.
    boost::lock_guard< boost::mutex > guard( mutex_ );

    // Retrieve the socket interface index for this busId.
    const busIdType busId = msg.getBusId();
    set< int > sds; // Socket descriptors.

    if ( busId != ALL_BUSSES ) {

        const BusIdToInterfaceMap::iterator busIdIt = busIdToIf_.find( busId );
        if ( busIdIt == busIdToIf_.end() ) {
            ostringstream err;
            err << "SocketCan::postMessage - No can interface associated with "
                << "bus " << busId << ".";
            throw CARMA_ERROR( err.str() );
        }

        const BusInfoMap::iterator busInfoIt = busses_.find( busIdIt->second );
        if ( busInfoIt == busses_.end() ) {
            ostringstream err;
            err << "SocketCan::postMessage - No bus info associated with "
                << "interface " << busIdIt->second << ", bus " << busId << ".";
            throw CARMA_ERROR( err.str() );
        }

        sds.insert( busInfoIt->second.wsfd );
        busInfoIt->second.txCount++;
        busInfoIt->second.oneMinTxCount++;

    } else {
        BOOST_FOREACH( BusInfoMap::value_type & val, busses_ ) {
            sds.insert( val.second.wsfd );
            val.second.txCount++;
            val.second.oneMinTxCount++;
        }
    }

    struct can_frame canFrame;
    canFrame.can_id = msg.getId();
    canFrame.can_id &= CAN_EFF_MASK;
    canFrame.can_id |= CAN_EFF_FLAG;
    DataVector data = msg.getData(); 
    canFrame.can_dlc = data.size();
    memcpy( &canFrame.data[0], &data[0], data.size() );

    while ( !sds.empty() ) {
    
        ::fd_set writefds;
        FD_ZERO( &writefds );
        BOOST_FOREACH( int sd, sds ) {
            FD_SET( sd, &writefds );
        }

        int result = 0;
    
        while ( result <= 0 ) {

            result = select( *(sds.rbegin()) + 1, NULL, &writefds, NULL, NULL );

            if ( result < 0 ) {
                if ( errno == EINTR ) 
                    continue;
                else 
                    throwPosixError( errno, 
                                "SocketCan::postMessage() select failed.");
            } else if ( result == 0 ) {
                // Timeout - shouldn't happen but continue anyways.
                continue; 
            } else {
                break;
            }
        }

        set< int > setsds;
        BOOST_FOREACH( int sd, sds ) {
            if ( FD_ISSET( sd, &writefds ) ) {
                result = write( sd, &canFrame, sizeof( struct can_frame ) );
                if ( result <= 0 ) 
                    throwPosixError( errno, "SocketCan::postMessage() fail." );
                setsds.insert( sd ) ;

            }
        }

        BOOST_FOREACH( int setsd, setsds ) {
            sds.erase( setsd );
        }
    }
}

void
SocketCan::echoAll( const bool enable )
{
    const int recv_own_msgs( enable );

    BOOST_FOREACH( const int sd, fileDescriptors_ ) {
        const int sockoptresult = setsockopt( sd, SOL_CAN_RAW, 
                                              CAN_RAW_RECV_OWN_MSGS, 
                                              &recv_own_msgs, 
                                              sizeof( recv_own_msgs ) );
        if ( sockoptresult != 0 ) 
            throwPosixError( errno, "Unable to setsockopt." );
   }
}

busIdType 
SocketCan::parseBusId( const string & canInterfaceName ) 
{
    // Assume can interface is named something like can0 and just
    // parse the first numeric chars on.
    const string busIdString = canInterfaceName.substr( 
            canInterfaceName.find_first_of( "1234567890" ) );

    return ::atoi( busIdString.c_str() );
}

void
SocketCan::updateThread( ) 
try {
    CARMA_CPTRACE( TRACE_THREADING, "SocketCan::updateThread() - Started." );

    // Get the current time at second resolution + 1/2 frame offset
    const ptime start( second_clock::universal_time() + 
                       microseconds( 250000 ) );
    
    time_iterator ti( start, microseconds( 500000 ) );

    while ( !boost::this_thread::interruption_requested() ) {
        boost::this_thread::sleep( *ti );
        ++ti;

        boost::lock_guard< boost::mutex > guard( mutex_ );
        BOOST_FOREACH( BusInfoMap::value_type & busVal, busses_ )
            busVal.second.updateBusStatus();
    }

} catch ( const boost::thread_interrupted & ) {
    CARMA_CPTRACE( TRACE_THREADING, "SocketCan::updateThread() - Clean exit." );
} catch (...) {
    CARMA_CPTRACE( TRACE_THREADING, "SocketCan::updateThread() - Dirty exit." );
    logCaughtAsError( );
}

BusStatusMap
SocketCan::getBusStatus() const
{
	std::map<busIdType, busStatusType> tmp;

    boost::lock_guard< boost::mutex > guard( mutex_ );

    BOOST_FOREACH( const BusInfoMap::value_type & busVal, busses_ )
        tmp[ busVal.second.id ] = busVal.second.status;

    return tmp;
}
