/** @file
 * Definition of carma::canbus::JanzDio class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2014/09/10 16:13:35 $
 * $Id: JanzDio.cc,v 1.3 2014/09/10 16:13:35 iws Exp $
 */
#include "carma/canbus/JanzDio.h"
#include "carma/util/posixErrors.h"

#include <boost/thread/locks.hpp>
#include <boost/foreach.hpp>
#include <cerrno>
#include <fcntl.h>
#include <janz/mttl.h>
#include <unistd.h>

using namespace boost;
using namespace carma::canbus;
using namespace carma::util;
using namespace std;

namespace {

    // Function to pin mapping.
    const int    RESERVED	= ~0x01;
    const int    RESET 	    = ~0x02;
    const int    POWER		= ~0x04;
    const int    CLEAR		= ~0x00;

} // namespace < unnamemd >

JanzDio::JanzDio( ) :
    devs_( ),
    emulate_( true ),
    mutex_( )
{
    const string devString( "/dev/null" );
    initializeDevice( devString, O_WRONLY, false );
}

JanzDio::JanzDio( const std::string & dev, bool resetOnStart ) :
    devs_( ),
    emulate_(false),
    mutex_( )
{
    initializeDevice( dev, O_RDONLY, resetOnStart );
}

JanzDio::JanzDio( const std::string & dev0,
          const std::string & dev1,
          bool resetOnStart ) :
    devs_( ),
    emulate_(false),
    mutex_( )
{
    initializeDevice( dev0, O_RDONLY, resetOnStart );
    initializeDevice( dev1, O_RDONLY, resetOnStart );
}

JanzDio::JanzDio( const vector< string > & devNames, const bool resetOnStart ) :
    devs_( ),
    emulate_(false),
    mutex_( )
{
    BOOST_FOREACH( const string & devName, devNames ) {
        initializeDevice( devName, O_RDONLY, resetOnStart );
    }
}

JanzDio::~JanzDio()
{
    BOOST_FOREACH( const DeviceInfo & dev, devs_ ) {
        close( dev.fd );
        // Ignore return value
   }
}

void
JanzDio::initializeDevice( const string & deviceName,
                       const int flags,
                       const bool resetOnStart )
{
    int fd;
    DeviceInfo mttldev;
    mttldev.name = deviceName;

    fd = open( mttldev.name.c_str(), flags, 0 );

    if ( fd < 0 ) {
        throwPosixError( errno,
                         "JanzDio::JanzDio() - Opening " + deviceName + "." );
    }

    mttldev.fd = fd;

    mttlWrite( fd, CLEAR );
    mttldev.state = CLEAR;

    devs_.push_back( mttldev );

    // If instructed to reset on startup, drive reset high.
    if ( resetOnStart ) {
        resetHi( );
    }
}

void
JanzDio::mttlWrite( int fd, int data )
{
    int status;
    if ( !emulate_ ) {
        status = mttl_write( fd, data );
    } else {
        status = write( fd, &data, sizeof( data ) );
    }

    if ( status < 0 ) {
        throwPosixError( errno, "JanzDio::mttlWrite() Failed." );
    }
}

void
JanzDio::clear( )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );

    BOOST_FOREACH( DeviceInfo & dev, devs_ ) {
        dev.state = CLEAR;
        mttlWrite( dev.fd, dev.state );
	}
}

void
JanzDio::set( const int bit )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );

    BOOST_FOREACH( DeviceInfo & dev, devs_ ) {
        dev.state &= bit;
        mttlWrite( dev.fd, dev.state );
	}
}

void
JanzDio::unset( const int bit )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );

    BOOST_FOREACH( DeviceInfo & dev, devs_ ) {
        dev.state |= ~bit;
        mttlWrite( dev.fd, dev.state );
	}
}

void
JanzDio::powerOn()
{
    set( POWER );
}

void
JanzDio::powerOff()
{
    unset( POWER );
}

void
JanzDio::resetHi()
{
    set( RESET );
}

void
JanzDio::resetLo()
{
    unset( RESET );
}

void
JanzDio::reservedHi()
{
    set( RESERVED );
}

void
JanzDio::reservedLo()
{
    unset( RESERVED );
}
