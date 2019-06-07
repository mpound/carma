
#include "carma/canbus/GpioDio.h"
#include "carma/util/ErrorException.h"
#include "carma/util/posixErrors.h"
#include "carma/util/Trace.h"

#include <boost/thread/locks.hpp>
#include <boost/foreach.hpp>
#include <cerrno>
#include <fcntl.h>
#include <unistd.h>

using namespace boost;
using namespace carma::canbus;
using namespace carma::util;
using namespace std;

namespace {

    // Function to pin mapping.
    const unsigned char RESERVED   = ~0x01;
    const unsigned char RESET      = ~0x02;
    const unsigned char POWER      = ~0x04;
    const unsigned char CLEAR      = ~0x00;

    const Trace::TraceLevel TRACE_CTOR_DTOR( Trace::TRACE3 );
    const Trace::TraceLevel TRACE_GPIO( Trace::TRACE5 );

} // namespace < unnamed >

GpioDio::GpioDio( ) :
    state_( 0x00 ),
    gpioDevs_( ),
    mutex_( ),
    emulate_( true )
{
    GpioInfo info;

    info.basePin = -1;

    const int fd = ::open( "/dev/null", O_WRONLY );
    if ( fd < 0 ) {
        throwPosixError( errno, "Unable to open /dev/null" );
    }

    for ( int pin = 0; pin < 8; ++pin ) info.fds.push_back( fd );

    gpioDevs_.push_back( info );

    clear();
}

GpioDio::GpioDio( vector< int > basePins, const bool resetOnStart ) :
    state_( 0x00 ),
    gpioDevs_( ),
    mutex_( ),
    emulate_( false )
{
    BOOST_FOREACH( const int basePin, basePins )
    {
        GpioInfo info;
        info.basePin = basePin;

        // Now open 8 contiguous pins starting with the base pin.
        // These must already be 'exported' in gpio parlance.
        for ( int pin = 0; pin < 8; ++pin ) {
            ostringstream pinss;
            pinss << "/sys/class/gpio/gpio" << basePin + pin << "/value";
            const int fd = ::open( pinss.str().c_str(), O_RDWR );
            if ( fd < 0 ) {
                throwPosixError( errno, "Unable to open " + pinss.str() );
            }
            CARMA_CPTRACE( TRACE_CTOR_DTOR, "GpioDio() Opened " + pinss.str() );
            info.fds.push_back( fd );
        }

        gpioDevs_.push_back( info );
    }

    clear();

    if ( resetOnStart )
        resetHi( );
}

GpioDio::~GpioDio( )
{
    try {

        if ( emulate_ ) {
            ::close( *( gpioDevs_.begin()->fds.begin() ) );
        } else {
            BOOST_FOREACH( const GpioInfo & info, gpioDevs_ ) {
                BOOST_FOREACH( const int fd, info.fds ) {
                    ::close( fd );
                }
            }
        }

    } catch (...) {
        // Stifle
    }
}

void
GpioDio::writeHoldingLock( )
{
    BOOST_FOREACH( const GpioInfo & info, gpioDevs_ ) {

        if ( info.fds.size() != 8 )
            throw CARMA_ERROR( "Expected one file descriptor per pin." );

        for ( int pin = 0; pin < 8; ++pin ) {
            // The value needs to be written out as ascii.
            int val = ( state_ >> pin ) & 0x01;
            ostringstream valss;
            valss << val << "\n";
            string valString( valss.str() );

            CARMA_CPTRACE( TRACE_GPIO, "GpioDio writing " << valString <<
                           " to " << info.basePin << " pin " << pin <<
                           " @ fd " << info.fds[pin] );

            const int result = ::write( info.fds[pin], valString.c_str(),
                                        valString.size() );

            if ( result == 0 )
                throw CARMA_ERROR( "Nothing written!" );
            else if ( result < 0 )
                throwPosixError( errno, "GpioDio::writeWord - write failed" );
        }
    }
}

void
GpioDio::clear( )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );
    state_ = CLEAR;
    writeHoldingLock( );
}

void
GpioDio::set( unsigned char bits )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );
    state_ &= bits;
    writeHoldingLock( );
}

void
GpioDio::unset( unsigned char bits )
{
    boost::lock_guard< boost::mutex > guard( mutex_ );
    state_ |= ~bits;
    writeHoldingLock( );
}

void
GpioDio::powerOn( )
{
    set( POWER );
}

void
GpioDio::powerOff()
{
    unset( POWER );
}

    void
GpioDio::resetHi()
{
    set( RESET );
}

    void
GpioDio::resetLo()
{
    unset( RESET );
}

    void
GpioDio::reservedHi()
{
    set( RESERVED );
}

    void
GpioDio::reservedLo()
{
    unset( RESERVED );
}
