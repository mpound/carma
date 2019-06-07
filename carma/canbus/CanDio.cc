/**@file
 * Definition of carma::canbus::CanDio class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.23 $
 * $Date: 2014/01/30 22:11:15 $
 * $Id: CanDio.cc,v 1.23 2014/01/30 22:11:15 iws Exp $
 *
 */

#include "carma/canbus/CanDio.h"
#include "carma/canbus/JanzCanIo.h"
#include "carma/canbus/JanzDio.h"

#ifdef HAVE_SOCKETCAN
#include "carma/canbus/GpioDio.h"
#include "carma/canbus/SocketCan.h"
#endif

#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Trace.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/foreach.hpp>
#include <set>
#include <unistd.h>
#include <cstdio>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

namespace { // Anonymous namespace for local constants

    const Trace::TraceLevel TRACE_DEVS = Trace::TRACE3;

    // Device maps.  This class was designed exclusively for use with
    // the specialized CARMA Janz CanDio cPCI card.  The device names
    // of the individual components are limited to the following.  They
    // are included here to minimize the number of command line arguments
    // needed by the Master class.  The Master class has one input parameter,
    // namely the Janz cPCI carrier board number (indicated by either a 
    // hex switch or a Rom chip on the Janz cPCI carrier board).  

    // Device map for mttl boards. 
    const char *mttlDevMap[16][2] = {
        {"/dev/mttla_03", "/dev/mttlb_03"},
        {"/dev/mttla_13", "/dev/mttlb_13"},
        {"/dev/mttla_23", "/dev/mttlb_23"},
        {"/dev/mttla_33", "/dev/mttlb_33"},
        {"/dev/mttla_43", "/dev/mttlb_43"},
        {"/dev/mttla_53", "/dev/mttlb_53"},
        {"/dev/mttla_63", "/dev/mttlb_63"},
        {"/dev/mttla_73", "/dev/mttlb_73"},
        {"/dev/mttla_83", "/dev/mttlb_83"},
        {"/dev/mttla_93", "/dev/mttlb_93"},
        {"/dev/mttla_a3", "/dev/mttlb_a3"},
        {"/dev/mttla_b3", "/dev/mttlb_b3"},
        {"/dev/mttla_c3", "/dev/mttlb_c3"},
        {"/dev/mttla_d3", "/dev/mttlb_d3"},
        {"/dev/mttla_e3", "/dev/mttlb_e3"},
        {"/dev/mttla_f3", "/dev/mttlb_f3"}
    };

    // Device map for dpm can modules.
    const char *dpmDevMap[16][2] = {
        {"/dev/dpm_00", "/dev/dpm_01"},
        {"/dev/dpm_10", "/dev/dpm_11"},
        {"/dev/dpm_20", "/dev/dpm_21"},
        {"/dev/dpm_30", "/dev/dpm_31"},
        {"/dev/dpm_40", "/dev/dpm_41"},
        {"/dev/dpm_50", "/dev/dpm_51"},
        {"/dev/dpm_60", "/dev/dpm_61"},
        {"/dev/dpm_70", "/dev/dpm_71"},
        {"/dev/dpm_80", "/dev/dpm_81"},
        {"/dev/dpm_90", "/dev/dpm_91"},
        {"/dev/dpm_a0", "/dev/dpm_a1"},
        {"/dev/dpm_b0", "/dev/dpm_b1"},
        {"/dev/dpm_c0", "/dev/dpm_c1"},
        {"/dev/dpm_d0", "/dev/dpm_d1"},
        {"/dev/dpm_e0", "/dev/dpm_e1"},
        {"/dev/dpm_f0", "/dev/dpm_f1"}
    };

    const long MS_PER_S = 1000;          // Milliseconds per second.
    const long NS_PER_MS = 1000000;      // Nanoseconds per millisecond
    const long DEFAULT_PULSEWIDTH = 250; // Default pulsewidth in ms.

    typedef vector< CanDio::DevTermPair > DevTermPairVec;

    vector< string > 
    extractDioDevNames( const vector< CanDio::DevTermPair > & devTermPairs )
    {
        vector< string > mttlDevNames;

        const DevTermPairVec::const_iterator dtBegin = devTermPairs.begin();
        const DevTermPairVec::const_iterator dtEnd = devTermPairs.end();
        for ( DevTermPairVec::const_iterator dt = dtBegin; dt != dtEnd; ++dt )
        {
            CanDio::BoardSlotPair bm = dt->first; 
            const int boardId = validateModulbusNo( bm.first );
            const int modbusId = validateSlotNo( bm.second );
            const string mttlDevName( mttlDevMap[ boardId ][ modbusId ] );

            mttlDevNames.push_back( mttlDevName );
        }

        return mttlDevNames;
    }

    JanzCanIo::NameTermPairVec
    extractCanNameTermPairs( const vector<CanDio::DevTermPair> & devTermPairs )
    {
        JanzCanIo::NameTermPairVec answer;

        const DevTermPairVec::const_iterator dtBegin = devTermPairs.begin();
        const DevTermPairVec::const_iterator dtEnd = devTermPairs.end();
        for ( DevTermPairVec::const_iterator dt = dtBegin; dt != dtEnd; ++dt )
        {
             const  CanDio::BoardSlotPair bm = dt->first; 
             const int boardId = validateModulbusNo( bm.first );
             const int modbusId = validateSlotNo( bm.second );
             const string canDevName( dpmDevMap[ boardId ][ modbusId ] );
             const bool term = dt->second;

             answer.push_back( make_pair( canDevName, term ) );
         }

         return answer;
     }

    vector< boost::filesystem::path > 
    extractDevicePaths( const string & devNameStr,
                        const boost::filesystem::path & devDirPath )
    {
        CARMA_CPTRACE( TRACE_DEVS, "Extracting device paths..." );

        using namespace boost::filesystem;

        std::vector< path > devPaths;

        if ( !exists( devDirPath ) )
            throw CARMA_ERROR( devDirPath.string() + " does not exist." );
        
        if ( !is_directory( devDirPath ) ) 
            throw CARMA_ERROR( devDirPath.string() + " is not a directory." );

        directory_iterator di( devDirPath ), diend;
        BOOST_FOREACH( const path & p, make_pair( di, diend ) ) 
        {
            const string fn = p.filename().string();
            CARMA_CPTRACE( TRACE_DEVS, "Candidate " + fn );
            if ( fn.size() >= devNameStr.size() && 
                 fn.substr( 0, devNameStr.size() ) == devNameStr ) 
            {
                if ( !is_symlink( p ) )
                    throw CARMA_ERROR( p.string() + " is not a symlink." );

                devPaths.push_back( p );
                CARMA_CPTRACE( Trace::TRACE3, "Extracting " + p.string() );
            }
        }

        return devPaths;
    }
                    

    int 
    extractModulbusNoFromDeviceLink( const boost::filesystem::path & linkPath )
    {
        CARMA_CPTRACE( TRACE_DEVS, "Extracting modulbus no from device link "
                       + linkPath.string() );

        // Read modulbus number from the 'modulbus_number' file given a
        // path to a symlink in the sysfs filesystem of the form:
        // ../../devices/pci0000:01/0000:01:0c.0/janz-ican3.1/net/can1
        // for can, and
        // ../../devices/pci0000:01/0000:01:0d.0/janz-ttl.5/gpio/gpiochip216
        // for gpio.  The file resides at the same level as the enumerated
        // janz-xxx directories.

        using namespace boost::filesystem;

        if ( linkPath.empty() ) 
            throw CARMA_ERROR( "Input path is empty." );

        path symPath = read_symlink( linkPath );
        if ( symPath.empty() ) 
            throw CARMA_ERROR( "Symlink path for " + linkPath.string() + 
                               " is empty." );

        symPath = absolute( symPath, linkPath.parent_path() );

        // Strip off the leading three directories.
        for ( int fn = 0; fn < 3; ++fn ) symPath.remove_filename( );

        // Tack on a modulbus_number filename.
        symPath /= "modulbus_number";
            
        CARMA_CPTRACE( TRACE_DEVS, "Opening " + symPath.string() );

        if ( exists( symPath ) ) {
            std::string modulbus_number_str;
            boost::filesystem::ifstream ifs( symPath );

            if ( !ifs )
                throw CARMA_ERROR( "Unable to open " + symPath.string() );

            ifs >> modulbus_number_str;

            unsigned int modulbus_number = 0;
            if (std::sscanf(modulbus_number_str.c_str(), "%x", &modulbus_number) != 1) {
                std::ostringstream oss;
                oss << "unable to parse modulbus_number_str: " << modulbus_number_str;
                throw CARMA_ERROR(oss.str());
            }

            // Work around buggy Linux driver that doesn't mask off the reserved
            // bits in the modulbus identification register.
            modulbus_number &= 0xf;

            return modulbus_number;
        }

        throw CARMA_ERROR( "Unable to extract modulbus_number given "
                           + linkPath.string() );

    }

    vector< int >
    getGpioBasePins( const int boardId )
    {
        CARMA_CPTRACE( TRACE_DEVS, "Retrieving gpio base pins." );

        using namespace boost::filesystem;

        // Scan /sys/class/gpio directory for symlinks named gpiochip*
        const string gpioChipStr( "gpiochip" );
        const path gpioDirPath( "/sys/class/gpio" );

        std::vector< path > chipPaths = extractDevicePaths( gpioChipStr,
                                                            gpioDirPath );

        // For each discovered gpiochip, extract modbus number and test that
        // it matches input boardId.  If it does, this is our Huckleberry.
        BOOST_FOREACH( const path & p, chipPaths ) 
        {
            const int modulbus_number = extractModulbusNoFromDeviceLink( p );
            if  ( modulbus_number == boardId ) {
                istringstream iss( 
                    p.filename().string().substr( gpioChipStr.size() ) );
                int basePin;
                iss >> basePin;
                if ( iss ) {
                    CARMA_CPTRACE( TRACE_DEVS, "Extracted base pin from " + 
                                   p.filename().string() );
                    vector< int > basePins;
                    basePins.push_back( basePin );
                    basePins.push_back( basePin + 8 );
                    return basePins;
                } else 
                    throw CARMA_ERROR( "Unable to extract basePin from "
                            + p.filename().string() );
            }
        }

        throw CARMA_ERROR( "No gpio for specified Janz board found." ); 
    }

    vector< int >
    getGpioBasePin( const int boardId, const int slotId ) 
    {
        CARMA_CPTRACE( TRACE_DEVS, "Retrieving gpio base pin." );

        const vector< int > basePins( getGpioBasePins( boardId ) ); 

        if ( slotId < 0 || slotId > 1 ) 
            throw CARMA_ERROR( "Invalid slot id." );

        vector< int > basePin;
        basePin.push_back( basePins[ slotId ] );
        return basePin;
    }

    vector< int >
    getGpioBasePins( const vector< CanDio::DevTermPair > & devTermPairs )
    {
        CARMA_CPTRACE( TRACE_DEVS, "Retrieving gpio base pins." );

        vector< int > gpioBasePins;
        BOOST_FOREACH( const CanDio::DevTermPair & dtp, devTermPairs ) {
            vector< int > basePinVec = getGpioBasePin( dtp.first.first, 
                                                       dtp.first.second );
            BOOST_FOREACH( int basePin, basePinVec ) 
                gpioBasePins.push_back( basePin );
        }
        return gpioBasePins;
    }

    vector< string > 
    getSocketCanInterfaces( const int boardId )
    {
        CARMA_CPTRACE( TRACE_DEVS, "CanDio Retrieving socket can interfaces for"
            " board " << boardId << "." );

        using namespace boost::filesystem; 

        // Scan /sys/class/net for can* symlinks.
        const string canInterfaceStr( "can" );
        const path canDirPath( "/sys/class/net" );

        std::vector< path > canPaths = extractDevicePaths( canInterfaceStr,
                                                           canDirPath );
        std::vector< string > canInterfaces; // Order by interface id.
        BOOST_FOREACH( const path & p, canPaths )
        {
            const int modulbus_number = extractModulbusNoFromDeviceLink( p );
            if  ( modulbus_number == boardId ) {
                canInterfaces.push_back( p.filename().string() );
            }
        }

        if ( canInterfaces.size() != 2 )
            throw CARMA_ERROR( "Expected exactly 2 can interfaces for board." );

        sort( canInterfaces.begin(), canInterfaces.end() );

        return canInterfaces;

    }

    vector< string >
    getSocketCanInterface( const int boardId, const int slotId ) 
    {
        CARMA_CPTRACE( TRACE_DEVS, "CanDio Retrieving socket can interface "
            "for board " << boardId << " slot " << slotId << "." );

        const vector< string > canIfs( getSocketCanInterfaces( boardId ) );
        
        if ( canIfs.size() != 2 )
            throw CARMA_ERROR( "Expected exactly 2 can interfaces for board." );

        vector< string > canIf;
        if ( slotId == 0 )
            canIf.push_back( canIfs.at( 0 ) );
        else if ( slotId == 1 )
            canIf.push_back( canIfs.at( 1 ) );
        else
            throw CARMA_ERROR( "Only support slot ids of 0 or 1." );

        return canIf;
    }

    vector< string >
    getSocketCanInterfaces( const vector< CanDio::DevTermPair > & devTermPairs )
    {
        { 
            ostringstream trace;
            trace << "CanDio retrieving socket can interfaces for dev term "
                << "pairs: ";
            BOOST_FOREACH( const CanDio::DevTermPair & dtpair, devTermPairs ) {
                trace << "( " << dtpair.first.first << ", " 
                      << dtpair.first.second << " ) ";
            }
            CARMA_CPTRACE( TRACE_DEVS, trace.str() );
        }

        CARMA_CPTRACE( TRACE_DEVS, "CanDio Retrieving socket can interfaces." );

        vector< string > canIfs;
        BOOST_FOREACH( const CanDio::DevTermPair & dtp, devTermPairs ) {
            vector< string > ifs = getSocketCanInterface( dtp.first.first,
                                                          dtp.first.second );
            BOOST_FOREACH( const string & scif, ifs ) 
                canIfs.push_back( scif );
        }
        
        return canIfs;
    }

} // End namespace <unnamed>

// -----------------------------------------------------------------------------
CanDio::CanDio() :
    pulseWidth_( DEFAULT_PULSEWIDTH ),
    terminate_( true ),
    dio_( new JanzDio() ),
    cio_( new JanzCanIo() )
{
    // Nothing here
}
    
// -----------------------------------------------------------------------------
CanDio::CanDio(int boardId, bool reset, bool terminate) : 
    pulseWidth_( DEFAULT_PULSEWIDTH ),
    terminate_( terminate ),
#ifdef HAVE_SOCKETCAN
    dio_( new GpioDio( getGpioBasePins( boardId ), reset ) ),
    cio_( new SocketCan( getSocketCanInterfaces( boardId ) ) )
#else
	dio_( new JanzDio( mttlDevMap[validateModulbusNo(boardId)][0], 
                       mttlDevMap[validateModulbusNo(boardId)][1], reset ) ),
    cio_( new JanzCanIo( dpmDevMap[validateModulbusNo(boardId)][0], terminate,
                         dpmDevMap[validateModulbusNo(boardId)][1], terminate ))
#endif
{
    // If we're resetting upon startup, then clear the CAN read queue to 
    // clean out any messages retrieved prior to the reset.
    if ( reset )
        cio_->clearReadQueue();
}
	
// -----------------------------------------------------------------------------
CanDio::CanDio(int boardId, int modulbusId, bool reset, bool terminate) :
    pulseWidth_( DEFAULT_PULSEWIDTH ),
    terminate_( terminate ),
#ifdef HAVE_SOCKETCAN
    dio_( new GpioDio( getGpioBasePin( boardId, modulbusId ), reset ) ),
    cio_( new SocketCan( getSocketCanInterface( boardId, modulbusId ) ) )
#else
	dio_( new JanzDio( 
            mttlDevMap[validateModulbusNo(boardId)][validateSlotNo(modulbusId)],
            reset ) ), 
	cio_( new JanzCanIo(
            dpmDevMap[validateModulbusNo(boardId)][validateSlotNo(modulbusId)],
            terminate ) )
#endif
{
  std::cout << "Entering CanDio constructor..." << std::endl;

    // If we're resetting upon startup, then clear the CAN read queue to 
    // clean out any messages retrieved prior to the reset.
    if ( reset ) 
        cio_->clearReadQueue();

    std::cout << "Leaving CanDio constructor..." << std::endl;
}
	  	
// -----------------------------------------------------------------------------
CanDio::CanDio( const vector< DevTermPair > & devTermPairs,
                const bool reset ) :
    pulseWidth_( DEFAULT_PULSEWIDTH ),
    terminate_( ),
#ifdef HAVE_SOCKETCAN
    dio_( new GpioDio( getGpioBasePins( devTermPairs ), reset ) ),
    cio_( new SocketCan( getSocketCanInterfaces( devTermPairs ) ) )
#else
	dio_( new JanzDio( extractDioDevNames( devTermPairs ), reset ) ),
	cio_( new JanzCanIo( extractCanNameTermPairs( devTermPairs ) ) )
#endif
{
    // If we're resetting upon startup, then clear the CAN read queue to 
    // clean out any messages retrieved prior to the reset.
    if ( reset ) 
        cio_->clearReadQueue();
}


// -----------------------------------------------------------------------------
void CanDio::reset() 
{	
    timespec pw, rem;  // Timespec for pulsewidth and remainder.

    pw.tv_sec = static_cast<time_t>(pulseWidth_ / MS_PER_S);
    pw.tv_nsec =static_cast<long>((pulseWidth_ % MS_PER_S) * NS_PER_MS);
    
    // Set reset hi, sleep for pulsewidth, clear read queue and set reset lo.
	dio_->resetHi();
    nanosleep(&pw, &rem);
	cio_->clearReadQueue();
	dio_->resetLo();
}

// -----------------------------------------------------------------------------
bool CanDio::isTerminated()
{
    return terminate_;
}

carma::canbus::Message
CanDio::getMessage( )
{
    return cio_->getMessage();
}

void
CanDio::postMessage( const carma::canbus::Message & msg,
                     carma::canbus::txPriorityType prio )
{
    cio_->postMessage( msg, prio );
}

std::map< busIdType, carma::canbus::busStatusType >
CanDio::getBusStatus( )
{
    return cio_->getBusStatus( );
}
            
void 
CanDio::echoAll( bool enable )
{
    cio_->echoAll( enable );
}

void
CanDio::setTimestampEchoLatency( const int tsLatency, const busIdType busId )
{
    cio_->setTimestampEchoLatency( tsLatency, busId );
}

void
CanDio::queueMessage( const Message & msg )
{
    cio_->queueMessage( msg );
}

void
CanDio::clearReadQueue( )
{
    cio_->clearReadQueue( );
}

