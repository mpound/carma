/** @file
 * Application for reading packets off of a CANbus - Based on Colby's 
 * original application which used carma::canbus::DirectCan.  
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2012/08/03 23:10:10 $
 * $Id: cansniffer.cc,v 1.5 2012/08/03 23:10:10 abeard Exp $
 */

#ifdef HAVE_SOCKETCAN
#include "carma/canbus/SocketCan.h"
#else
#include "carma/canbus/DirectCan.h"
#include "carma/canbus/JanzCanIo.h"
#endif

#include "carma/canbus/InetCan.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <iomanip>
#include <iostream>
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>
#include <memory>
#include <sstream>

using namespace std;
using namespace carma::util;
using namespace carma::canbus;
using namespace log4cpp;

typedef enum {
#ifdef HAVE_SOCKETCAN
    SOCKETCAN, // Use socket can framework
#else
    EXCLUSIVE, // For exclusive access to a single Janz CAN card (e.g. dpm_00).
    DIRECT, // Use DirectCan class - can sniff all busses on the machine.
#endif
    REMOTE, // To sniff remotely using CAN-Over-IP.
    DRY     // Just print acceptance filter 
} ModeType;

typedef struct {
    int host;
    int eng;
    int api;
    int node;
    int mid;
} CanAddressType;

void 
exitWithUsage( )
{
    cerr << "Usage: " << Program::getProgram( ).getUsageString( ) << endl
         << endl << "See \'" << Program::getProgram( ).getArg0( ) 
         << " --description\' and \'" 
         << Program::getProgram( ).getArg0( ) << " --keywords\'" 
         << " for more information." << endl;
    exit( 1 );
}

bool 
parameterWasSpecified( const ::std::string & param ) 
{
    return Program::getProgram( ).parameterWasSpecified( param );
}
    
int
getIntParameter( const ::std::string & param )
{
    return Program::getProgram( ).getIntParameter( param );
}

bool
getBoolParameter( const ::std::string & param )
{
    return Program::getProgram( ).getBoolParameter( param );
}

::std::string
getStringParameter( const ::std::string & param )
{
    return Program::getProgram( ).getStringParameter( param );
}

ModeType 
determineMode( ) 
{
#ifdef HAVE_SOCKETCAN
    ModeType mode = SOCKETCAN;
#else
    ModeType mode = DIRECT; 
#endif

    if ( getBoolParameter( "dry" ) ) {
        mode = DRY;
    } 
    else if ( parameterWasSpecified( "board" ) && 
            !parameterWasSpecified( "server" ) ) 
    {
        if ( parameterWasSpecified( "canbus" ) )
        {
#ifdef HAVE_SOCKETCAN
            mode = SOCKETCAN;
#else
            mode = EXCLUSIVE;
#endif
        }
        else
        {
            exitWithUsage( );
        }
    } 
    else if ( !parameterWasSpecified( "board" ) &&
              parameterWasSpecified( "server" ) ) 
    {
        mode = REMOTE;
    }
    else if ( parameterWasSpecified( "board" ) &&
              parameterWasSpecified( "server" ) ) 
    {
        exitWithUsage( );
    } 
    else
    {
#ifdef HAVE_SOCKETCAN
        mode = SOCKETCAN;
#else
        mode = DIRECT;
#endif
    }

    return mode;
}
    
bool 
addressParametersValid( const CanAddressType & address )
{
    // Verify that message addressing parameters are within range.
    return ( ( address.host >= -1 && address.host <= 1 ) &&
             ( address.eng >= -1 && address.eng <= 1 ) &&  
             ( address.api >= 0 && address.api <= 255 ) &&
             ( address.node >= 0 && address.node <= 511 ) && 
             ( address.mid >= -1 && address.mid <= 1023 ) );
}

bool
canbusParameterValid( const ModeType mode, 
                      const int canbus )
{
    // Verify that the canbus parameter is valid.  Note this command line 
    // option is interpreted differently depending on the mode.
    switch ( mode ) {
#ifdef HAVE_SOCKETCAN
        case SOCKETCAN:
            return canbus == 0 || canbus == 1 || canbus == ALL_BUSSES;
#else
        case EXCLUSIVE:
            return ( canbus == 0 || canbus == 1 );
        case DIRECT:
#endif
        case REMOTE:
        default:
            return ( ( canbus >= 0 && canbus <= 31 ) || 
                     ( canbus == ALL_BUSSES ) );
    }
    return false;
}
 
unsigned int
calculateAcceptanceCode( const CanAddressType & address )
{
    unsigned int code = 0x00000000;

    code = ( address.host != -1 ? ( address.host << 28 ) : 0 ) |
           ( address.eng != -1 ? ( address.eng << 27 ) : 0 ) |
           ( address.mid != -1 ? ( address.mid << 17 ) : 0 ) |
           ( address.api != 0 ? ( address.api << 9 ) : 0 ) |
           ( address.node != 0 ? ( address.node ) : 0 );

    return code;
}

unsigned int
calculateAcceptanceMask( const CanAddressType & address )
{
    unsigned int mask = 0x00000000;

    mask = ( address.host == -1 ? ( 1 << 28 ) : 0 ) |
           ( address.eng ==  -1 ? ( 1 << 27 ) : 0 ) |
           ( address.mid == -1 ? ( 0x3ff << 17 ) : 0 ) | 
           ( address.api == 0 ? ( 0xff << 9 ) : 0 ) |
           ( address.node == 0 ? ( 0x1ff ) : 0 );

    return mask;
}

void
coutAcceptanceFilter( const CanAddressType & address, const int canbus )
{
    unsigned int mask = calculateAcceptanceMask( address );
    unsigned int code = calculateAcceptanceCode( address );
    cout << "Acceptance mask 0x" << hex << mask << dec << endl
         << "Acceptance code 0x" << hex << setw(8) << setfill('0') 
         << code << dec << endl 
         << "Bus code 0x" << hex << canbus << dec << endl;
}

CanIo *
createCanIo( const CanAddressType & address, 
             const int canbus,
             const ModeType mode )
{
    CanIo * can = 0;

    switch ( mode ) {
#ifdef HAVE_SOCKETCAN
        case SOCKETCAN:
            can = new SocketCan( );
            break;
#else
        case EXCLUSIVE:
            {
                const int board = getIntParameter( "board" );
                const bool term = getBoolParameter( "term" );
                if ( ( board>= 0 && board <= 0xf ) ) 
                { 
                    ostringstream os;
                    os << "/dev/dpm_" << hex << board 
                        << dec << canbus << ends;
                    can = new JanzCanIo( os.str( ).c_str( ), term );
                } 
                else 
                {
                    exitWithUsage( );
                }
            }
            break;
        case DIRECT:
            can = new DirectCan( );
            break;
#endif
        case REMOTE:
            {
                const string hostname = getStringParameter( "server" );
                const unsigned int mask = calculateAcceptanceMask( address );
                const unsigned int code = calculateAcceptanceCode( address );
                InetCan * netcan = new InetCan( hostname );

                netcan->clearFilters( );
                netcan->addFilter( code, mask, canbus );

                can = netcan; 
            }
            break;
        default:
            break;
    }
    return can;
}

CanAddressType
retrieveCommandLineAddress( ) 
{
    CanAddressType address;
    
    address.host = ( parameterWasSpecified( "host" ) ?
                     getIntParameter( "host" ) : -1 );
    address.eng = ( parameterWasSpecified( "eng" ) ?
                    getIntParameter( "eng" ) : -1 );
    address.api = getIntParameter( "api" ); 
    address.node = getIntParameter( "node" );
    address.mid = ( parameterWasSpecified( "mid" ) ?
                    getIntParameter( "mid" ) : -1 );

    return address;
}

void 
sniff( CanIo * can, 
       const CanAddressType & match,
       const int canbus, 
       const ModeType mode ) 
{
    const bool ascii = getBoolParameter( "ascii" );
    const bool abbrv = getBoolParameter( "abbrv" );
    const bool time = getBoolParameter( "time" );
    const bool mjd = getBoolParameter( "mjd" );

    if ( mjd )
        cout.precision( 12 );

    carma::canbus::Message msg;
    int msgHost;
    apiType msgApi;
    nodeType msgNode;
    msgType msgMid;
    modeType addressMode;
    double rxMjd; 

    vector<string> formatTokens;
    int fsize = 0;
    if ( parameterWasSpecified( "format" ) )
    {
      string format = getStringParameter( "format" );

      string::size_type lp = format.find_first_not_of( "%", 0 );
      string::size_type p = format.find_first_of( "%", lp );

      while ( string::npos != p || string::npos != lp )
      {
          formatTokens.push_back( format.substr( lp, p - lp ) );
          lp = format.find_first_not_of( "%", p );
          p = format.find_first_of( "%", lp );
      }

      vector<string>::iterator i;
      for ( i = formatTokens.begin(); i != formatTokens.end(); i++ )
      {
          if ( *i == "g" )
              fsize += 8;
          else if ( *i == "f" )
              fsize += 4;
          else if ( *i == "ud" )
              fsize += 4;
          else if ( *i == "d" )
              fsize += 4;
          else if ( *i == "uh" )
              fsize += 2;
          else if ( *i == "h" )
              fsize += 2;
          else if ( *i == "c" )
              fsize += 1;
      }

      if ( fsize > 8 ) {
        cerr << "Warning! Format spec is longer than 8 byte CAN packet" << endl;
        cerr << "Continuing but ignoring spec beyond 8 bytes." << endl;
      }

    }

    while ( true ) {

        msg = can->getMessage( );

        msgHost = static_cast< int >( isToHost( msg.getId( ) ) );

        addressMode = getMode( msg.getId( ) );
        if ( addressMode == APPLICATION ) 
        {
            fromId( msgApi, msgNode, msgMid, msg.getId( ) );
        } 
        else 
        {
            fromEngId( msgApi, msgNode, msgMid, msg.getId( ) );
        }

        // Do we spit this message out?
        if ( ( match.host == msgHost || match.host == -1 ) &&
                ( addressMode == match.eng || match.eng == -1 ) &&
                ( match.api == msgApi || match.api == 0 || msgApi == 0 ) && 
                ( msgNode == match.node || match.node == 0 || msgNode == 0 ) &&
                ( msgMid == match.mid || match.mid == -1 ) &&
#ifdef HAVE_SOCKETCAN 
                ( canbus == ALL_BUSSES || msg.getBusId( ) == canbus )  
#else
                ( mode == EXCLUSIVE || ( mode != EXCLUSIVE && 
                                         ( msg.getBusId( ) == canbus || 
                                           canbus == ALL_BUSSES ) ) )
#endif
           )
        {   // We've got a winner - dump contents to console...
            if ( mode == REMOTE ) {
                rxMjd = Time::MJD( );
            } 
            else 
            {
                rxMjd = msg.getRxMjd( );
            }

            if ( time ) // Output timestamp in HH:MM:SS.SSS format.
                cout << Time::getTimeString( rxMjd, 3 ) << ", ";

            if ( mjd ) // Output mjd
                cout << "MJD " << rxMjd << ", ";

            cout << msg.dump( abbrv, ascii ) << endl;

            if ( fsize > 0 )
            {
                fsize = 0;
                vector<string>::iterator i;
                DataVector data = msg.getData();

                cout << " ";

                for ( i = formatTokens.begin(); i != formatTokens.end(); i++ )
                {
                    if ( *i == "g" )
                    {
                        fsize += 8;
                        cout << dataToDouble( data );
                    }
                    else if ( *i == "f" )
                    {
                        fsize += 4;
                        cout << dataToFloat( data );
                    }
                    else if ( *i == "ud" )
                    {
                        fsize += 4;
                        cout << dataToUlong( data );
                    }
                    else if ( *i == "d" )
                    {
                        fsize += 4;
                        cout << dataToLong( data );
                    }
                    else if ( *i == "uh" )
                    {
                        fsize += 2;
                        cout << dataToUshort( data );
                    }
                    else if ( *i == "h" )
                    {
                        fsize += 2;
                        cout << dataToShort( data );
                    }
                    else if ( *i == "c" )
                    {
                        fsize += 1;
                        unsigned char c = dataToUbyte( data );
                        cout << (char)(::isprint((char)c) ? c : '?');
                    }

                    cout << " ";

                    if ( fsize >= 8 )
                        break;
                }

            }
            else if ( ascii ) // what else can we do?
                //       cout << msg.asciidump( );

                cout << endl;

        } 
        else 
        {
            // This is a nice way to detect if filtering is done correctly.
            if ( mode == REMOTE ) {
                Program::getLogger( ) << Priority::WARN << "CAN-Over-IP packet "
                    << "received but doesn't match address filter - either "
                    << "the acceptance filter is being calculated wrong or "
                    << "the server is misinterpreting it.  Message is: "
                    << msg.dump( false, false );
            }
        }
    } // End while (true)
}
    
/**
 * @version $Revision: 1.5 $
 *
 * @description
 * \nSniffs messages on one or more CAN busses. The application can run in one 
 * of four modes; Remotely via connection to a CAN-Over-IP server, locally via
 * Direct CAN, locally via exclusive access to the Janz card or in 'dry' mode
 * which just calculates and outputs an acceptance filter based on command line
 * address parameters.  By default the application uses Direct CAN mode.  
 * See 'cansniffer --keywords' for details on command line options.  
 *
 * @usage canSniffer [server=(hostname)] [board=(0-15)] [canbus=(0,1) or (0-31)]
 *      \n\t[eng=(0-1)] [api=(0-255)] [node=(0-511)] [mid=(0-1023)] 
 *      \n\t[time=(true,false)] [mjd=(true, false)] [abbrv=(true, false)] 
 *      \n\t[ascii=(true,false)]
 *
 * @key server   @noDefault string 
 *    Hostname of server for remote sniffing. Incompatible with board option.
 * @key board    @noDefault    int 
 *    Janz board # for exclusive bus access. Incompatible with server option.
 * @key canbus   @noDefault    int 
 *    Canbus id filter or modbusId (0,1) in exclusive mode. Default is every
 *    \n\tbus in non-exclusive mode. Parameter is required for exclusive mode. 
 * @key term          true    bool
 *    Specify whether or not to terminate the bus in exclusive mode.
 * @key host     @noDefault    int 
 *    Filter by host bit.  0 for msgs from host, 1 to the host (default both).
 * @key eng      @noDefault    int 
 *    Filter by mode bit. 0 for application, 1 for engineering (default both).
 * @key api               0    int 
 *    Filter by api or board type if eng=1 (default is all). 
 * @key node              0    int 
 *    Filter by node location code or serial number if eng=1 (default is all). 
 * @key mid      @noDefault    int 
 *    Filter by message ID (default all).
 * @key time          false    bool 
 *    Display message timestamp. In remote mode the output timestamp is the 
 *    \n\ttime the packet was received by the client, NOT the host.
 * @key mjd           false    bool 
 *    Output timestamp in MJD format subject to restraints of time option.
 * @key ascii         false    bool 
 *    Output ascii representation of packet after hex values.
 * @key abbrv         false    bool 
 *    Output abbreviated canpacket representation.
 * @key dry           false    bool 
 *    Print out acceptance filter for above parameters and exit.
 * @key format @noDefault s 
 *    Format for values in sprintf format e.g. %f%c%c%c%c.  Available formats:
 *    \n\t%g  - 8 byte double
 *    \n\t%f  - 4 byte float
 *    \n\t%ud - 4 byte unsigned int
 *    \n\t%d  - 4 byte int
 *    \n\t%uh - 2 byte unsigned short
 *    \n\t%h  - 2 byte short
 *    \n\t%c  - 1 byte unsigned char
 * @logger DEFAULT_FACILITY carma.canbus.cansniffer
 */
int 
Program::main() 
try {
    const CanAddressType address = retrieveCommandLineAddress( ); 
    const int canbus = ( parameterWasSpecified( "canbus" ) ?
                         getIntParameter( "canbus" ) : ALL_BUSSES );
    const ModeType mode = determineMode( );

    CARMA_CPTRACE( Trace::TRACE5, "Mode is " << mode << "." );

    // Validate input parameters common to all modes.
    
    if ( !addressParametersValid( address ) )
        exitWithUsage( );

    if ( !canbusParameterValid( mode, canbus ) )
        exitWithUsage( );

    if ( mode == DRY ) {
        coutAcceptanceFilter( address, canbus );
        return 0;
    }

    ::std::auto_ptr< CanIo > can( createCanIo( address, canbus,  mode ) );

    sniff( can.get( ), address, canbus, mode );

    return 0; // We'll never get here
} catch ( const ::std::exception & ex ) {
    cerr << ex.what( ) << endl;
    return 1;
} catch ( ... ) {
    cerr << "Unknown exception caught in Program::main() - exiting." << endl;
    return 1;
} // End Program::main()
