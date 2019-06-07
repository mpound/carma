/**
 * Collects monitor data from can hosts and device managers, and
 * writes the integrated subsystem monitor data to local IPQ.
 * Publisher thread publishes data from local IPQ to ACC.
 *
 * @author: Amar Amarnath
 *
 * $Id: frameScriberPublisher.cc,v 1.42 2012/12/19 00:15:56 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iomanip> 
#include <string> 
#include <sstream>
#include <unistd.h>
#include "carma/corba/Server.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/monitor/MonitorPointUpdateServant.h"
#include "carma/monitor/SubsystemFrameBuffer.h"
#include "carma/monitor/FramePublisher.h"
#include "carma/monitor/FramePublisherThread.h"
#include "carma/monitor/monitorframe_skel.h"
#include "carma/monitor/monitorframe_skel_tie.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/MonitorSubsystemIPQwriter.h"
#include "carma/monitor/MonitorSubsystemMaster.h"

#include <boost/ref.hpp>
#include <boost/thread.hpp>

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;

/**
 * $Id: frameScriberPublisher.cc,v 1.42 2012/12/19 00:15:56 abeard Exp $
 */

//
// @version     $Revision: 1.42 $ $Date: 2012/12/19 00:15:56 $ 
//
// @usage     frameScriberPublisher subsystem=<subsystem name> [delay[=250]] [imr[=imr]] [ --
//            [-ORBDefaultInitRef corbaloc::<imrhost>] [-ORBconfig <config file>]
//            subsystem name must be legal CARMA subsystem name - try
//            bin/dumpMonitor list=true for a list of legal subsystem names.
//
// @description Catch a set of monitor points, write points to shared memory
//      and publish completed frames of monitor points as notifications.
//      This binary is run for each monitor subsystem with the subsystem name 
//  as parameter. The binary also accepts an integer that specifies an offset 
//  in millisecond from the half-second for writing subsystem monitor data 
//  to the subsystem's IPQ and for publishing this data to the ACC.
//  Each subsystem must run a copy of this program.
//
// @key subsystem    @mandatory  string  subsystem name, e.g. subsystem=test
// @key scriberDOon  true        bool    if true, the Scriber DO is turned on, else its off
// @key IPQwriterOn  true        bool    if false, IPQwriter & publisher threads are off
// @key publisherOn  true        bool    if true, the publisher is turned on, else its off 
// @key delay        250         int     offset from the half-second, in milliseconds
//                                       must be between 0 and 450 milliseconds to take effect
//                                       Default value is 250 msec.
// @logger MONITOR_FACILITY carma.monitor.fsp


void
dumpMonitorSubsystems( )
{
    const auto_ptr< MonitorSystem > carma( new CarmaMonitorSystem );
    
    for ( int i = 0; i < carma->getNumChildren(); ++i ) {
        cout << "\""
             << carma->getChild(i).componentRef().toString(false, false, false)
             << "\""
             << endl;
    }
}


int
Program::main( ) {

    const string subsysName = getStringParameter( "subsystem" );
    
    setInstanceLogname( "carma.monitor.fsp." + subsysName );
    
    const bool turnScriberDoOn = getBoolParameter( "scriberDOon" );
    const bool turnIpqWriterOn = getBoolParameter( "IPQwriterOn" );
    const bool turnPublisherOn = getBoolParameter( "publisherOn" );

    const int maxWriteDelayInMs = 450;
    int writeDelayInMs = getIntParameter( "delay" );

    if ( ( writeDelayInMs < 0) ) {
        ostringstream oss;

        oss << "frameScriberPublisher: subsystem=" << subsysName
            << " - Invalid delay of " << writeDelayInMs << ", exiting."; 

        programLogErrorIfPossible( oss.str( ) );

        return 1;
    }

    if ( writeDelayInMs > maxWriteDelayInMs ) {
        ostringstream oss;
        
        oss << "frameScriberPublisher: subsystem = " << subsysName
            << " - Input delay value of " << writeDelayInMs << " msec, "
            << "exceeds maximum of " << maxWriteDelayInMs << ". "
            << "Using a write delay of " << maxWriteDelayInMs << " msec"
            << "instead.";

        programLogErrorIfPossible( oss.str() );

        writeDelayInMs = maxWriteDelayInMs;
    }

    MonitorSubsystem & readSubsystem = 
        MonitorSubsystemMaster::makeSubsystem( subsysName );

    MonitorSubsystem & writeSubsystem =
          MonitorSubsystemMaster::makeSubsystem( subsysName );

    SubsystemFrameBuffer & writeBuffer =
        writeSubsystem.monitorPointSet().getBuffer();
    
    boost::mutex writeBufferMutex;

    writeBuffer.write();  // write first authoritative version

    // convert from milliseconds to seconds
    const float delayInS =
        static_cast< float >(static_cast< double >(writeDelayInMs) / 1000.0);

    CARMA_CPTRACE( Trace::TRACE7, 
                   "frameCollator: Delay value of " << writeDelayInMs <<
                   " msec " <<" used for IPQwriterThread." );

    MonitorSubsystemIPQwriter writerThread( writeSubsystem, delayInS, 
                                            writeBufferMutex );
    boost::thread writerThreadThread; // Not a thread yet.

    string senderName;
    {
        const ushort subsysId =
            readSubsystem.monitorPointSet().getBuffer().getSubsystemID();
        
        senderName = MonitorPointUpdateServant::makeName( subsysId );
    }

    carma::corba::Client & client = getCorbaClient( ); 
    FramePublisherThread publisherThread( readSubsystem, senderName, client );

    boost::thread publisherThreadThread; // Not a thread yet.

    MonitorPointUpdateServant servant( writeBuffer, writeBufferMutex );

    carma::corba::Server & server = getCorbaServer( );
    server.addServant< POA_carma::monitor::MonitorPointUpdate_tie >( 
        servant,
        senderName );

    try  {
        if ( turnIpqWriterOn ) {
            // scriber thread - writes to IPQ
            boost::thread newWriterThread( boost::ref( writerThread ) );
            writerThreadThread.swap( newWriterThread );
        }

        if ( turnIpqWriterOn && turnPublisherOn ) {
            // reads IPQ and publishes to ACC
            boost::thread newPublisherThread( boost::ref( publisherThread ) );
            publisherThreadThread.swap( newPublisherThread );
        }

        if ( turnScriberDoOn ) {
            server.run( false ); // Blocks until imr termination

            if ( turnIpqWriterOn && turnPublisherOn ) {
                publisherThreadThread.interrupt( );
                publisherThreadThread.join( );
                writerThreadThread.interrupt( );
                writerThreadThread.join( );
            }
        }

        programLogInfoIfPossible( 
            "frameScriberPublisher: threadGroup has exited." );
    
    } catch ( ... ) {
        programLogCriticalIfPossible(
            "framescriberPublisher: Exception caught in Program::main - " +
            getStringForCaught() );
            
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
