/**
 * @version $Id: bimaIFHost.cc,v 1.28 2014/04/02 23:10:47 iws Exp $
 * @usage bimaIFHost [board=0] [canbus=1] [antenna=bima1]
 *
 * @key emulate false b Emulate hardware.
 * @key board 0 i Which canbus board to use.
 * @key canbus 1 i Which canbus wire to use, bima antennas default to the second wire for IF.
 * @key antenna @noDefault s Antenna for identification purposes.
 *                           Will use gethostname() if unspecified.
 * @key awdelay  0.200  d Monitor system autowrite delay in seconds.
 *
 * @logger RX_FACILITY carma.bima.AntennaIF
 */

#include <iostream>
#include <pthread.h>

// Corba includes
#include "carma/corba/corba.h"

#include "carma/monitor/BimaSubsystem.h"
#include "carma/antenna/bima/IFCanMaster.h"
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/common/AntennaControl.h"
#include "carma/antenna/common/AntennaControl_skel.h"
#include "carma/antenna/common/AntennaControl_skel_tie.h"
#include "carma/antenna/bima/BimaCMRxControl.h"
#include "carma/antenna/bima/BimaCMRxControl_skel.h"
#include "carma/antenna/bima/BimaCMRxControl_skel_tie.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/antenna/common/IFControl_skel.h"
#include "carma/antenna/common/IFControl_skel_tie.h"
#include "carma/antenna/common/TiltmeterControl.h"
#include "carma/antenna/common/TiltmeterControl_skel.h"
#include "carma/antenna/common/TiltmeterControl_skel_tie.h"
#include "carma/antenna/common/TiltmeterControlImpl.h"
#include "carma/antenna/bima/ProcessMonitor.h"
#include "carma/antenna/bima/SharedMemory.h"

#include "carma/corba/Server.h"

#include "carma/util/IPQreader.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <unistd.h>

using namespace std;
using namespace log4cpp;
using namespace carma;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::antenna;
using namespace carma::antenna::bima;

struct MonitorData{
  const char *name;
  Configuration& config;
  carma::corba::Server& server;
};

void *MonitorThread(void *data){
  SharedMemory *_bimaShm;
  MonitorData *md;
  md = (MonitorData*)data;
  int startattempts = 0;
  while ( true ){
    try{
      _bimaShm = new SharedMemory( md->config.getAntenna().c_str() );
      break;
    }
    catch ( ... ){
      if ( startattempts++ < 30 ){
	sleep(3);
      }
      else
	throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
    }
  }
  sleep(5); // sleep while the servers start
  int count = 0;
  while(true){
    double time = Time::MJD();
    try{
      if(md->server.terminated()){
	count++;
	if(count >= 3){
	  _bimaShm->putData( md->name, &STOPPED);
	  break;
	}
      }
      else{
	_bimaShm->putData( md->name, &time);
	count = 0;
      }
      sleep(5);
    }
    catch(...){
      _bimaShm->putData( md->name, &STOPPED);
      return NULL;
    }
  }
  _bimaShm->putData( md->name, &STOPPED);
  return NULL;
}

int Program::main()
{
  CARMA_CPTRACE(Trace::TRACE1, "bimaIFHost starting.");
  SharedMemory *bimaShm;

  // Get log4cpp logger (aka Category)
  log4cpp::Category & log = getLogger();

  AntennaNameResolver *anr;

  if ( parameterWasSpecified( "antenna" ) )
    anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
  else
    anr = new AntennaNameResolver();

  Configuration config( anr->getAntennaName(), Program::getConfDir() );

  int board = getIntParameter( "board" );
  int canbus = getIntParameter( "canbus" );
  bool emulate = getBoolParameter( "emulate" );
  const double autoWriteDelayInS = getDoubleParameter("awdelay");

  setInstanceLogname( string("carma.") + config.getAntenna() + string(".AntennaIF") );

  int startattempts = 0;
  while ( true )
    {
    try
      {
	bimaShm = new SharedMemory( config.getAntenna().c_str() );
	break;
      }
    catch ( ... )
      {
	if ( startattempts++ < 10 ) 
	  {
	    log << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	    sleep(1);
	  }
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 9 attempts!" );
      }
    }


  try
  {
    carma::monitor::BimaSubsystem *bimaMon =
      new carma::monitor::BimaSubsystem( config.getAntennaNo() );

    if ( bimaMon == 0 )
      throw CARMA_ERROR( "Unable to allocate BimaSubsystem Monitor " );

    bimaMon->startAutoWriter( autoWriteDelayInS );

    // Must be created so that the file IFIPQ exists before instantiatined an rx
    // object...
    // Add IPQ command reader
    CPTRACE( Trace::TRACE3, "Attaching ifReader" );
    IPQreader<TelemetryCommand> *ifReader = new IPQreader<TelemetryCommand>( IFIPQ, true, IPQLEN );

    CPTRACE( Trace::TRACE3, "DONE");
    Rx *rx = new Rx( config );

    CPTRACE( Trace::TRACE3, "master");
    IFCanMaster *master;

    
    if ( emulate )
      master = new IFCanMaster( *ifReader, *rx, *bimaMon, *anr, config );
    else 
      master = new IFCanMaster( board, canbus, *ifReader, *rx, *bimaMon, *anr, config );
    CPTRACE( Trace::TRACE3, "DONE2");
    pthread_t writeThread;

    CPTRACE( Trace::TRACE2, "Launching threads" );

    // Kick off CAN message processing...
    CPTRACE( Trace::TRACE2, "CAN Master thread..." );
    master->start();

    CPTRACE( Trace::TRACE2, "IPQ Command Queue thread..." );
    if ( pthread_create( &writeThread, (pthread_attr_t *)NULL,
	  IFCanMaster::startWriterThread, (void *)master ) != 0 )
      throw CARMA_ERROR( strerror( errno ) );

    log << log4cpp::Priority::INFO << "BIMA IF device handler started.";

    corba::Server & server = getCorbaServer();

    const string prefixStr( "carma." + config.getAntenna() + "." );
    const string iorPol1Str( prefixStr + common::IF1_NAME );
    const string iorPol2Str( prefixStr + common::IF2_NAME );
    const string iorACStr( prefixStr + common::ANTENNA_NAME );
    const string iorTStr( prefixStr + common::TILTMETER_NAME );
    const string cmrxStr( prefixStr + "cmRxControl" );

    

    namespace POA_cac = POA_carma::antenna::common;
    server.addServant< POA_cac::IFControl_tie >( master->getAntennaIFPol1(),
                                                 iorPol1Str );
    server.addServant< POA_cac::IFControl_tie >( master->getAntennaIFPol2(),
                                                 iorPol2Str );
    server.addServant< POA_cac::AntennaControl_tie >( 
        master->getAntennaControl(),
        iorACStr );
    server.addServant< POA_carma::antenna::bima::CMRxControl_tie >(
						      master->getCmRxControl(),
						      cmrxStr);
        
    common::TiltmeterControlImpl tiltmeter( master->getTiltmeter() ); 
    server.addServant< POA_cac::TiltmeterControl_tie >( tiltmeter,
                                                        iorTStr );
    MonitorData md = {"IFUP",config,server};

    pthread_attr_t attribs;
    pthread_attr_init(&attribs);
    pthread_attr_setdetachstate(&attribs,PTHREAD_CREATE_DETACHED);
    pthread_t threadID;
    int rc = pthread_create(&threadID, &attribs, MonitorThread,(void *)&md);
    if(rc != 0){
      ostringstream oss;
      oss << "Monitor thread creation failed: " << rc;
      pthread_attr_destroy(&attribs);
      throw CARMA_ERROR(oss.str().c_str());
    }
    pthread_attr_destroy(&attribs);


    // Block on server forever. 
    server.run( false );

  }
  catch ( carma::util::ErrorException &eex )
  {
    log << log4cpp::Priority::ERROR << eex.getErrorMessage();
    bimaShm->putData("IFUP", &STOPPED);
    return EXIT_FAILURE;
  }
  catch (...)
  {
    log << log4cpp::Priority::ERROR
      << "Unknown exception caught in Program::main()";
    bimaShm->putData("IFUP", &STOPPED);
    return EXIT_FAILURE;
  }

  bimaShm->putData("IFUP", &STOPPED);
  return EXIT_SUCCESS;
}

