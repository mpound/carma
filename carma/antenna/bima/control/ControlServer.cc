/**
 * @file 
 * ControlServer class definition.  Skeleton lifted out of
 * OVRO control server.
 *
 * @Author Andy Beard 
 * @Author Colby Gutierrez-Kraybill
 * $Revision: 1.16 $
 * $Id: ControlServer.cc,v 1.16 2012/08/01 14:13:06 friedel Exp $
 */

#include "carma/antenna/bima/control/bimaDriveControl.h"
#include "carma/antenna/bima/control/bimaDriveControl_skel.h"
#include "carma/antenna/bima/control/bimaDriveControl_skel_tie.h"
#include "carma/antenna/bima/control/ControlServer.h"
#include "carma/antenna/common/CalibratorControl.h"
#include "carma/antenna/common/CalibratorControl_skel.h"
#include "carma/antenna/common/CalibratorControl_skel_tie.h"
#include "carma/antenna/common/CryoControl.h"
#include "carma/antenna/common/CryoControl_skel.h"
#include "carma/antenna/common/CryoControl_skel_tie.h"
#include "carma/antenna/common/LOControl.h"
#include "carma/antenna/common/LOControl_skel.h"
#include "carma/antenna/common/LOControl_skel_tie.h"
#include "carma/antenna/common/FocusControl.h"
#include "carma/antenna/common/FocusControl_skel.h"
#include "carma/antenna/common/FocusControl_skel_tie.h"
#include "carma/antenna/common/FrontEndControl.h"
#include "carma/antenna/common/FrontEndControl_skel.h"
#include "carma/antenna/common/FrontEndControl_skel_tie.h"
#include "carma/antenna/common/OpticsControl.h"
#include "carma/antenna/common/OpticsControl_skel.h"
#include "carma/antenna/common/OpticsControl_skel_tie.h"
#include "carma/antenna/common/PolarizationControl.h"
#include "carma/antenna/common/PolarizationControl_skel.h"
#include "carma/antenna/common/PolarizationControl_skel_tie.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxControl_skel.h"
#include "carma/antenna/common/RxControl_skel_tie.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/common/RxSelector_skel.h"
#include "carma/antenna/common/RxSelector_skel_tie.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/corba/Server.h"
#include "carma/antenna/bima/ProcessMonitor.h"

#include <pthread.h>
using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::antenna::bima;
using namespace carma::antenna::common;

// -----------------------------------------------------------------------------
ControlServer::ControlServer( Configuration &config,
                              corba::Server & server ) : 
    _config( config ),
    cal_( _config ),
    cryo_( ),
    drive_( _config.getAntenna(), config.isEmulating() ),
    lo_( _config ),
    focus_( _config ),
    fe_( _config ),
    op_( _config ),
    po_( _config ),
    server_( server )
{
    CPTRACE( Trace::TRACE5, "Constructing ControlServer..." );

    namespace POA_cac = POA_carma::antenna::common;
    namespace POA_cabc = POA_carma::antenna::bima::control;

    // Add servants which will not be published on the nameserver but rather 
    // retrieved via the super shitty RxControl interface.
    LOControl_ptr loControlPtr;
    FrontEndControl_ptr feControlPtr;
    OpticsControl_ptr opControlPtr;
    PolarizationControl_ptr poControlPtr; 

    server_.addServant< POA_cac::LOControl_tie >( lo_, loControlPtr );
    server_.addServant< POA_cac::FrontEndControl_tie >( fe_, feControlPtr );
    server_.addServant< POA_cac::OpticsControl_tie >( op_, opControlPtr );
    server_.addServant< POA_cac::PolarizationControl_tie >( po_, poControlPtr );

    // RxControlImpl and RxSelectorImpl need to be created dynamically since
    // they need created servant references to exist.
    RxControl_ptr rxControlPtr;
    rxcontrolAp_ = std::auto_ptr< RxControlImpl >( 
        new RxControlImpl( _config, loControlPtr, feControlPtr, 
                          opControlPtr, poControlPtr ) );
    server_.addServant< POA_cac::RxControl_tie >( *rxcontrolAp_, rxControlPtr );

    CPTRACE( Trace::TRACE5, "Creating RxSelector Object" );
    rxselectorAp_ = std::auto_ptr< RxSelectorImpl >(
        new RxSelectorImpl( _config, rxControlPtr ) );

    // Add servants which will be published on the nameserver.
    const string prefix( "carma." + _config.getAntenna() + "." );
    const string calName( prefix + CALIBRATOR_NAME );
    const string cryoName( prefix + CRYO_NAME );
    const string driveName( prefix + DRIVE_NAME );
    const string focusName( prefix + FOCUS_NAME );
    const string rxSelectorName( prefix + RXSELECTOR_NAME );

    server_.addServant< POA_cac::CalibratorControl_tie >( cal_, calName );
    server_.addServant< POA_cac::CryoControl_tie >( cryo_, cryoName );
    server_.addServant< POA_cabc::DriveControl_tie >( drive_, driveName );
    server_.addServant< POA_cac::FocusControl_tie >( focus_, focusName );
    server_.addServant< POA_cac::RxSelector_tie >( *rxselectorAp_, 
                                                   rxSelectorName );
}

// -----------------------------------------------------------------------------
void ControlServer::runServer()
{
  CPTRACE(Trace::TRACE2,"running server");
    pthread_attr_t attribs;
    pthread_attr_init(&attribs);
    pthread_attr_setdetachstate(&attribs,PTHREAD_CREATE_DETACHED);
    pthread_t threadID;
    int rc = pthread_create(&threadID, &attribs, MonitorThread,(void*)this);
    if(rc != 0){
      ostringstream oss;
      oss << "Monitor thread creation failed: " << rc;
      pthread_attr_destroy(&attribs);
      throw CARMA_ERROR(oss.str().c_str());
    }
    pthread_attr_destroy(&attribs);

    server_.run( false );
}

// -----------------------------------------------------------------------------
ControlServer::~ControlServer()
{
    // Nothing
}

void *ControlServer::MonitorThread(void *data){
  ControlServer *cs = (ControlServer*)data;
  SharedMemory *_bimaShm;
  int startattempts = 0;
  while ( true ){
    try{
      _bimaShm = new SharedMemory( cs->_config.getAntenna().c_str() );
      break;
    }
    catch ( ... ){
      if ( startattempts++ < 30 ){
	sleep(3);
      }
      else{
	throw CARMA_ERROR( "Unable to open shared memory file after 29 attempts!" );
      }
    }
  }
  sleep(5); // sleep while the servers start
  int count = 0;
  while(true){
    double time = Time::MJD();
    try{
      if(cs->server_.terminated()){
	count++;
	if(count >= 3){
	  _bimaShm->putData( "CONTRLUP", &STOPPED);
	  break;
	}
      }
      else{
	_bimaShm->putData( "CONTRLUP", &time);
	count = 0;
      }
      sleep(5);
    }
    catch(...){
      _bimaShm->putData( "CONTRLUP", &STOPPED);
      return NULL;
    }
  }
  _bimaShm->putData( "CONTRLUP", &STOPPED);
  return NULL;
}
