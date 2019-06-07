/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.18 $
 * $Date: 2012/02/07 02:25:51 $
 * $Id: CalWheel.cc,v 1.18 2012/02/07 02:25:51 plambeck Exp $
 */


#include <cmath>

// CARMA includes
#include "carma/antenna/bima/CalWheel.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

// --- normal, correct, settings below ---
const int CalWheel::skytarg[5] = {26284, 15019, 31916, 20651, 15292};
const int CalWheel::ambtarg[5] = {18774,  7510, 24406, 13142,    50};
const int CalWheel::hottarg[5] = {11264,    50, 16897,  5632,    50};

int cmsky = 25000;
int cmamb = 500;
int mmclear = 20000;   // this is the nominal position for mm observing

CalWheel::CalWheel( Configuration& config )
     : TelemetryClient( config )
{
  _calwheel = new Motor( "calwheel", config );
  _pol = new Motor( "polarizer", config );
  _cmOpticsInstalled = _config.cmOpticsInstalled();

  // ensure that something is in the "CALWHINFO" location
  // If not really moving, this will be corrected on the next
  // Monitor updater pass in MonitorUpdater.cc
  int status = ERROR;
  putData( "CALWHINFO", &status, 1);
  setCurSequenceNo( 0 );
  setNextSequenceNo( 0 );
}

void CalWheel::setPosition( CalWheel::Positions position, int iband )
{
  int status;
  status = (int)MOVING;   // default status is MOVING while in this routine
  putData( "CALWHINFO", &status, 1 );

  // if new cm optics are installed, a single amb load moves in/out for all bands
  if ( _cmOpticsInstalled ) {
    char mask = 0x20 ;
    char sky = 0x00 ;
    char amb = 0x20 ;
    char bitsout = sky ;
    if ( position==AMB )
      bitsout = amb ;
    printf("# CalWheel ... cm optics installed - setting bits to %x\n", bitsout) ;
    setbits( "BITSTO50", bitsout, mask ) ;

    // wait until we reach position; timeout is 10 secs
    bool ambSw = false ;
    bool skySw = false ;
    int ncount = 20 ;
    int statusbits ;
    while (ncount-- && (getPosition(iband) != position) ) {
      getData( "BITSIN5F", &statusbits) ; 
      ambSw = statusbits & 0x10 ;
      skySw = statusbits & 0x20 ;
      printf("#          ... waiting; skySw = %d, ambSw = %d\n", skySw,ambSw) ;
      usleep(500000) ;	// wait 0.5 sec
    }
    if (ncount > 0) {
      getData( "BITSIN5F", &statusbits) ; 
      ambSw = statusbits & 0x10 ;
      skySw = statusbits & 0x20 ;
      printf("#          ... OK:  skySw = %d, ambSw = %d\n", skySw,ambSw) ;
    } else {
      if ( statusbits & 0x80 ) {
        printf("#          ... on remote control, but failed to reach target\n" ) ;
      } else {
        printf("#          ... ON LOCAL CONTROL, failed to reach target\n" ) ;
      }
    }
  }

  // if old optics are installed, tune the calwheel motor
  else {
    //  printf("# calwheel position set for dewarwindow %d\n", iband);

    if ( iband == 4 ) {		// for 1cm band, use polarizer frame instead
      if ( position == SKY )
        _pol->moveToTarget( cmsky );
      else if ( position == AMB )
        _pol->moveToTarget( cmamb );
      else if ( position == FIXED )
        _pol->moveToTarget( cmamb );
    } else {
    // move polarizer frame if it is in the way for some reason (comment out these lines in Berkeley)
    //    if ( abs(_pol->position() - mmclear ) > 500 )
    //      _pol->moveToTarget( mmclear );

    // now move calwheel to appropriate position
      if ( position == SKY ) {
        _calwheel->moveToTarget( skytarg[ iband ] );
        usleep(50000) ;   // recheck posn after 0.5 sec, try again if necessary
        if ( getPosition( iband ) != position ) {
          _calwheel->moveToTarget( skytarg[ iband ] );
        }
      }
      else if ( position == AMB ) {
        _calwheel->moveToTarget( ambtarg[ iband ] );
        usleep(50000) ;   // recheck posn after 0.5 sec, try again if necessary
        if ( getPosition( iband ) != position ) {
          _calwheel->moveToTarget( ambtarg[ iband ] );
        }
      }
      else if ( position == FIXED ) {
        _calwheel->moveToTarget( hottarg[ iband ] );
      }
    }
  }
    

  status = (int)ERROR;   // now that we are finished, default status turns back to ERROR
  putData( "CALWHINFO", &status, 1 );
  setCurSequenceNo( getNextSequenceNo() );
}

void CalWheel::setPosition( string stringpos, int iband )
{
  
  CPTRACE( Trace::TRACE3, "CalWheel::setPosition( " << stringpos << ", " << iband << " )" );

  if ( stringpos.compare( "sky" ) == 0 )
    setPosition( SKY, iband );
  else if ( stringpos.compare( "amb" ) == 0 )
    setPosition( AMB, iband );
  else if ( stringpos.compare( "fixed" ) == 0 )
    setPosition( FIXED, iband );
}

int CalWheel::getInstantPosition()
{
  return _calwheel->position();
}

CalWheel::Positions CalWheel::getPosition( int iband )
{
//  int iband = band - 'A';
  int pos;

  // if calwheel is not found to be on a named postion, then return what is in CALWHINFO
  // CALWHINFO is set to MOVING while CalWheel is moving it, ERROR when CalWheel exits
  getData( "CALWHINFO", &pos, 1) ;

  // if new cm optics are installed, read mirror limit switches to figure out position
  if ( _cmOpticsInstalled ) {
    int statusbits ;
    getData( "BITSIN5F", &statusbits) ; 
    if ( (statusbits & 0x30) == 0x20 )  // amb = bit4 = low; AND sky = bit5 = high
      pos = AMB ;
    else if ( (statusbits & 0x30) == 0x10 )  // sky = bit5 = low; AND amb = bit4 = high
      pos = SKY ;
  }  

  // with old optics, read calwheel position, compare with band-dependent values in table
  else {
    if ( iband == 4 ) {       // 1cm receiver - query polarization setup
      if ( abs(_pol->position() - cmsky ) < 500 )
        pos = SKY;
      else if ( abs(_pol->position() - cmamb ) < 500 )
        pos = AMB;
      else if ( abs(_pol->position() - cmamb ) < 500 )
        pos = FIXED;
    } 
    else {
      if ( abs(_calwheel->position() - skytarg[ iband ]) < 500 )
        pos = SKY;
      else if ( abs(_calwheel->position() - ambtarg[ iband ]) < 500 )
        pos = AMB;
      else if ( abs(_calwheel->position() - hottarg[ iband ]) < 500 )
        pos = FIXED;
    }
  }
  return (CalWheel::Positions)pos;
}

double CalWheel::getAmbTemp()
{
  return thermistor(( atodin( "TAMBCLPL" ) * THERMI ));
}

void CalWheel::setCurSequenceNo( int seq )
{
  putData( "CALSEQNO", &seq );
}

void CalWheel::setNextSequenceNo( int seq )
{
  putData( "CALNXTSEQNO", &seq );
}

int CalWheel::getCurSequenceNo()
{
  int seq;
  getData( "CALSEQNO", &seq );
  return seq;
}

int CalWheel::getNextSequenceNo()
{
  int seq;
  getData( "CALNXTSEQNO", &seq );
  return seq;
}
