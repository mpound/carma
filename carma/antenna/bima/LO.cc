/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.19 $
 * $Date: 2011/09/19 17:20:13 $
 * $Id: LO.cc,v 1.19 2011/09/19 17:20:13 control Exp $
 */


#include <cmath>

// CARMA includes
#include "carma/antenna/bima/LO.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;


LO::LO( Configuration& config )
     : TelemetryClient( config )
{
  _name = string( "lo" );
  _yigAnt = config.getAntennaNo() - 1;
}


bool LO::xLockStatus()
{
  int statusbits;
  
  getData( "BITSIN5F", &statusbits );

  return ( (statusbits & 0x02) == 0 ) ? false : true;
}


int LO::getLockInfo()
{
  int value;

  getData( "XLOCKINFO", &value, 1 );
  return value;
}

void LO::setLockInfo( int value )
{
  putData( "XLOCKINFO", &value, 1 );
}

void LO::lockX( double x_freq )
{
  char ss[80];
  ostringstream errMsg;
  int srch[] = {50, -50, 100, -100, 150, -150, 0} ;
  int nsrch = 0;

  // exit with error if YIG freq is outside 8-12.5 GHz allowed range
  if ( x_freq < 8.0 || x_freq > 12.5 )
  {
    sprintf(ss, "Unable to lock Xband to %.3f GHz; outside 8 - 12.5 GHz allowed range\n", x_freq);
    _logger << Priority::ERROR << ss ;
    printf("%s", ss);
    errMsg << ss;
    throw CARMA_ERROR( errMsg.str() );   // we need to insert a message here!
  }

  setCommanded( x_freq );       // store commanded x_freq in shared memory
  setLockInfo( 1 );             // this needs to be changed to a class const... 1 is search
  sprintf(ss, "Locking Xband to %.3f GHz", x_freq);
  printf("%s\n", ss);
  _logger << Priority::INFO << ss;

  short yig_counts = (short) ( (x_freq - 8.0)/(12.5 - 8.0) * 2047 ) ;
     // 0-2047 counts tune YIG from 8.0 to 12.5 GHz
  if ( yig_counts < 0 ) yig_counts = 0;
  if ( yig_counts > 2047 ) yig_counts = 2047;
  short predicted = yig_counts;       // save for later

  // send to receiver
  tpoke( "YIGTUNE", (unsigned short)yig_counts );
  sleep(1);   // wait 1 sec for circuit to settle
  int nsteps = 5;
  if ( (xLockStatus() != true) && (nsteps--) )
    sleep(1);  // wait up to 5 secs more for YIG to lock 

  // send warning if readback disagrees with setpoint
  double xf;    // save readback so message reports exactly what if-statement saw
  if ( abs( (xf = xBandFreq()) - x_freq) > 0.1 ) {
    sprintf(ss, "Readback Xfreq = %.3f GHz -- on local control?", xf) ;
    printf("%s\n",ss);
    _logger << Priority::WARN << ss;
  }
 
  nsteps = 30;
  int waittime = 100000; // 10th of second
  bool keeptrying = true;

  while ( (nsteps--) && ( keeptrying ) )
  {
    double errv = xBandErrorVolts();
    sprintf(ss, "... yig_counts: %d   Xlock: %d   errv: %.2f\n", 
	yig_counts, xLockStatus(), errv ) ;
    printf("%s", ss);
    _logger << Priority::INFO << ss ;

    if ( xLockStatus() == true )
    {
      setLockInfo( 2 ); // const again, this is OPTIMIZING

      if ( ( fabs(errv) < 0.1 )
           || ( (errv > 0) && (yig_counts == 2047) ) 
           || ( (errv < 0) && (yig_counts == 0) )
         )
        keeptrying = false;
      else if (errv > 0.)
        yig_counts++;
      else 
        yig_counts--;

      waittime = 100000; // 10th of second

    }
    else if (nsrch < 7 )      // search farther if not locked
    {
      yig_counts = predicted + srch[nsrch++];
      waittime = 1000000; // 1 second
    }
    else                        // exit if search loop failed
    {
      keeptrying = false;
    }

    if ( yig_counts < 0 ) yig_counts = 0;
    if ( yig_counts > 2047 ) yig_counts = 2047;
    CPTRACE( Trace::TRACE5, "YIG wrote counts: " << yig_counts );
    tpoke( "YIGTUNE", (unsigned short) yig_counts );
    if ( _config.isEmulating() == false )
      usleep(waittime); // circuit settling...
  }
   
  // finish up
  if ( xLockStatus() != true )
  {
    sprintf(ss, "Failed to lock Xband to %.3f GHz; predicted yig counts = %d\n",
	x_freq, predicted) ;
    printf("%s",ss);
    _logger << Priority::ERROR << ss ;
  }
  else
  {
    sprintf(ss, "Locked Xband to %.3f GHz, errv = %.2f, yig_counts = %d, predicted = %d\n",
	x_freq, xBandErrorVolts(), yig_counts, predicted);
    printf("%s",ss);
    _logger << Priority::INFO << ss;
  }

  setLockInfo( 0 ); // this means we're not doing anything, so just pay attention to xLockStatus
}

double LO::xBandFreq ()
{
  unsigned short raw;
  double GHz;
 
  raw = atodin( "XBDYIGTN" );
  GHz = ((raw * 4.516129 / 32768. * 2.697 * 4.5 / 10.) + 8.);
  return GHz;
}

double LO::xBandErrorVolts ()
{
  return (double)(( atodin( "XBDERRV" ) * VOLTS10 ) / 2.);
}

double LO::xBandIFLevel ()
{
  return (double)( atodin( "XBDIFLVL" ) * VOLTS10 );
}

void LO::setCommanded( double xfreq )
{
  putData( "YIGCMDED", &xfreq );
}

double LO::getCommanded()
{
  double xfreq;
  getData( "YIGCMDED", &xfreq );
  return ( xfreq );
}


