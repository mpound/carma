/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.15 $
 * $Date: 2009/10/27 15:17:10 $
 * $Id: DewarRegulation.cc,v 1.15 2009/10/27 15:17:10 mpound Exp $
 */


// CARMA includes
#include "carma/antenna/bima/DewarRegulation.h"

#include <cmath>

using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::services;
using namespace carma::antenna::bima;

#define NMAX 20

const std::string DewarRegulation::_poaName = "DewarRegulator";

DewarRegulation::DewarRegulation( Configuration& config, bool server, bool emulate ) :
  SharedMemory( config.getAntenna().c_str() ),
  _dewar( config ),
  _config( config ),
  _log( Program::getLogger() )
{
  //  if ( server ) 
  //    init(); // init shared memory

  CPTRACE( Trace::TRACE1, "Constructing DewarRegulation Obj..." );

  _emulate = emulate;
  _setPoint = _oldPoint = 0.;
  _defrostStart = 0;
  _lastRMS = time(NULL);

  // must have at least one value
  // The current instantaneous temp is a good choice as any
  _maxminT.push_back( TimedValue( time(NULL), _dewar.stage4temp() ) );
  _maxminV.push_back( TimedValue( time(NULL), 0.0 ) ); // must have at least one value
}

DewarRegulation::~DewarRegulation()
{
    printf("exiting...\n");
    setHeater( 0. ); // turn off heater on exit
}

bool DewarRegulation::alreadyRunning()
{
  int t1, t2;
  getData( "WATCHDOG", &t1, 1 );
  usleep(500000);	// wait 1/2 sec
  getData( "WATCHDOG", &t2, 1 );
  printf("Check whether DewarRegulation already is running; t1 = %d, t2 = %d\n",t1,t2);
  return ( t1 != t2 );
}

void DewarRegulation::updateWatchdog()
{
  struct timeval tm;
  struct timezone tz;
  gettimeofday( &tm, &tz );
  int t = tm.tv_usec;
  putData( "WATCHDOG", &t, 1 );
}


void DewarRegulation::regulate()
{
  double tAvg, tDelta, tSum, tCur, vHeater, tInt, gaini, gainp;
  int avgCnt, status, setChange, hSettle, hmincnt, hmaxcnt;
  bool renew = true;
  bool filled = false;
  double sAvg, sAvgSqSum, sAvgMax, sAvgMin, vHeaterMax, vHeaterMin;
  double cMagMin, cMagMax;
  float t[NMAX];
  int ptr = 0;


  tAvg = tDelta = tSum = vHeater = tInt = 0.;
  avgCnt = 0;
  renew = false;
  sAvg = sAvgSqSum = sAvgMin = vHeaterMin = sAvgMax = 0.; 
  vHeaterMax = 11.98;
  hSettle = 300;  // 30 seconds in 1/10th increments
  setChange = status = 1;

  gaini = _dewar.getGainI();
  gainp = _dewar.getGainP();
  CPTRACE( Trace::TRACE1, "gaini: " << gaini );
  CPTRACE( Trace::TRACE1, "gainp: " << gainp );

  hmincnt = _dewar.getHeaterMinCnt();
  hmaxcnt = _dewar.getHeaterMaxCnt();

  // magnitude of cycle range
  cMagMin = 300.;
  cMagMax = 0.; 

  CPTRACE( Trace::TRACE3, "Entering regulation loop..." );
  on();
  putData( "REGCHANGE", &setChange, 1 );
  int zero = 0;
  putData( "DEREGLOG", &zero, 1 );

  while ( true )
  {
    if ( isOn() == true )
    {
      usleep(100000);      // wake up every 1/10th of sec
      updateWatchdog();    // used by other processes

      if ( isDefrosting() == true )
      {
	if ( _defrostStart == 0 )
	{ 
	  _defrostStart = time(NULL);
	  _oldPoint = _setPoint;
	  setPoint( 30. );
	  time_t fiveminahead = _defrostStart+300;
	  _log << Priority::INFO << "Defrosting mixer, should complete by "
	    << asctime(localtime(&fiveminahead));
	  CPTRACE( Trace::TRACE1, "Entering defrost mode, should be complete by "
	      << asctime(localtime(&fiveminahead)) );
	}
	else 
	{
	  if ( _defrostStart < (time(NULL)-300) )
	    cancelDefrost();	// sets DEFROST=0 in common
	}
      }

      else if ( isDefrosting() == false && _defrostStart != 0 )
      {
	_defrostStart = 0;
	renew = true;
	setPoint( _oldPoint );
	// TODO: Call updateSetPoint to get regulation
	// to drop the setPoint
	CPTRACE( Trace::TRACE1, "Ending defrost mode" );
	_log << Priority::INFO << "Defrosting completed.";
      }
    }
    else // isOn() == true 
    {
      CPTRACE( Trace::TRACE3, "  regulation is off, sleeping for 2 seconds" );
      sleep(2); // off, no reason to wake up every 1/10th of a sec
    }

    if ( renew )
    {
      CPTRACE( Trace::TRACE3, "Zeroing out average, delta, etc..." );
      tAvg = tDelta = tSum = 0.;
      avgCnt = ptr = 0;
      renew = false;
      filled = false;
    }

    tCur = (*this.*getTemp)();

    // Don't bother if temp is > 30K or < 2.5K
    if ( (tCur > 30.) || (tCur < 2.5) ) {
      CPTRACE( Trace::TRACE3, "Current temperature out of regulation range: " << tCur );
    }
    else 
    {   

      // fill out array with NMAX data before computing tAvg
      if ( !filled )
      {
	CPTRACE( Trace::TRACE5, "tAvg array not filled" );
	t[ptr++] = tCur;
	tAvg += tCur;		        // accumulating sum in tAvg

	if (ptr >= NMAX)
	{
	  CPTRACE( Trace::TRACE5, " tAvg array now filled!" );
	  filled = true;                // set filled to 1 once array is filled
	  tAvg /= NMAX;                 // initial calculation of tAvg
	}
      }
      else	// array is filled
      {                          
	ptr = ptr % NMAX;	              // pointer to array location for this temp; wraps around
	tAvg += (tCur - t[ptr]) / NMAX ;      // compute new running average
	setAvgTemp( (float)tAvg );            // fill in shared memory value, update max/min, etc.

	if (ptr == 0)                         // check for new setpoint every 2 secs
	{
	  CPTRACE( Trace::TRACE5, "2 sec avg temp: " << tAvg );
	  getData( "REGCHANGE", &setChange, 1 );
	  // getPoint();
	  CPTRACE( Trace::TRACE5, "Status, reg: " << status << " regchange: " << setChange
	      << " setpoint: " << _setPoint );
	}

	t[ptr++] = tCur;                      // store new temp in the array, then update ptr 

	if ( setChange == 0 ) // keep regulating
	{
	  CPTRACE( Trace::TRACE3, "  setChange == 0 " );
	  tDelta = _setPoint - tAvg;
	  tInt += tDelta;
	  if (tInt > 100/gaini) tInt = 100./gaini;	// put cap on integral gain
	  double ttmp = ( (gainp * tDelta) + (gaini * tInt) );
	  vHeater = ( ttmp > 0. ) ? sqrt(ttmp) : 0.;
	  CPTRACE( Trace::TRACE3, " gainp: " << gainp
	      << " tDelta: " << tDelta << " gaini: " << gaini
	      << " tInt: " << tInt << " ttmp: " << ttmp );
	}

	setHeater( vHeater ); // internally limits 0-11.98

	if ( setChange > 0 )
	{
	  hSettle = 300;  // 30 seconds in 1/10th increments
	  setChange = 0;
	  putData( "REGCHANGE", &setChange, 1 );
	  getPoint();
	  CPTRACE( Trace::TRACE2, "Set Point Changed to: " << _setPoint << " hSettle: " << hSettle );
	}

	// Update statistics
      }
    }
  }
}

// 

void DewarRegulation::setPoint( double temp )
{
  int change = 1;
  _setPoint = temp;
  putData( "SETPOINT", &_setPoint, 1 );
  putData( "REGCHANGE", &change, 1 );
  double mjd = Time::MJD();
  putData( "SETPOINTMJD", &mjd, 1 );
}

double DewarRegulation::getPoint()
{
  getData( "SETPOINT", &_setPoint, 1 );
  return _setPoint;
}

double DewarRegulation::getPointMJD()
{
  double mjd;
  getData( "SETPOINTMJD", &mjd, 1 );
  return mjd;
}

void DewarRegulation::setHeater( double volts )
{
  CPTRACE( Trace::TRACE3, "   Set heater3 to " << volts << "V" );
  _dewar.limit( volts, 0., 11.98 );
  _dewar.setHeater3V( volts );
  setRunningV( volts ); // update running 30 min log of max/min values
}

void DewarRegulation::diagnosticLog( double tavg,  double min, double max )
{
  int logit;
  static FILE *diagLog = NULL;


  getData( "DEREGLOG", &logit, 1 );

  if ( logit > 0 )
  {
    if ( diagLog == NULL )
      diagLog = fopen( "/tmp/dereg.dat", "w" );

    if ( diagLog != NULL )
    {
      fprintf( diagLog, "%3.6g %3.6g %3.6g %3.6g %2.3g\n",
	  _setPoint, tavg, min, max, _dewar.getHeater3mW() );
      fflush( diagLog );
    }
  }

}

void DewarRegulation::startDiagLog( bool start )
{
  int doit = 0;

  if ( start )
    doit = 1;

  putData( "DEREGLOG", &doit, 1 );
}

void DewarRegulation::thread( DewarRegulation &This )
{
  This.regulate();
}

// No-op for now, to avoid compiler warnings for the
// main bimaDewarReg program
bool DewarRegulation::isOk()
{
  return true;
} 

bool DewarRegulation::isOn()
{
  int status;
  bool wereon = false;

  getData( "DEWARREG", &status );

  if ( status == 1 )
    wereon = true;

  return wereon;
}

void DewarRegulation::on()
{
  int status = 1;
  CPTRACE( Trace::TRACE2, " on() called" );
  putData( "DEWARREG", &status );
}

void DewarRegulation::off()
{
  int status = 0;
  putData( "DEWARREG", &status );
}

void DewarRegulation::defrost()
{
  int status = 1;
  putData( "DEFROST", &status );
}

void DewarRegulation::cancelDefrost()
{
  int status = 0;
  CPTRACE( Trace::TRACE3, " cancelDefrost() called" );
  putData( "DEFROST", &status );
}

bool DewarRegulation::isDefrosting()
{
  int status = 0;
  getData( "DEFROST", &status );

  return (status == 1);
}

void DewarRegulation::setRunningV( float v )
{
  putData( "DRCURV", &v, 1 );

  insertAndCullMaxMin( _maxminV, v );
  setMaxV( static_cast<TimedValue>(*_maxminV.begin())._v );
  setMinV( static_cast<TimedValue>(*_maxminV.rbegin())._v );
}

void DewarRegulation::setMaxV( float v )
{
  putData( "DRMAXV", &v, 1 );
}

void DewarRegulation::setMinV( float v )
{
  putData( "DRMINV", &v, 1 );
}

float DewarRegulation::getRunningV()
{
  float v;
  getData( "DRCURV", &v, 1 );
  return v;
}

// Returns max volts from last 30 minutes
float DewarRegulation::getMaxV()
{
  float v;
  getData( "DRMAXV", &v, 1 );
  return v;
}

// Returns min volts from last 30 minutes
float DewarRegulation::getMinV()
{
  float v;
  getData( "DRMINV", &v, 1 );
  return v;
}

void DewarRegulation::setAvgTemp( float k )
{
  CPTRACE( Trace::TRACE4, "setAvgTemp( " << k << " )" );

  putData( "DRAVGTEMP", &k, 1 );

  // Could optimize this by using _tempList to get at
  // the max/mins from the last 30 minutes instead of
  // having a separate list, but that just optimizes for
  // size, not speed
  insertAndCullMaxMin(_maxminT, k);
  setMaxAvgTemp( static_cast<TimedValue>(*_maxminT.begin())._v );
  setMinAvgTemp( static_cast<TimedValue>(*_maxminT.rbegin())._v );

  CPTRACE( Trace::TRACE4, " culling _tempList" );
  // First value in running mean is older than seconds?
  if ( _tempList.size() != 0 &&
      static_cast<TimedValue>(*_tempList.begin())._t < time(NULL)-1800 )
    _tempList.pop_front();

  _tempList.push_back( TimedValue( time(NULL), k ) );

  if ( _lastRMS < time(NULL)-2 )
  {
    computeAvgTempSetPointRMS( _tempList );
    _lastRMS = time(NULL);
  }
}


void DewarRegulation::computeAvgTempSetPointRMS( list<TimedValue> &aList )
{
  list<TimedValue>::iterator i;
  float sumx = 0., sqsumx = 0.;
  int n = 0;

  CPTRACE( Trace::TRACE3, "   computeAvgTempSetPointRMS(...)" );

  if ( aList.size() == 0 )
  {
    n = 1;
    sumx = 1.;
    sqsumx = 1.;
  }

  for ( i = aList.begin(); i != aList.end(); i++ )
  {
    n++;
    float val = static_cast<TimedValue>(*i)._v;
    sumx += val;
    sqsumx += val*val;
  }

  CPTRACE( Trace::TRACE2, "    n: " << n << " sumx: " << sumx );

  float avg = sumx/n;
  float rms = sqrt( (sqsumx/n) - (avg*avg) );

  putData( "DRSETRMS", &rms, 1 );
}

float DewarRegulation::getTempSetPointRMS()
{
  float rms;
  putData( "DRSETRMS", &rms, 1 );
  return rms;
}

float DewarRegulation::getAvgTemp()
{
  float k;
  getData( "DRAVGTEMP", &k );
  return k;
}

float DewarRegulation::getMinAvgTemp()
{
  float k;
  getData( "DRMINT", &k, 1 );
  return k;
}

float DewarRegulation::getMaxAvgTemp()
{
  float k;
  getData( "DRMAXT", &k, 1 );
  return k;
}

void DewarRegulation::setMinAvgTemp( float k )
{
  CPTRACE( Trace::TRACE5, "  setMinAvgTemp( " << k << " ) " );
  putData( "DRMINT", &k, 1 );
}

void DewarRegulation::setMaxAvgTemp( float k )
{
  CPTRACE( Trace::TRACE5, "  setMaxAvgTemp( " << k << " ) " );
  putData( "DRMAXT", &k, 1 );
}

void DewarRegulation::cullEntriesOlderThan( list<TimedValue> &aList, time_t t )
{
  list<TimedValue>::iterator i = aList.begin();

  while ( i != aList.end() )
  {
    TimedValue curtt( static_cast<TimedValue>( *i ) );
    if ( curtt._t < t )
    {
      aList.erase( i );
      i = aList.begin();
    }
    else
      i++;
  }
}

void DewarRegulation::insertAndCullMaxMin( list<TimedValue> &aList, float value )
{
  list<TimedValue>::iterator i;
//  int pos = 0;

  TimedValue newtt( time(NULL), value );

  // first, cull out entries older than 30 minutes 
  cullEntriesOlderThan( aList, time(NULL)-1800 );

  for ( i = aList.begin(); i != aList.end(); i++ )
  {
    TimedValue curtt( static_cast<TimedValue>( *i ) );

    if ( newtt._v > curtt._v )
    {
      aList.insert( i, newtt );
      break;  // done the job, exit for loop
    }
    else if ( newtt._v == curtt._v )
    {
      // If value is equal to this one, erase older one
      // and insert newer one with more recent time.
      aList.insert( i, newtt );
      aList.erase( i );
      break;  // done the job, exit for loop
    }
  }

  // Append if we're reaching the last one
  if ( i == aList.end() )
    aList.push_back( newtt );
}

void DewarRegulation::setStage( int stage )
{
  CPTRACE( Trace::TRACE1, "Will get temp readings from stage: " << stage );
  _log << Priority::INFO << "Will get temp readings from stage: " << stage;

  switch ( stage )
  {
    case 1: getTemp = &DewarRegulation::getStage1Temp; break;
    case 2: getTemp = &DewarRegulation::getStage2Temp; break;
    case 3: getTemp = &DewarRegulation::getStage3Temp; break;
    case 4: getTemp = &DewarRegulation::getStage4Temp; break;
    case 5: getTemp = &DewarRegulation::getStage5Temp; break;
    default:
      throw CARMA_ERROR( "Illegal stage number, should be 1-5" );
      break;
  }
}
