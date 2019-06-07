/**@file
 * Class definition for Stepper Motors for BIMA systems.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.35 $
 * $Date: 2011/11/01 19:37:50 $
 * $Id: Motor.cc,v 1.35 2011/11/01 19:37:50 control Exp $
 */


#include <math.h>

// CARMA includes
#include "carma/antenna/bima/Motor.h"


using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;


Motor::Motor( string motorName, Configuration& config )
     : TelemetryClient( config )
{
    CPTRACE( Trace::TRACE3, "   Motor c'tor called: " << motorName 
	      << (_config.isEmulating() ? "EMULATE MODE" : "" ) );

  ostringstream errMsg;
// This should be moved into a table lookup...

       
/*
        subref.cardaddr = 0x10;
        subref.togglebit = 7;
        subref.ad_add = 0x70;
        subref.pulse = 20;
        subref.coast = 10;
        subref.backlash = 2;
        subref.adref = 0;

    if ( motorName.compare( "POLPLATE" ) == 0 )
    {
      pol.cardaddr = 0x60;
      pol.togglebit = 7;
      pol.ad_add = 0x126;
      pol.pulse = 20;
      pol.coast = 100;
      pol.backlash = 2;
      pol.adref = 0;
    }
*/
    // Default motor type...
    _type = Motor::RECEIVER;
    _fullname = motorName;
    // Default limits

    if ( motorName.compare( "polarizer" ) == 0 )
    {
      _name = "POLFRMPS";
      _dir =  "DIRBYTE60";
      _toggle = "TOGGLEBYTE60";
      _lock = "STATUS60";
      _bitsin = "BITSTO60";
      _cardaddr = 0x60;
      _togglebit = 7;
      _pulse = 20;
      _coast = 100;
      _backlash = 2; 
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 4;
       _far = 1000;
       _fast = 0;
       _close = 500;
       _medium = 2;
       _near = 200;
       _slow = 4;
       _hair = 100;
       _crawl = 4;
       _offset = _backlash + 50;
    }
    else if ( motorName.compare( "focus" ) == 0 )
    {
      _name = "FOCUSPOS";
      _type = Motor::FOCUS;
      _hifocus = 25.;
      _lofocus = -25.;
      _dir =  "DIRBYTE50";
      _toggle = "TOGGLEBYTE50";
      _lock = "STATUS50";
      _bitsin = "BITSTO50";
      _cardaddr = 0x50;
      _togglebit = 7;
      _pulse = 50;
      _coast = 250;
      _backlash = 50;
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
    // Dick claims that the old code was being too narrow and
    // that really, being within 32 counts is fine...
    // Normally, _pulse/2 is a number between 1-5...
    // Having more leeway keeps us from missing the target so easily...
    //  _halfstep = (int)_pulse/2;
       _halfstep = 4;
       _far = 200;
       _fast = 0;
       _close = 100;
       _medium = 4;
       _near = 50;
       _slow = 8;
       _hair = 10;
       _crawl = 10;
       _offset = 100;
    }
    else if ( motorName.compare( "calwheel" ) == 0 )
    {
      _name = "CALWHEELPS";
      _dir =  "DIRBYTE60";
      _toggle = "TOGGLEBYTE60";
      _lock = "STATUS60";
      _bitsin = "BITSTO60";
      _cardaddr = 0x60;
      _togglebit = 0;
      _pulse = 50;
      _coast = 250;
      _backlash = 50;
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 50;
       _far = 800;
       _fast = 0;
       _close = 400;
       _medium = 2;
       _near = 200;
       _slow = 3;
       _hair = 100;
       _crawl = 2;
       _offset = 100;
    }
    else if ( motorName.compare( "mmoscAD" ) == 0 )
    {
      _type = Motor::MMOSC;
      _name = "MMOSCADPS";
      _dir =  "DIRBYTE60";
      _toggle = "TOGGLEBYTE60";
      _lock = "STATUS60";
      _bitsin = "BITSTO60";
      _cardaddr = 0x60;
      _togglebit = 5;
      _pulse = 2;
      _coast = 30;
      _backlash = 50;
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
    // Dick claims that the old code was being too narrow and
    // that really, being within 32 counts is fine...
    // Normally, _pulse/2 is a number between 1-5... 
    // Having more leeway keeps use from missing the target so easily...
    //  _halfstep = (int)_pulse/2;
       _halfstep = 4;
       _far = 200;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4;
       _hair = 50;
       _crawl = 8;
       _offset = 100;
    }
    else if ( motorName.compare( "mmbckAD" ) == 0 )
    {
      _name = "MMBCKADPS";
      _dir =  "DIRBYTE60";
      _toggle = "TOGGLEBYTE60";
      _lock = "STATUS60";
      _bitsin = "BITSTO60";
      _cardaddr = 0x60;
      _togglebit = 4;
      _pulse = 2;
      _coast = 80;
      _backlash = 30; 
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 4;
       _far = 200;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4;
       _hair = 50;
       _crawl = 4;
       _offset = _backlash + 50;
    }
    else if ( motorName.compare( "mmoscB" ) == 0 )
    { 
      _type = Motor::MMOSC;
      _name = "MMOSCBPS";
      _dir =  "DIRBYTE50";
      _toggle = "TOGGLEBYTE50";
      _lock = "STATUS50";
      _bitsin = "BITSTO50";
      _cardaddr = 0x50;
      _togglebit = 5;
      _pulse = 2;
      _coast = 30;
      _backlash = 50;
      _ad_ref = "BREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 4;
       _far = 200;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4;
       _hair = 50;
       _crawl = 8;
       _offset = _backlash + 50;
    }
    else if ( motorName.compare( "mmbckB" ) == 0 )
    {
      _name = "MMBCKBPS";
      _dir =  "DIRBYTE50";
      _toggle = "TOGGLEBYTE50";
      _lock = "STATUS50";
      _bitsin = "BITSTO50";
      _cardaddr = 0x50;
      _togglebit = 4;
      _pulse = 6;
      _coast = 100;
      _backlash = 50;
      _ad_ref = "BREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 4;
       _far = 200;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4;
       _hair = 50;
       _crawl = 4;
       _offset = _backlash + 50;
    }
    else if ( motorName.compare( "attnD" ) == 0 )
    {
      _name = "ATTNDPS";
      _dir =  "DIRBYTE60";
      _toggle = "TOGGLEBYTE60";
      _lock = "STATUS60";
      _bitsin = "BITSTO60";
      _cardaddr = 0x60;
      _togglebit = 1;
      _pulse = 10;
      _coast = 250;
      _backlash = 50;
      _ad_ref = "ADREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
    // Dick claims that the old code was being too narrow and
    // that really, being within 32 counts is fine...
    // Normally, _pulse/2 is a number between 3-5... 
    // Having more leeway keeps use from missing the target so easily...
    //  _halfstep = (int)_pulse/2;
       _halfstep = 10;
       _far = 500;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4; 
       _hair = 50;
       _crawl = 4;
       _offset = _backlash + 50;
    }
    else if ( motorName.compare( "attnB" ) == 0 )
    {
      _name = "ATTNBPS";
      _dir =  "DIRBYTE50";
      _toggle = "TOGGLEBYTE50";
      _lock = "STATUS50";
      _bitsin = "BITSTO50";
      _cardaddr = 0x50;
      _togglebit = 6;
      _pulse = 10;
      _coast = 100;
      _backlash = 50;
      _ad_ref = "BREFLOPLT";
      _lockcode = ( (_cardaddr & 0x70) >> 4 ) + 0x90;
      _mask = 0x01 << _togglebit;
       _halfstep = 10;
       _far = 500;
       _fast = 0;
       _close = 100;
       _medium = 2;
       _near = 50;
       _slow = 4; 
       _hair = 50;
       _crawl = 4;
       _offset = _backlash + 50;
    }
    else
    {
      errMsg << "Unknown motor type '" << motorName << "'";
      throw CARMA_ERROR( errMsg.str() );
    }
}


void Motor::disableLock()
{
  tpoke( _lock, (unsigned char)0x00 );
  tpoke( _toggle, (unsigned char)0x00 );
  if ( _config.isEmulating() == false )
    usleep( 10000 );                             
}

void Motor::enableLock()
{
  int lockreadback;
  //tpeek( _lock, &lockreadback );              // check whether tuning already is enabled
  //tpeek's are highly unnecessary in this situation
  // as the data is being sent back at 100Hz anyway...
  // just use get data...dammit.
  getData( _lock, &lockreadback );

  CPTRACE( Trace::TRACE2, "EnableLock, _lock: " << _lock << " lockreadback: " << lockreadback );
  if ( (lockreadback & 0x10) == 0 ) {         // if not, then go through the procedure
    disableLock();
    tpoke( _lock, (unsigned char)_lockcode );
    if ( _config.isEmulating() == false )
      usleep( 10000 );
    tpoke( _toggle, (unsigned char)0xFF );
    if ( _config.isEmulating() == false )
      usleep( 10000 );	
  }
}

// Specialized setbits because this relies on 100Hz update of motor positions
// and therefore the data is always current in shared memory...
/*
   void Motor::setbits( const char *bitsin, const char *bitsout, unsigned char value )
   {
   int ibits;
   char bits;
   unsigned char ubits;

   getData( bitsin, &ibits, 1 );
   bits = (char)ibits;
   ubits = (bits & ~_mask) | (value & _mask) ;

   tpoke( bitsout, ubits );
   }
 */

void Motor::step( int num )
{
  enableToggleBit( _toggle, _mask, num ); 
}

void Motor::setDirection( unsigned char dir )
{
  if ( _type == Motor::FOCUS )
  {
    unsigned char truedir = ( dir == Motor::DOWN ) ? 0x80 : 0x00;
    tpoke( _dir, truedir );
  }
  else
  {
    setbits( _dir, dir, (char)0xFF ); // all bits get set
  }
}

void Motor::limitTarget( unsigned short& aTarget )
{ 
  if ( _type == Motor::FOCUS )
  {
    // limits motion to between [-26:26] mm
    if ( aTarget < 4500 )
      aTarget = 4500;
    else if ( aTarget > 61524 )
      aTarget = 61524;
  }
  else if ( _type == Motor::MMOSC )
  {
    if ( aTarget < 15000 )
    {
      cout << "  mmosc move target: " << aTarget << " flooring to: 15000" << endl;
      _logger << Priority::INFO << "  mmosc move target: "
	<< aTarget << " flooring to: 15000";
      aTarget = 15000;
    }
  }
  else
  {
    if ( aTarget < _lowerlimit ) {
      cout << "Changed target from " << aTarget << " to " << _lowerlimit << ", the lower limit" << endl ;
      aTarget = _lowerlimit;
    }
    else if ( aTarget > _upperlimit ) {
      cout << "Changed target from " << aTarget << " to " << _upperlimit << ", the upper limit" << endl ;
      aTarget = _upperlimit;
    }
  }
}

// step down until position is at or below the target
// slow down the step speed when close to target to minimize overshoot
void Motor::moveDown( unsigned short target )
{
  unsigned short newTarget = target;
  unsigned short pos, lastpos, newspeed, speed;
  int distance, iters;
  int icount;    

  limitTarget( newTarget );
  CPTRACE( Trace::TRACE2, getName() << " quick move above newTarget: " << newTarget
      << " pos: " << (unsigned short)position() );
  setDirection( Motor::DOWN );
  enableLock();		// returns immediately if tuning already is enabled
  lastpos = (unsigned short)65535;
  icount = 0;
  iters = 0;
  newspeed = speed = _fast;

  // Don't bother with this path if we're emulating..
  if ( _config.isEmulating() )
  {
    CPTRACE( Trace::TRACE1, " WE'RE EMULATING?" );
    return;
  }

  // note: save pos so that TRACE statement will report same position that the code sees
  while ( (pos = (unsigned short)position() ) > newTarget )
  {
    distance =  pos - newTarget;
    if ( distance > _far )
      newspeed = _fast;
    else if ( distance > _close )
      newspeed = _medium;
    else if ( distance > _near )
      newspeed = _slow;
    else 
      newspeed = _crawl;

    if (newspeed != speed)
      icount = 1000;
    speed = newspeed;

    CPTRACE( Trace::TRACE5, "position: " << pos << " speed: "
	<< speed << " iters: " << iters << " lastpos: " << lastpos );
    if (pos < lastpos)
    {
      lastpos = pos;
      iters = 0;	// reset iteration counter if motor seems to be moving up
    }
    if (iters++ > 500) {
      _logger << Priority::ERROR << "Max allowed iterations exceeded, Motor stalled?";
      printf("Motor appears to be stalled\n");
      break;
    }

    // step keeps stepping for 0.2 sec; avoid calling it each 0.01 sec
    if (icount++ > 20)
    {
      icount = 0;
      step( speed );
    }
    if ( _config.isEmulating() == false )
      usleep(10000);
  }
  disableToggleBit();     // stops the motor
  CPTRACE( Trace::TRACE5, "stop at: " << pos );
}

// step up until position is at or above the target
// slow down the step speed when close to target to minimize overshoot
void Motor::moveUp( unsigned short target )
{
  unsigned short newTarget = target;
  unsigned short pos, lastpos;
  int icount, iters, speed, newspeed;
  int distance;
  newspeed = speed = _fast;

  limitTarget( newTarget );
  CPTRACE( Trace::TRACE2, getName() << " quick move above newTarget: " << newTarget
      << " pos: " << (unsigned short)position() );
  setDirection( Motor::UP );

  CPTRACE( Trace::TRACE5, "before enableLock" );
  enableLock();		// returns immediately if tuning already is enabled
  CPTRACE( Trace::TRACE5, "after enableLock" );
  lastpos = 0;
  icount = 0;
  iters = 0;

  // Don't bother with this path if we're emulating..
  if ( _config.isEmulating() )
  {
    CPTRACE( Trace::TRACE1, " WE'RE EMULATING?" );
    return;
  }

  // note: save pos so TRACE statement reports same position that the code sees
  while ( (pos = (unsigned short)position() ) < newTarget )
  {
    distance = newTarget - pos;
    if ( distance > _far )
      newspeed = _fast;
    else if ( distance > _close )
      newspeed = _medium;
    else 
      newspeed = _slow;

    if (newspeed != speed)
      icount = 1000;		// forces immediate call to step, which will update the speed
    speed = newspeed;

    CPTRACE( Trace::TRACE5, "position: " << pos << " speed: "
	<< speed << " iters: " << iters << " lastpos: " << lastpos );
    if (pos > lastpos)
    {
      lastpos = pos;
      iters = 0;	// reset iteration counter if motor seems to be moving up
    }
    if (iters++ > 500) {
      _logger << Priority::ERROR << "Motor appears to be stalled: " << _fullname;
      printf("Motor appears to be stalled\n");
      break;
    }

    // step keeps stepping for 0.2 sec; avoid calling it each 0.01 sec
    if (icount++ > 20)
    {
      icount = 0;
      step( speed );
    }
    if ( _config.isEmulating() == false )
      usleep(10000);
  }
  disableToggleBit();
  CPTRACE( Trace::TRACE5, "stop at: " << pos );
}

// from Motor::UP means step down to reach the target
void Motor::moveToTarget( unsigned short target, StepFrom from )
{
  unsigned short pos = (unsigned short)position();

  CPTRACE( Trace::TRACE3, "Moving to target: " << target
      << " from pos: " << pos );
  /*
     cout << "moveToTarget " << target << " from " ;
     if (from == Motor::BELOW)
     cout << "below only" << endl;
     else if (from == Motor::ABOVE)
     cout << "above only" << endl;
     else
     cout << "either direction" << endl;
   */

  if ( abs( pos - target ) < 2.*_halfstep )
  {
    CPTRACE( Trace::TRACE3, "Exit moveToTarget: position " << pos 
	<< ", already is within tolerance " << 2.*_halfstep << " of target" );
  }
  else 
  {
    // if from is neither Motor::ABOVE nor Motor::BELOW, we drop through this section
    if ( pos > ( target - _offset ) && from == Motor::BELOW ) 
    {
      cout << "Moving " << _offset << " counts below target" << endl;
      moveDown( target - _offset );
    }
    else if ( pos < ( target + _offset ) && from == Motor::ABOVE )
    {
      cout << "Moving " << _offset << " counts above target" << endl;
      moveUp( target + _offset );
    }

    if ( pos > target )
      moveDown( target + _halfstep );
    else
      moveUp( target - _halfstep );
  }
}


short Motor::position()
{
  short pos = 0;

  if ( _type == Motor::RECEIVER || _type == Motor::MMOSC )
  {
    int i = atodin( _name );

    if ( i < 0 )
      i = 0;
    else if ( i > 32767 )
      i = 32766;

    pos = (short)i;
  }
  else if ( _type == Motor::FOCUS )
  {
    int ipos;
    getData( _name, &ipos, 1 );
    pos = (short)ipos;
  }

  return pos;
}

short Motor::getADMax()
{
  int admax;

  getData( _ad_ref, &admax, 1 );
  CPTRACE( Trace::TRACE4, "getADMax(): _ad_ref=" << _ad_ref << ", admax =" << admax );
  return atodin( (unsigned short)admax );
}

const char * Motor::getName()
{
  return _name;
}
