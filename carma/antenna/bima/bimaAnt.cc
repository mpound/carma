/*
 * @usage bimaScope [dewar=dewar5] [sensor=sensor1] [t=10] [of=scope.dat]
 * @key antenna @noDefault s Antenna look up table data... assumes gethostname().
 * @key az @noDefault d Move to this az angle
 * @key el @noDefault d Move to this el angle
 * @key pegvel @noDefault i Move at the given velocity
 * @key status @noDefault b Print status info.
 * @key watchazel @noDefault b Print az/el as fast as possible
 * @key watchtilts @noDefault b Print tilt1/tilt2 as fast as possible
 * @key getazoffset @noDefault i Print out Az offset given a hint in degrees, answer is in resolver counts.
 * @key geteloffset @noDefault i Print out El offset given a hint in degrees, answer is in resolver counts.
 * @key tiltaz1 @noDefault d Start a tilt measurement at the given az.
 * @key tiltaz2 @noDefault d End a tilt measurement at the given az.
 * @key tiltinc 60.0 d Increments to stop at for tilt measurements.
 * @key tiltwait 10 i Seconds to wait at each increment before measuring tilt.
 * @key unixtomjd @noDefault i Convert seconds from UNIX epoch to MJD representation.  This will get moved somewhere else.
 * @logger DEFAULT_FACILITY carma.bima.bimaAnt
 */



#include <unistd.h>
#include <sys/time.h>

// C++ Includes...
#include <iostream>
#include <sstream>
#include <cmath>

// CARMA Includes...
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include "carma/antenna/bima/Drives.h"
#include "carma/antenna/bima/AntennaNameResolver.h"

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

int Program::main()
{
  try
  {
    ostringstream oss;
    AntennaNameResolver *anr;

    if ( parameterWasSpecified( "unixtomjd" ) ) 
    {
      int s = getIntParameter( "unixtomjd" );
      cout << setprecision( 15 );
      cout << Time::computeMJD( static_cast<time_t>(s) ) << endl;
      exit(EXIT_SUCCESS);
    }

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), Program::getConfDir() );

    Drives d( config );
    bool move = false;
    double targAz, targEl;

    targAz = 0.0;
    targEl = 90.0;

    if ( parameterWasSpecified( "tiltaz1" ) )
    {
      double tiltaz1 = getDoubleParameter( "tiltaz1" );

      if ( parameterWasSpecified( "tiltaz2" ) == false )
      {
        cerr << "Must specify tiltaz2 when specifying tiltaz1" << endl;
        exit(EXIT_FAILURE);
      }
     
      double tiltaz2 = getDoubleParameter( "tiltaz2" );
      double tiltinc = getDoubleParameter( "tiltinc" );
      int tiltwait = getIntParameter( "tiltwait" );

      d.setTargetAz( 0, 0, tiltaz1, true );
      d.pegEl( true ); // make sure El does not move

      cerr << "Moving to starting Az: " << tiltaz1 << endl;
      while ( fabs( tiltaz1 - d.getAz() ) > .05 )
      {
        d.setState( Drives::SLEW );
        d.toggle();
        usleep( 800000 ); // magical .8 sec
      }
      
      d.stop();

      double newTarg, lastTarg;
      double ave1, ave2;
      int rms1, rms2;

      cout << "# Az  1Ave 1RMS 1outliers 2Ave 2RMS 2outliers" << endl;

      while ( fabs(tiltaz2 - d.getAz()) > tiltinc )
      {
        newTarg = d.getAz() + tiltinc;
        cerr << "Reached: " << d.getAz() << " allowing telescope to settle for " << tiltwait
             << ((tiltwait > 1) ? "seconds" : "second") << endl;
        sleep( tiltwait );
        
        int outliers1, outliers2;

        d.getTiltCounts( "TILT1SAMPLES", outliers1, rms1 );
        ave1 = d.getTilt1Arcmin();
        d.getTiltCounts( "TILT2SAMPLES", outliers2, rms2 );
        ave2 = d.getTilt1Arcmin();

        cout << d.getAz() << " " << ave1 << " " << (rms1*VOLTS4*13.3) << " " << outliers1 
             << " " << ave2 << " " << (rms2*VOLTS4*13.3) << " " << outliers2<< endl;
        
        d.setTargetAz( 0, 0, newTarg, true );

        while ( fabs( newTarg - d.getAz() ) > .5 )
        {
          d.setState( Drives::SLEW );
          d.toggle();
          usleep( 800000 ); // magical .8 sec
        }
        d.stop();

        lastTarg = newTarg;
      }

    }

    if ( parameterWasSpecified( "getazoffset" ) )
    {
      int hint = getIntParameter( "getazoffset" );

      cout << "Computing Coarse Azimuth offset based on hint: " << hint << " deg" << endl;
      cout << "Az Offset: " << d.computeAzCoarseOffset( hint ) << endl;
    }

    if ( parameterWasSpecified( "geteloffset" ) )
    {
      int hint = getIntParameter( "geteloffset" );

      cout << "Computing Coarse Elevation offset based on hint: " << hint << " deg" << endl;
      cout << "El Offset: " << d.computeElCoarseOffset( hint ) << endl;
    }

    if ( parameterWasSpecified( "status" ) )
    {
      cout << "Antenna status info: " << endl;

      cout << "   Key is: " << ( d.isKeyOn() ? "ON" : "OFF" ) << endl;
      cout << "   Manual/Fault bits: " << ( d.areFaultBits() ? "YES" : "NONE" ) << endl;
    }

    if ( parameterWasSpecified( "pegvel" ) )
    {
      d.pegVelocities( getIntParameter( "pegvel" ) ); // force velocity magnitudes to be this
    }

    if ( parameterWasSpecified( "watchazel" ) )
    {
      cout << "# az el digvelaz digvelel" << endl;
      while (1)
      {
	cout << d.getAz() << " " << d.getEl() << " " << d.getAzDigVeFilter() << " " 
	  << d.getElDigVeFilter() << "\n";
	usleep(20000);
      }
    }
    
    if ( parameterWasSpecified( "watchtilts" ) )
    {
      cout << "# tilt1counts tilt2counts tilt1arcmin tilt2arcmin" << endl;

      while (1)
      {
        cout << d.getTilt1Counts() << " " << d.getTilt2Counts() << " "
             << d.getTilt1Arcmin() << " " << d.getTilt2Arcmin() << endl;
        usleep( 20000 );
      }
    }
    
    double now = carma::util::Time::MJD();
    if ( parameterWasSpecified( "az" ) )
    {
      targAz = getDoubleParameter( "az" );
      d.setTargetAz( 0, now, targAz, true );
      move = true;
    }
    else
    {
      d.setTargetAz( 0, now, d.getAz(), true );
      d.pegAz( true );
    }
      
    if ( parameterWasSpecified( "el" ) )
    {
      targEl = getDoubleParameter( "el" );
      d.setTargetEl( now, targEl, true );
      move = true;
    }
    else
    {
      d.setTargetEl( now, d.getEl(), true );
      d.pegEl( true );
    }

    if ( move )
    {
      CPTRACE( Trace::TRACE3, "Moving!  New Targets, az: " << targAz << " el: " << targEl );
      d.setState(Drives::TRACK);
      cout << "# az el azvel elvel" << endl;
    }
     
    while ( move )
    {
      move = false;

      d.toggle();

      usleep( 100000 ); 

      if ( d.isAzPegged() == false && fabs( targAz - d.getAz() ) > .0005 )
        move = true;

      if ( d.isElPegged() == false && fabs( targEl - d.getEl() ) > .0005 )
        move = true;

      cout << std::setprecision(15) << std::left << std::setw(17)
  	<< d.getAz() << " " << d.getEl() << " " << (int)d.getAzVel() << " " << (int)d.getElVel() << endl;
    }

  }
  catch ( carma::util::ErrorException &cuue )
  {
    cout << "Caught exception: " << cuue.what() << endl;
  }

  exit(EXIT_SUCCESS);
}

