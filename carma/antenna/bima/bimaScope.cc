/*
 * @usage bimaScope [dewar=dewar5] [sensor=sensor1] [t=10] [of=scope.dat]
 * @key antenna @noDefault s Antenna look up table data...
 * @key sensor "sensor1" s Sensor to take data.
 * @key t 10 i Amount of time to take data.
 * @key of "scope.dat" s Output file.
 * @key tmconfig "antenna/bima/telemetry.xml" s Default telemetry description.
 * @logger DEFAULT_FACILITY carma.bima.Scope
 */


#include <unistd.h>
#include <sys/time.h>

// C++ Includes...
#include <iostream>
#include <sstream>

// CARMA Includes...
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/Dewar.h"

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

int Program::main()
{
  try
  {
    ostringstream oss;
    int t = getIntParameter( "t" );
    string sensor = getStringParameter( "sensor" );
    string of = getStringParameter( "of" );
    vector<double> data;
    vector<double> timestamp;

    data.reserve( (100 * t) + 1 );
    timestamp.reserve( (100 * t) + 1 );

    AntennaNameResolver *anr;

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), getConfDir() );

    Dewar d( config );

    int s = 0;

    if ( sensor.compare( "sensor1" ) == 0 )
      s = 1;
    else if ( sensor.compare( "sensor2" ) == 0  )
      s = 2;
    else if ( sensor.compare( "sensor3" ) == 0  ) 
      s = 3;
    else if ( sensor.compare( "sensor4" ) == 0 ) 
      s = 4;
    else if ( sensor.compare( "sensor5" ) == 0 ) 
      s = 5;
    else
    {
      cerr << "Invalid sensor selection, must be sensor1, ..., sensor5" << endl;
    }

    cout << config << endl;
    cout << "Taking " << t << " second" << ( t > 1 ? "s" : "" )
         << " of data on sensor " << sensor << endl;

    struct timeval tv;
    struct timezone tz;
    long cs, ts = 0, us; 
    double datum;

    gettimeofday( &tv, &tz );
    cs = tv.tv_sec;
    double tus = 0;
    double cus = ( tv.tv_usec / 1000000. );
  
    usleep( 10000 );

    while( (ts < (cs + t)) )
    {
      if ( cus > tus )
      {
        if ( s == 1 )
          datum = d.stage1temp();
        else if ( s == 2 )
          datum = d.stage2temp();
        else if ( s == 3 )
          datum = d.stage3temp();
        else if ( s == 4 )
          datum = d.stage4temp();
        else if ( s == 5 )
          datum = d.stage5temp();

        data.push_back( datum );
        timestamp.push_back( cus );
        tus = cus + .01;
      }

      gettimeofday( &tv, &tz );
      ts = tv.tv_sec;
      us = tv.tv_usec;
      cus = (double)(ts - cs) + ( us / 1000000. );
    }

    cout << "Finished collecting, outputting to file " << of << endl;

    ofstream ofs( of.c_str(), ios::trunc );

    if ( ! ofs )
      cerr << "unable to create file " << of.c_str() << endl;

    ofs.precision(7);

    int i;
    for ( i = 0; i < (int)data.size(); i++ )
      ofs << timestamp.at(i) << " " << data.at(i) << endl;

    ofs.flush();
    ofs.close();
    cout << "Output " << i << " data points." << endl;
  }
  catch ( const carma::util::ErrorException & ee )
  {
    cerr << ee.what() << endl;
  }

  exit(1);
}

