/** @file
 * Time conversion utility program
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.5 $
 * $Date: 2007/12/17 22:37:47 $
 * $Id: timeConvert.cc,v 1.5 2007/12/17 22:37:47 abeard Exp $
 */

// System includes
#include <unistd.h>
#include <time.h>

// C++
#include <iostream>
#include <iomanip>
#include <iosfwd>
#include <string>

#include <stdlib.h>

#include "carma/util/Time.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

using namespace std;
using namespace carma::util;

/** 
 * @version $Revision: 1.5 $
 *
 * @usage timeConvert [format=\"...\"] [tomjd=\"...\"] [instantmjd=...] [frommjd=...]
 *
 * @description
 *  This program is a utility to convert dates to and from MJD.  All times are
 *  in terms of GMT.  The default time format string is \"%Y %b %d %H:%M:%S\"
 *
 *  Examples:
 *    timeConvert instantmjd=t                            # returns instantaneous MJD
 *    timeConvert tomjd=\"2007 Jan 01 12:00:00\"            # returns MJD
 *    timeConvert format=\"%Y %m %d\"  tomjd=\"2007 01 01\"   # returns MJD
 *    timeConvert frommjd=54001                           # returns date string
 *    timeConvert format \"%Y %m %d\" frommjd=54001         # returns formatted date string
 *    timeConvert tomjd=\"2007 Jan 01 00:00:00\" modsecs=-1   # returns date -1 second
 *
 * @key format "%Y %b %d %H:%M:%S" s Default format see \"man date\" for more info
 * @key tomjd  @noDefault s Convert time to MJD (using format)
 * @key toframe @noDefault s Convert time to Carma frame time (using format)
 * @key instantmjd false b Get instantaneous MJD and print 
 * @key instantframe false b Get instantaneous Carma frame time and print
 * @key frommjd @noDefault d Convert MJD to formatted string
 * @key fromframe @noDefault i Convert Carma frame to formatted string
 * @key modsecs @noDefault d Modify via seconds delta
 * @key moddays @noDefault d Modify via days delta
 * @key precision 6 i MJD precision to print.  6 provides 1/10th of a second. Rounds up!
 *
 * @logger DEFAULT_FACILITY carma.util.timeConvert
 */ 

int carma::util::Program::main()
{
  try
  {
    string format = getStringParameter( "format" );
    int precision = getIntParameter( "precision" );
    double modsecs = 0.0;

    cout.setf( ios::fixed );
    cout << setprecision( precision );

    if ( parameterWasSpecified( "moddays" ) )
    {
      modsecs = getDoubleParameter( "moddays" );
    }

    if ( parameterWasSpecified( "modsecs" ) )
    {
      modsecs += (getDoubleParameter( "modsecs" ) / Time::SECONDS_PER_DAY);
    }


    if ( getBoolParameter( "instantmjd" ) )
    {
      double instmjd = Time::MJD();
      cout << (instmjd + modsecs)  << endl;
      return EXIT_SUCCESS;
    }

    if ( getBoolParameter( "instantframe" ) )
    {
      const double modmjd = Time::MJD() + modsecs;
      const frameType instframe = Time::computeFrame( modmjd );
      cout << instframe << endl;
      return EXIT_SUCCESS;
    }

    if ( parameterWasSpecified( "tomjd" ) )
    {
      string tomjd = getStringParameter( "tomjd" );
      double mjd = Time::computeMJD( tomjd, format, Time::GMT ) + modsecs;
      cout << mjd << endl;
      return EXIT_SUCCESS;
    }

    if ( parameterWasSpecified( "toframe" ) )
    {
      const string toframe = getStringParameter( "toframe" );
      const double modmjd = 
        Time::computeMJD( toframe, format, Time::GMT ) + modsecs;
      const frameType frame = Time::computeFrame( modmjd );
      cout << frame << endl;
      return EXIT_SUCCESS;
    }

    if ( parameterWasSpecified( "frommjd" ) )
    {
      double frommjd = getDoubleParameter( "frommjd" ) + modsecs;
      cout << Time::getDateTimeString( frommjd, 2, format ) << endl;
      return EXIT_SUCCESS;
    }

    if ( parameterWasSpecified( "fromframe" ) )
    {
      const int fromframe = getIntParameter( "fromframe" );
      const double frommjd = Time::MJD( fromframe ) + modsecs;
      cout << Time::getDateTimeString( frommjd, 2, format ) << endl;
      return EXIT_SUCCESS;
    }
  }
  catch ( carma::util::ErrorException& err )
  {
    cerr << err.what() << endl;
    return EXIT_FAILURE;
  }
  catch ( ... )
  {
    cerr << "Program threw unidentified exeception." << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
};

