/*
 * @usage bimaDriveReplay
 * 	  Expects cansniffer produced file with time=t (Not MJD)
 * @key antenna "a1" s Antenna look up table data...
 * @key if @noDefault s Input file (output of cansniffer).
 * @key tmconfig "antenna/bima/telemetry.xml" s Default telemetry description.
 * @logger DEFAULT_FACILITY carma.bima.DriveReplay
 */


#include <unistd.h>
#include <string.h>
#include <sys/time.h>

// C++ Includes...
#include <iostream>
#include <sstream>

// Xerces includes
#include <xercesc/parsers/SAXParser.hpp>
#include <xercesc/validators/DTD/DTDValidator.hpp>

// CARMA Includes...
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include "carma/canbus/Utilities.h"

#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/TelemetryConfigHandler.h"
#include "carma/antenna/bima/Drives.h"

using namespace std;
using namespace carma::util;
using namespace carma::canbus;
using namespace carma::antenna::bima;

static SAXParser::ValSchemes validation = SAXParser::Val_Auto;

namespace 
{
  void convertToCannishness( const char *line, string &ptime, int &mid,
      vector<carma::canbus::byteType> &data )
  {
    ostringstream oss;
    string strline( line );

    data.resize(0);

    // time stamp
    ptime = string( strline.substr( 0, 12 ) );

    // mid
    mid = (int)strtol( strline.substr( 44, 5 ).c_str(), NULL, 16 );

    for ( int i = 67; i < 91; i+=3 )
    {
      data.push_back((byteType)((int)strtol(
	      strline.substr( i, 2 ).c_str(), NULL, 16 )));
    }

  }

  void interpretMID( string ptime, int mid, Drives &d )
  {
    //static int atomicfastupdate = 0;

    switch ( mid )
    {
      case 0x100:
	// AZ related
	cout << setprecision(9);
	cout << ptime << " " << d.getAz() << " " << d.getAzDigVeFilter();
	break;
      case 0x101:
	// EL related
	cout << setprecision(9);
	cout << " " << d.getEl() << " " << d.getElDigVeFilter();
	cout << endl;
	break;
    }
  }

} // anonymous namespace

int Program::main()
{
  int processingLine = 0;
  try
  {
    ostringstream oss;
    AntennaNameResolver *anr;
    TelemetryInfo *tInfo;

    if ( parameterWasSpecified( "antenna" ) )
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
    else
      anr = new AntennaNameResolver();

    Configuration config( anr->getAntennaName(), getConfDir() );
    SharedMemory shm( config.getAntenna().c_str() );
    shm.init();

    Drives drives( config, false ); // don't connect up to TelemetryHost

    ifstream *replayFile; 
    string fname;

    if ( parameterWasSpecified( "if" ) )
    {
      fname = getStringParameter( "if" );
      replayFile = new ifstream( fname.c_str() );
    }
    else
      throw CARMA_ERROR(
	  "Must include input file on command line e.g. if=test.dat" );

    
    if ( replayFile->is_open() )
    {
      char line[151];

      XMLPlatformUtils::Initialize();
      DTDValidator *validator = new DTDValidator;
      SAXParser *parser = new SAXParser( validator );

      validator->setErrorReporter( parser );

      TelemetryConfigHandler *tmConfig = new TelemetryConfigHandler();
      parser->setDocumentHandler( tmConfig );
      parser->setErrorHandler( tmConfig );
      parser->setValidationScheme( validation );
      parser->parse( config.getTelemConfFile().c_str() );

      int mid;
      string ptime;
      vector<carma::canbus::byteType> data;

      cout << "# TIME, AZ, AZVELF, EL, ELVELF" << endl;

      while (! replayFile->eof() )
      {
	replayFile->getline(line, 150);

	if ( replayFile->eof() )
	  break;

	processingLine++;

	convertToCannishness(line, ptime, mid, data );

	// cout << "api: " << api << " mid: 0x" << (hex) << mid;
	// for ( int i = 0; i < 8; i++ )
	//  cout << " " << (int)data[i];

	// cout << (dec) << endl;

	if ( (tInfo = tmConfig->getInfo( mid )) != (TelemetryInfo *)0 )
	{
	 //  cout << "  mid: " << tInfo->getPacketName() << endl;

	  int len = tInfo->getLength();

	  for ( int i = 0; i < len; i++ )
	  {
	    int value;

	    switch ( tInfo->getSize(i) )
	    {
	      case 1:
		value = (int)dataToUbyte( data );
		break; 
	      case 2:
		value = (int)dataToUshort( data );
		break;
	    }

	  shm.putData( tInfo->getName(i), &value, 1 );
	  }
	}
	else
	{
	  // cerr << "  mid: " << mid << " no match" << endl;
	}

	// Now do something sensible
	interpretMID( ptime, mid, drives );
      }
    }
    else
    {
      oss << "Unable to open file: '" << fname << "': " << strerror(errno) << endl;
      throw CARMA_ERROR( oss.str().c_str() );
    }

    cout << endl << "#  Processed " << processingLine << " line"
      << (processingLine > 1 ? "s" : "" )
      << endl;

  }
  catch ( const carma::util::ErrorException & ee )
  {
    cerr << ee.what() << endl;
    cerr << " Processing data file line: " << processingLine << endl;
  }

  exit(1);
}

