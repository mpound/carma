/** @file
 * Program::main implementation for the  BIMA "value" program 
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.27 $
 * $Date: 2011/12/20 22:25:35 $
 * $Id: bimaValue.cc,v 1.27 2011/12/20 22:25:35 scott Exp $
 */

// System includes
#include <unistd.h>
#include <time.h>

// C++
#include <iostream>
#include <iosfwd>
#include <string>
#include <vector>

#include <stdlib.h>

#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/TelemetryClient.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Time.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::antenna::bima;

/** 
 * @version $Revision: 1.27 $
 *
 * @usage Usage: bimaValue name=shmname [tmconfig=filename.xml] [shmfile=/tmp/bima_shared_memory]
 * Example: bimaValue name=HEATER2 
 *
 * @description
 * Historically, a program called 'value' allowed BIMA users/developers to query
 * information out of the shared memory.  This is the equivilent program.
 *
 * @key name shmname s Key for name=value pair.  Multiple names can be specified with name=name1,name2,name3,...,nameN.  If multiple names are specified, plot format is forced true.
 * @key plot @noDefault b If true, writes values out in a plottable format.
 * @key value @noDefault s Value for setting the value of the variable specified by name=...
 * @key tpeek @noDefault b Read raw bytes from given position, reads four bytes
 * @key update 0 i Will update value continuously, update is units of 100 milliseconds
 * @key time  @noDefault b Write timestamp out in MJD
 * @key tmconfig filename.xml s Optional key for file that holds CANBus message types->Telemetry mapping
 * @key antenna @noDefault s Name of antenna...Used to construct location of shared memory file
 *                           Uses gethostname() if not specified.
 * @key atodin  false b Interpret integer values with the 'atodin' function before output
 * @logger DEFAULT_FACILITY carma.bima.bimaValue
 */ 
int carma::util::Program::main()
{
  try
  {
    AntennaNameResolver *anr;
    string antName;

    if ( parameterWasSpecified( "antenna" ) ) {
      anr = new AntennaNameResolver( getStringParameter( "antenna" ).c_str() );
      antName = anr->getAntennaName();
    } 
    else { 
      anr = new AntennaNameResolver();
      antName = anr->getAntennaName() ;
      // new code below converts from new to old antenna names, e.g., "c7" -> "bima1"
      const char* antInput = antName.c_str();
      char antType;
      int antNumber;
      sscanf( antInput, "%c%d", &antType, &antNumber) ;
      if (antType == 'c') {
        ostringstream output;
        output  << "bima" << (antNumber - 6) ;
        antName = output.str() ;
      }
      cout << "# " << anr->getAntennaName() << " -> " << antName << endl;
    }
        
    Configuration config( antName, getConfDir() );
    SharedMemory shmBima( config.getAntenna().c_str() );

    TelemetryClient *tmc = 0;
    bool atodin = getBoolParameter( "atodin" );
    if ( atodin )
      tmc = new TelemetryClient( config );

    int update = getIntParameter( "update" );
    std::string name = getStringParameter("name");
    vector<string> names;
    int n;

    string namesline = name;
    // tokenize name=name1,name2,name3
    while ( (n = name.find_first_of( "," ) ) > -1 )
    {
      names.push_back( name.substr( 0, n ) );
      name = name.substr( n+1, name.length() );
    }
    names.push_back( name );


    char *byteValue = NULL;
    int *intValue = NULL;
    unsigned short *shtValue = NULL;
    float *floatValue = NULL;
    double *doubleValue = NULL;
    bool timestamp = false;
    bool plot = false;

    if ( parameterWasSpecified( "time" ) )
      timestamp = getBoolParameter( "time" );

    if ( parameterWasSpecified( "plot" ) )
      plot = getBoolParameter( "plot" );

    if ( names.size() > 1 )
      plot = true;

    if ( parameterWasSpecified( "tpeek" ) )
    {
      unsigned short value;

      if ( tmc == NULL )
	tmc = new TelemetryClient( config );

      tmc->tpeek( name.c_str(), &value );
      cout << "Sent tpeek for name: " << name << " value: " << hex << value << endl;

      exit(EXIT_SUCCESS);
    }

    if ( name != "" )
    {
      vector<char> ttype;
      vector<int> tsize;


      if ( plot )
	cout << "# " << ( timestamp ? "MJD," : "" ) << namesline << endl;

      if ( parameterWasSpecified( "value" ) )
      {
	string a = getStringParameter( "value" );
	int base = 10;
	const char *n = names[0].c_str();

	// hacky hack
	char t;
	int s = 0;
	shmBima.getVariableInfo( n, &t, &s );

	if ( t == USHT_TYPE || t == INT_TYPE )
	{
	  if ( n[0] == '0' )
	    base = 16;

	  int v = (int)strtol( a.c_str(), NULL, base );

	  if ( t == USHT_TYPE )
	  {
	    unsigned short us = (unsigned short)v;
	    shmBima.putData( n, &us, 1 );
	  }
	  else
	    shmBima.putData( n, &v, 1 );
	}
	else if ( t == FLOAT_TYPE || t == DOUBLE_TYPE )
	{
	  // Very hacky, no checking on conversion!
	  double ds = strtod( a.c_str(), NULL );

	  if ( t == FLOAT_TYPE )
	  {
	    float fs = (float)ds;
	    shmBima.putData( n, &fs, 1 );
	  }
	  else
	    shmBima.putData( n, &ds, 1 );
	}
	else if ( t == CHAR_TYPE )
	{
	  shmBima.putData( n, a.c_str(), s );
	}
      }
      else
      {
	static bool first;
	int msize;

	cout.precision(12);

	for ( int i = 0; i < (int)names.size(); i++)
	{
	  int s = 0;
	  char t;
	  shmBima.getVariableInfo( names[i].c_str(), &t, &s );
	  ttype.push_back( t );
	  tsize.push_back( s );
	}

	do
	{
	  if ( timestamp )
	    cout << (double)carma::util::Time::MJD() << " ";

	  first = true;

	  for ( int i = 0; i < (int)names.size(); i++ )
	  {
	    char type; 
	    int size;
	    string name;

	    type = ttype[i];
	    size = tsize[i];
	    name = names[i];

	    if ( type == CHAR_TYPE )
	    {
	      msize = (size+1) * sizeof(char);
	      if ( (byteValue == NULL) && (byteValue = (char *)malloc(msize)) == NULL )
		throw CARMA_ERROR( "Unable to allocate memory for byteValue" );

	      shmBima.getData( name.c_str(), byteValue, size );

	      if ( plot == false )
		cout << name.c_str() << " (char)=";

	      for ( int i = 0; i < size; i++ )
		cout << (char)byteValue[i];
	    }
	    else if ( type == USHT_TYPE ) 
	    {
	      msize = (size+1) * sizeof(unsigned short);
	      if ( (shtValue == NULL) && (shtValue = (unsigned short *)malloc(msize)) == NULL )
		throw CARMA_ERROR( "Unable to allocate memory for shtValue" );

	      shmBima.getData( name.c_str(), shtValue, size );
	      for ( int i = 0; i < size; i++)
	      {
		if ( plot == false )
		  cout << name.c_str() << " (usht)=";

		cout << (unsigned short)shtValue[i];

		if ( size > 1 ) cout << " ";
	      }
	    } 
	    else if ( type == INT_TYPE )
	    {
	      msize = (size+1) * sizeof(int);
	      if ( (intValue == NULL) && (intValue = (int *)malloc(msize)) == NULL )
		throw CARMA_ERROR( "Unable to allocate memory for intValue" );

	      shmBima.getData( name.c_str(), intValue, size );

	      if ( atodin )
		*intValue = (int)tmc->atodin( *intValue );

	      for ( int i = 0; i < size; i++)
	      {
		if ( plot == false )
		  cout << name.c_str() << " (int)=";

		cout << (int)intValue[i];
		if ( size > 1 ) cout << " ";
	      }
	    }
	    else if ( type == FLOAT_TYPE )
	    {
	      msize = (size+1) * sizeof(float);
	      if ( (floatValue == NULL) && (floatValue = (float *)malloc(msize)) == NULL )
		throw CARMA_ERROR( "Unable to allocate memory for floatValue" );

	      shmBima.getData( name.c_str(), floatValue, size );
	      for ( int i = 0; i < size; i++)
	      {
		if ( plot == false )
		  cout << name.c_str() << " (float)=";

		cout << (float)floatValue[i];
		if ( size > 1 ) cout << " ";
	      }
	    }
	    else if ( type == DOUBLE_TYPE )
	    {
	      msize = (size+1) * sizeof(double);
	      if ( (doubleValue == NULL) && (doubleValue = (double *)malloc(msize)) == NULL )
		throw CARMA_ERROR( "Unable to allocate memory for doubleValue" );

	      shmBima.getData( name.c_str(), doubleValue, size );
	      for ( int i = 0; i < size; i++)
	      {
		if ( plot == false )
		  cout << name.c_str() << " (double)=";

		cout << (double)doubleValue[i];
		if ( size > 1 ) cout << " ";
	      }
	    }

	    if ( plot )
	      cout << " ";
	    else
	      cout << "    ";

	  } // for loop over names vector
	  // no usleep if set to -1
	  if ( update > 0 )
	    usleep( 10000 * update );

	  if ( plot )
	    cout << endl;
	  else
	    cout << (char)13;

	  cout.flush();
	  first = false;

	}
	while ( update );

	cout << endl;

      } // if value
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

