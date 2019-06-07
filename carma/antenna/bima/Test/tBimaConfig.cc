//
// @version	$Id: tBimaConfig.cc,v 1.6 2011/02/14 15:29:03 mpound Exp $
//
// @usage	tBimaConfig
//
// @description
//      Tests if the BIMA antenna configuration tables can
//      be properly loaded and if they appear to have
//      self-consistent data in them.
//      This occurrs by first instantiating a copy of the
//      Configuration object and then using it to instantiate
//      the Rx, Dewars and Drives objects, each of which in turn
//      use information that came from the Configuration object.
//      If the information is not self-consistent (e.g. the
//      entry in conf/antenna/bima/desc.tab for bima1 refers
//      to a non-existent dewar name in conf/antenna/bima/rx/dewars.tab
//      this test should catch that error.
//      This test iterates through entries that *must* be
//      in conf/antenna/bima/desc.tab and so on (a1 is the berkeley
//      lab antenna test computer, then bima1-bima9)
//      Another implicit test performed here is the functioning
//      of the AntennaNameResolver object, which is used to
//      instantiate copies of the Configuration object.
//
// @noKeys
//
// @logger DEFAULT_FACILITY carma.antenna.bima.Test.tBimaConfig

// CARMA
#include "carma/util/Program.h"

// BIMA Specific
#include "carma/antenna/bima/Configuration.h"
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/antenna/bima/Dewar.h"
#include "carma/antenna/bima/Drives.h"

// C++ STL
#include <string>
#include <vector>

#include <pthread.h>

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      static void *testConfig( void *name )
      {
	try
	{
	  AntennaNameResolver anr( static_cast<const char *>(name) );
	  Configuration config( anr.getAntennaName(),
	      ::carma::util::Program::getConfDir() );
	  config.setEmulate( true );

	  // This ensures there is a shared memory file
	  // in place when attempting to instantiate
	  // the Rx/Dewar/Drives objects afterward
	  SharedMemory placeholder( static_cast<const char *>(name) ); 
	  placeholder.init();
	  //

	  // If these three objects instantiate without
	  // throwing exceptions, then these tests have
	  // almost worked!
	  Rx rx( config );
	  Dewar dewar( config );
	  Drives drives( config );
	  //

	  // Now final check
	  rx.loadSISTable( dewar.getSISMixerBConfFile() );
	  rx.loadSISTable( dewar.getSISMixerDConfFile() );

	  // Print out the configurations for posterity
	  cout << config << rx << dewar << drives << endl;
	  //

      return 0;
	}
	catch (const ::carma::util::BaseException& e)
	{
	  cerr << "testConfig("<<name<<") caught exception: " << e.getMessage() << endl;
	}
	catch (...)
	{
	  cerr << "testConfig("<<name<<") caught unknown exception..." << endl;
	}
      return 0;

      }

    } // bima
  } // antenna
} // carma


int Program::main()
{
  int status = EXIT_FAILURE; // default to failed

  try
  {
    // Build list of entries that *must* work
    vector<string> names;
    names.push_back( string( "a1" ) );
    names.push_back( string( "bima1" ) );
    names.push_back( string( "bima2" ) );
    names.push_back( string( "bima3" ) );
    names.push_back( string( "bima4" ) );
    names.push_back( string( "bima5" ) );
    names.push_back( string( "bima6" ) );
    names.push_back( string( "bima7" ) );
    names.push_back( string( "bima8" ) );
    names.push_back( string( "bima9" ) );

    vector<string>::iterator i;
    pthread_t tThreads[names.size()];
    string a;
    int j = 0;

    cout << " Launching separate threads for each config check..." << endl << "    ";
    for ( i = names.begin(); i != names.end(); ++i )
    {
      a = *i;
      cout << a << " ";
      status = pthread_create( &tThreads[j++], NULL, testConfig,
                              (void *)strdup(a.c_str()) );

      if ( status != 0 ) {
        cerr << "Thread " << a << " creation returned failing status "
             << status <<endl;
        return status;
      } //else {
        //  cerr << "OK" <<endl;
      //}
    }

    cout << endl;

    cout << " Waiting for threads to complete" << endl;
    int s;
    for ( int n = 0; n < (int)names.size(); n++ )
    {
      pthread_join( tThreads[n], (void **)&s );
    }

    status = EXIT_SUCCESS; // Passed!
    cerr << " returning status " << status <<endl;
    return status;
  }
  catch (const carma::util::BaseException& e)
  {
    cerr << "Program::main() caught exception: " << e.getMessage() << endl;
  }
  catch (...)
  {
    cerr << "Program::main() caught unknown exception: " << endl;
  }

  cerr << " returning final status " << status << endl;
  return EXIT_SUCCESS;
}
