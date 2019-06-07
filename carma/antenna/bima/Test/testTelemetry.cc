
/*
 * @usage [antenna=bima1]
 * @key antenna "bima1" s Antenna...
 * @logger TEST_FACILITY carma.test.antenna.bima.testTelemetry
 */ 

#include <unistd.h>

#include <iostream>
#include <ostream>
#include <carma/antenna/bima/TelemetryClient.h>
#include <carma/antenna/bima/Configuration.h>
#include <carma/util/Program.h>

using namespace std;
using namespace carma::antenna::bima;

int carma::util::Program::main()
{

  try
    {
      string antenna = getStringParameter( "antenna" );
      Configuration config( antenna, getConfDir() );
      TelemetryClient tmc( config );
      /*
      char band = 'B';
      char index = band - 'A';
      int mask = 0x000c;
      int phbits[5] = { 0, 1, 2, 0, 1 };
      int bitsout = phbits[index] << 2;
      
      int bits = 0;
      bits = (bits & ~mask) | (index & mask);

      tmc.tpoke( "BANDSELECT", 0 );
      tmc.tpoke( "SISSET", 0x10 );
      tmc.tpoke( "YIG_SELECT", 0xff );
      */

      cout << "About to enable" <<endl;
      tmc.enableMonitorPackets();
      cout << "enabled" <<endl;
      sleep(3);
      cout << "About to disable" <<endl;
      tmc.disableMonitorPackets();
      cout << "Disabled" <<endl;

    }
  catch ( const carma::util::BaseException & be )
    {
      cerr << be.what() << endl;
    }

  exit(1);
}

