/*
 * @usage blah
 * @key blah "blah" s Blah
 * @logger TEST_FACILITY carma.test.antenna.bima.testMotor
 */

#include <unistd.h>

#include <iostream>
#include <ostream>
#include <carma/antenna/bima/TelemetryClient.h>
#include <carma/antenna/bima/Motor.h>
#include <carma/util/Program.h>

using namespace std;
using namespace carma::antenna::bima;

int carma::util::Program::main()
{

  try
    {
//      TelemetryClient::TelemetryClient tmc(new std::string("conf/antenna/bima/telemetry.xml"));
//      Motor motor( "mmbck_b", "", 0x50, 4, 0xF8, 0x00, 2, 8, 10 );
//      Motor motor( "cal", "", 0x60, 0, 0x106, 0x00, 0, 8, 10 );
      int pos;

      while ( 1 )
      {  
//        motor.getData( "MMBCKBPS", &pos, 1 );
//        pos = pos - 32768; // short int scaling
//        cout << "moving to 30000, current: " << dec << pos << endl;
//        motor.moveToTarget( 30000 );

//        motor.moveToTarget( 5000 );

      }

    }
  catch ( const carma::util::BaseException & be )
    {
      cerr << be.what() << endl;
    }

  exit(1);
}

