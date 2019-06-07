

#include <iostream>
#include <ostream>
#include <carma/antenna/bima/SharedMemory.h>
#include <carma/util/Program.h>
#include <cmath>
#include <limits>


using namespace std;
using namespace carma::antenna::bima;

int carma::util::Program::main(){ return 1;};



int main ( void )
{
  SharedMemory::SharedMemory shM;
  char testString[80];
  int anint = 12;
  int ints[5] = { 1, 2, 3, 4, 5 };
  int testInts[5];
  float afloat = 666.0;
  float floats[5] = { 6.0, 7.0, 8.0, 9.0 };
  float testFloats[5];
  double adouble = 666.0;
  double doubles[5] = { 6.0, 7.0, 8.0, 9.0 };
  double testDoubles[5];
  
  shM.init();

  shM.putCString( "testStr", "Testing" );
  shM.putInts( "testInt", &anint, 1 ); 
  shM.putInts( "testInts", ints, 5 );
  shM.putFloats( "testFlt", &afloat, 1 ); 
  shM.putFloats( "testFlts", floats, 5 );
  shM.putDoubles( "testDbl", &adouble, 1 ); 
  shM.putDoubles( "testDbls", doubles, 5 );

  shM.getCString( "testStr", testString, 20 );
  cout << "getCString( \"testStr\" ): " << testString << endl;

  shM.getInts( "testInt", testInts, 1 );
  cout << "getInts( \"testInt\" ): " << testInts[0] << endl;

  shM.getInts( "testInts", testInts, 5 );
  cout << "getInts( \"testInts\" ): ";
  for ( int i=0; i < 5; i++)
    cout << testInts[i] << " ";
  cout << endl;

  shM.getFloats( "testFlt", testFloats, 1 );
  cout << "getFloats( \"testFlt\" ): " << testFloats[0] << endl;

  shM.getFloats( "testFlts", testFloats, 5 );
  cout << "getFloats( \"testFlts\" ): ";
  for ( int i=0; i < 5; i++)
    cout << testFloats[i] << " ";
  cout << endl;

  shM.getDoubles( "testDbl", testDoubles, 1 );
  cout << "getDoubles( \"testDbl\" ): " << testDoubles[0] << endl;

  shM.getDoubles( "testDbls", testDoubles, 5 );
  cout << "getDoubles( \"testDbls\" ): ";
  for ( int i=0; i < 5; i++)
    cout << testDoubles[i] << " ";
  cout << endl;


  exit(1);
}

