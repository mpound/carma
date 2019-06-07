#include "carma/util/Program.h"
#include <cstdio>
#include <iostream>
#include <math.h>

using namespace std;
using namespace carma::util;

// the following doxygen-based tags are still in design. Names and format can change

//
// @version	$Revision: 1.36 $ $Date: 2010/02/22 19:43:23 $ 
//
// @usage	make a Plummer model, with a spatial or mass cut-off
//
// @description
//	mkplummer builds a nbody system according to a Plummer model, in virial
//	units (M=G=-4E=1, with E the total energy), and finite spatial extent
//	which can be regulated by specifying mfrac or rfrac or using their default
//	values.
//	litt: S.J. Aarseth, M. Henon and R. Wielen (1974),
//	      Astron. and Astrophys. 37, p. 183.
//
//	There could be more description here, but not sure if the parser will find it.
//
// @key  nbody    123        i       number of bodies in the Nbody system
// @key  seed     0          int     random seed 
// @key  mfrac    0.999      double  Mass cutoff
// @key  rfrac    22.8       d       Radial cutoff (virial units)
// @key  comment  @noDefault string  Optional comments added to output data stream
// @key  iflag    true       b       an as yet undocumented flag
// @key  oflag    false      bool    flag to keep on looping forever for ^Z testing
// @key  intP1    0x101      int     integer value #1
// @key  intP2    0X101      int     integer value #2
// @key  intP3    0101       int     integer value #3
// @key  intP4    101        int     integer value #4
// @key  intP5    0x1a1      int     integer value #5
// @key  intP6    0XAade     int     integer value #6
// @key  doSEGV   false      bool    whether to cause a SEGV deliberately
//
// @logger TEST_FACILITY carma.test.util.tProgram1
//


namespace {

void
compute( const bool forever,
         const bool doSegv ) {
    Program & p = Program::getProgram( );

#if 0
    // destruct now, and see what happens to the CLI...
    p.~Program();
#endif

    const int n = p.getIntParameter( "nbody" );
    
    printf( "Finishing off computing:   n=%d\n", n );
    
    if ( doSegv ) {
        int * foo = reinterpret_cast< int * >( 0x00000044 );
    
        *foo = 11;
    }
    
    if ( forever ) {
        double sum;
          
        for ( int i = 0; ; ++i ) {
            sum += sqrt( 2.0 );
        }
    }
}


}  // namespace < anonymous >


int
Program::main( ) {
    {
        const int extraArgc = getExtraArgc( );
        char **   extraArgv = getExtraArgv( );
        
        for ( int i = 1; i < extraArgc; ++i ) {
            cout << "extra arg[ " << i << " ] = "
                 << "\"" << extraArgv[ i ] << "\"\n";
        }
    }
    
    const int n = getIntParameter( "nbody" );
    const bool doSegv = getBoolParameter( "doSEGV" );
    
    const bool commentWasSpecified = parameterWasSpecified( "comment" );
    string comment;

    if ( commentWasSpecified )
        comment = getStringParameter( "comment" );
        
    const double mfrac = getDoubleParameter( "mfrac" );
    //const double rfrac = getDoubleParameter( "rfrac" );

    // seed = srandinter( getIntParameter( "seed" ) );
    const int seed = getIntParameter( "seed" );

    const bool i_flag = getBoolParameter( "iflag" );  // undocumented features
    const bool o_flag = getBoolParameter( "oflag" );  // undocumented features

    if ( o_flag )
        cout << "random seed = " << seed << endl;
    
    char seedlog[ 128 ];
    sprintf( seedlog, "       random number generator seed = %d", seed );
    
    if ( DebugLevel( 0 ) )
        cout << "COMMENT:" << seedlog << endl;
        
    cout << "nbody = " << n << endl;
    cout << "mfrac = " << mfrac <<endl;
    //cout << "rfrac = " << rfrac <<endl;
    cout << "i/o flag = " << i_flag << " " << o_flag << endl;
    cout << "comment = \"" << comment << "\"" << endl;

    cout << "intP1 = " << getIntParameter( "intP1" ) << endl;
    cout << "intP2 = " << getIntParameter( "intP2" ) << endl;
    cout << "intP3 = " << getIntParameter( "intP3" ) << endl;
    cout << "intP4 = " << getIntParameter( "intP4" ) << endl;
    cout << "intP5 = " << getIntParameter( "intP5" ) << endl;
    cout << "intP6 = " << getIntParameter( "intP6" ) << endl;

    compute( o_flag, doSegv );
    
    return 0;
}
