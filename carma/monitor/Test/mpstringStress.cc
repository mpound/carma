//
// @version $Revision: 1.3 $
//
// @usage 
// Before compiling set debug=true in MonitorPointString::getValue() const 
// in monitorPointSpecializations.cc
//
// @description
//      see if mpstrings allocates number of samples correctly.
//
// @key size 80 integer
//      size of largest string to test, in chars
//
// @logger TEST_FACILITY carma.test.monitor.mpstringStress
//

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
//#include "carma/monitor/SlPipelineSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include <iostream>
#include <string>

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

/// @TODO ADD TEST OF SETTING SOMETHING TO EMPTY STRING
//AFTER SETTING IT TO STRING WITH SIZE>8

namespace { //< anonymous >
void mpStringStats ( const MonitorPointString & mps ) 
{
    const int width = mps.getWidth();
    const int samps = mps.getNumSamples();
    const string v  = mps.getValue();
    const string av = mps.getAve();
    const string gvs = mps.getValueToString();
    cout <<  " width : " << width
         <<  " samps: "  << samps 
	 << endl
	 << " value: " << v
	 << endl
	 << " average: " << av
	 << endl
	 << " v2s: " << gvs
	 << endl;
    /*
     * grrr...MPString::getValueToString(i) ignores the
     * index!
    cout << " individual samples : " << endl;
    for (int i = 1 ; i <= samps; i++ ) {
	const string gvsi = mps.getValueToString( i );
        cout << i << " : " << gvsi << endl;
    }
    */
}

} // < anonymous >

int
Program::main( )
{
    try {
    int size = getIntParameter( "size" );

    programLogInfoIfPossible( "making the monitor system" );

    MonitorSystem * cms = new CarmaMonitorSystem;

	
    //SlPipelineSubsystem  &   slp = cms->slPipeline();
    ControlSubsystem  &  control = cms->control();
    ControlSubsystemBase::Subarray & sa = control.subarray( 0 );

    programLogInfoIfPossible( "playing with the monitor system" );

    cms->readNewest();

    MonitorPointString & mp = sa.obsBlockId();
    cout << " initial stats: " << endl;
    //mpStringStats ( mp );
    ostringstream os;

    for (int i = 0; i < size ; i++ ) {
	cout << " ASCENDING " << endl;
	os << ( i % 8 );
	mp.setValue( os.str() );
        mpStringStats ( mp );
	string val = mp.getValue( );
	cout << "final value: " << val << endl;
	cout << "-------------------------------------" << endl;
    }

    cout << " DESCENDING " << endl;
    while ( size > 0 ) {
	ostringstream ps;
	for (int i = size; i > 0; i-- ) {
	    ps << ( i % 8 );
	}
	mp.setValue( ps.str() );
        mpStringStats ( mp );
	string val = mp.getValue( );
	cout << "final value: " << val << endl;
	cout << "-------------------------------------" << endl;
	--size;
    }


    programLogInfoIfPossible( "deleting the monitor system" );

    MonitorSystem * deadManWalking = 0;
    
    ::std::swap( deadManWalking, cms );
    
    delete deadManWalking;

    deadManWalking = 0;
    
    programLogInfoIfPossible( "shutting down the tag ID authority" );
    
    dbms::TagIDAuthority::closeAuthority();
    
    return EXIT_SUCCESS;

    } catch ( ... ) {
	cerr << getArg0() << " exiting with failure!" << endl;
        return EXIT_FAILURE;
    }
}
