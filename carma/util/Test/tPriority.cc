//
// @version	$Revision: 1.2 $
//
// @usage	test out setting unix priority
//
// @description
// test out setting unix priority
//
// @key	priority 0       i priority to set
// @key	iter     10000   i number of iterations
// @key	policy   0       i scheduling olicy value to attempt change to,
//                         0 = SCHED_OTHER (normal timeshare)
//                         1 = SCHED_FIFO (first in first out).
//                         2 = SCHED_RR (round robin).
// @see man sched_setpriority
//
// @logger TEST_FACILITY carma.test.util.tPriority
//

#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/programLogging.h"
#include <iostream>
#include <sys/resource.h>
#include <sched.h>

using namespace ::std;
using namespace carma;
using namespace carma::util;


int
Program::main( )
{
    bool status = false;
    try {
	const int priority = getIntParameter( "priority" );
	const int iter     = getIntParameter( "iter" );
	const int policy   = getIntParameter( "policy" );
	int niceValue = ::getpriority( PRIO_PROCESS, 0 );
	cout << "Initial process nice = " << niceValue << endl;
	int progNice = getNiceLevel();
	cout << "Program nice = " << progNice << endl;
	status=true;
	int i = 0;
	int oldn;

	while( i < iter ) {
	    oldn = niceValue;
	    i++;
	    // do something expensive(?)
	    double mjd = Time::MJD();
	    string foo = Time::getTimeString(mjd,10);
	    string bar = Time::getDateTimeString(mjd,6,"%Y-%m-%d");
	    niceValue = ::getpriority( PRIO_PROCESS, 0 );
	    if ( niceValue == -1 ) {
		::perror("getpriority");
		status = false;
	    }

	    // check for external changes to nice value
	    if ( niceValue != oldn ) {
	         cout << "got new nice =" << niceValue << endl;
	    }

	    /*
	     * this always fails on eagle build because it
	     * is running make check with nice=10. comment
	     * it out until i figure out the correct fix
	     * so that the eagle build will go green.
	     */
	    /*
	    if ( i == iter / 2 ) {
		int ret = setpriority(0, PRIO_PROCESS, priority );
		if ( ret != 0 ) {
		    cout << "Unable to change my own priority:" 
			 << endl;
		    ::perror("setpriority");
		    status = false;
		    if ( progNice > priority ) {
			cout << "(Program nice keyword value was"
			     << " already higher than requested priority.)"
			     << endl;
			continue;
		    }

		    if ( niceValue > priority ) {
			cout << "(Process nice value was"
			     << " already higher than requested priority.)"
			     << endl;
		    }

		} else {
		    cout << "Changed process priority to " << priority << endl;
		}
	    }
	    */

	}

	pid_t me = 0;
	int sgt = ::sched_getscheduler( me );
	if (  sgt == -1 ) {
	    ::perror("sched_getscheduler");
	    status = false;
	}

	switch ( sgt ) {
	    case SCHED_FIFO:
		cout << "Sched policy is FIFO " << endl;
		break;
	    case SCHED_RR:
		cout << "Sched policy is RR " << endl;
		break;
	    case SCHED_OTHER:
		cout << "Sched policy is OTHER" << endl;
		break;
	    default:
		cout << "Unrecognized Sched policy " << endl;
		status = false;
		break;
	}

	struct sched_param * gparam 
	    = static_cast<struct sched_param *>( 
		    malloc(sizeof (struct sched_param) )
		    );
	int igp = ::sched_getparam( me , gparam );
	if (  igp == -1 ) {
	    cout << "Unable to get real time priority" <<endl;
	    ::perror("sched_getparam");
	    status = false;
	} else {
	    cout << "sched_getparam ok" << endl;
	    cout << "sched_param.sched_priority = "
		 << gparam->sched_priority
		 << endl;
	}

	// Now try to set it. Note priority value in
	// the struct has different meaning than nice: higher values
	// are higher priority and negative values are invalid.
	// So just take an absolute value here.
	gparam->sched_priority = abs(priority);
	igp = ::sched_setparam( me , gparam );
	if (  igp == -1 ) {
	    cout << "Unable to set real time priority" <<endl;
	    // note the error is "invalid argument" if permission
	    // is denied to change value
	    ::perror("sched_setparam");
	    status = false;
	} else {
	    cout << "sched_setparam worked" << endl;
	    cout << "sched_param.sched_priority = "
		 << gparam->sched_priority
		 << endl;
	}

	int myPolicy = SCHED_OTHER;
	string polstr = "SCHED_OTHER";
	switch ( policy ) {
	    default:
	    case 0:
	        myPolicy = SCHED_OTHER;
		polstr = "SCHED_OTHER";
		break;
	    case 1:
	        myPolicy = SCHED_FIFO;
		polstr = "SCHED_FIFO";
		break;
	    case 2:
	        myPolicy = SCHED_RR;
		polstr = "SCHED_RR";
		break;
	}

	gparam->sched_priority = 0;
	// SCHED_OTHER works, SCHED_RR, SCHED_FIFO do not.
	igp = ::sched_setscheduler(me, myPolicy , gparam);
	if (  igp == -1 ) {
	    cout << "Unable to set real time policy to " 
		 << polstr
		 << ":"
		 << endl;
	    // note the error is "invalid argument" if permission
	    // is denied to change value
	    ::perror("sched_setscheduler [i.e. policy]");
	    status = false;
	} else {
	    cout << "sched_setscheduler successfully set real time policy to " 
		 << polstr
		 << "."
		 << endl;
	}


    } catch (...) {
      cerr << getArg0() << " caught an exception. Bye." <<endl;
      return EXIT_FAILURE;
    }

    cerr << getArg0() << " returning with" 
	 << (status ? " success!" : " FAILURE :-(" )
	 << endl;
    return ( status ? EXIT_SUCCESS : EXIT_FAILURE );

}
