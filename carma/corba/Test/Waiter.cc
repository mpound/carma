#include "carma/corba/Test/Waiter.h"

#include <iostream>
#include <time.h>

using namespace carma::corba::test;
using namespace std;

void Waiter::pause( const CORBA::ULong ms ) 
{
    ::timespec ts, rem;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = ( ms % 1000 ) * 1000000;

    cout << "SERVER: pausing " << ms << "ms" << "." << endl;
    while ( ::nanosleep( &ts, &rem ) ) ts = rem;
}
