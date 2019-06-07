#include "carma/corba/Test/TerminatorImpl.h"
#include "carma/corba/Server.h"

#include <iostream>

using namespace carma::corba;
using namespace std;

test::TerminatorImpl::TerminatorImpl( Server & server ) : server_( server )
{
    // Nothing
}

void
test::TerminatorImpl::kill( ) 
{
    cout << "SERVER: Stopping server." << endl;
    server_.stop();
}
