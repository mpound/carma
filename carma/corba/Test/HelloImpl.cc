#include "carma/corba/Test/HelloImpl.h"

#include <boost/thread/thread.hpp>
#include <iostream>

using namespace std;

// Again note, there is no corba here.
void 
carma::corba::test::HelloImpl::hi() {

    cout << "SERVER (" << boost::this_thread::get_id() << "): hi!" << endl;

}
