#ifndef CARMA_CORBA_TEST_OBJECTMANAGERIMPL_H
#define CARMA_CORBA_TEST_OBJECTMANAGERIMPL_H

#include "carma/corba/Test/Hello.h"
#include "carma/corba/Test/HelloImpl.h"
#include "carma/corba/Test/Terminator.h"
#include "carma/corba/Test/TerminatorImpl.h"
#include "carma/corba/Test/Wait.h"
#include "carma/corba/Test/Waiter.h"

namespace carma {
namespace corba {

class Server;

namespace test {

class ObjectManagerImpl {
public:

    ObjectManagerImpl( carma::corba::Server & server );

    ~ObjectManagerImpl( );

    Hello_ptr Hello();

    Terminator_ptr Terminator();

    Wait_ptr Wait();

private:

    HelloImpl helloImpl_;
    Hello_ptr helloPtr_;

    TerminatorImpl terminatorImpl_;
    Terminator_ptr terminatorPtr_;

    Waiter waitImpl_;
    Wait_ptr waitPtr_;

}; // class ObjectManagerImpl

} } } // namespace carma::corba::test
#endif
