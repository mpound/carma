#ifndef CARMA_CORBA_TEST_WAIT_H
#define CARMA_CORBA_TEST_WAIT_H

#include "carma/corba/corba.h"

namespace carma {
namespace corba {
namespace test {

class Waiter {
public:

    void pause( CORBA::ULong ms );

};

}}}
#endif
