#ifndef CARMA_CORBA_HELLOIMPL_H
#define CARMA_CORBA_HELLOIMPL_H

// Note: No inclusions

namespace carma {
namespace corba {
namespace test {
    
    // Note this is a stand-alone class, no inheritance, no corba anything.
    class HelloImpl { 
    public:

        void hi();

    };

}}} 
#endif
