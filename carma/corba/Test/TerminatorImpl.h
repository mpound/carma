#ifndef CARMA_CORBA_TEST_TERMINATORIMPL_H
#define CARMA_CORBA_TEST_TERMINATORIMPL_H

namespace carma {
namespace corba {

class Server;

namespace test {

class TerminatorImpl {
public:

    TerminatorImpl( carma::corba::Server & server );

    void kill();

private:

    carma::corba::Server & server_;

}; // class TerminatorImpl

}}} // namespace carma::corba::test
#endif
