#include "carma/util/Test/SingletonTest.h"
#include "carma/util/Singleton.h"
#include "carma/util/Program.h"
#include <iostream>
#include <exception>

#include "cppunit/TextTestRunner.h"

using namespace std;
using namespace carma::util;
using namespace carma::util::test;

//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.utilTest
//

int Program::main()
{
try {
    bool result;
    CppUnit::TextTestRunner runner;
    runner.addTest( 
            SubclassedSingletonTest<Loner<CreateWithNewPolicy> >::suite() );
    runner.addTest( 
            SubclassedSingletonTest<Loner<CreateStaticPolicy> >::suite() );
    runner.addTest( 
            SubclassedSingletonTest
            <Loner<CreateWithNewUnmanagedPolicy> >::suite() );
    runner.addTest( SubclassedSingletonTest<Lonely>::suite() );
    runner.addTest(
            AdaptedSingletonTest
            <Adapted<CreateWithNewPolicy>, CreateWithNewPolicy>::suite() );
    runner.addTest(
            AdaptedSingletonTest
            <Adapted<CreateStaticPolicy>, CreateStaticPolicy>::suite() );
    runner.addTest(
            AdaptedSingletonTest
            <Adapted<CreateWithNewUnmanagedPolicy>, 
                 CreateWithNewUnmanagedPolicy>::suite() );
    runner.addTest( 
        SubclassedSingletonTest<Automanaged>::suite() );
    runner.addTest(
            AutomanagedTest::suite() );
    result = runner.run();
    return (result ? EXIT_SUCCESS : EXIT_FAILURE);

} catch (const std::exception &sex) {
    ::std::cerr << "Program::main() - Caught std::exception: " << ::std::endl;
    ::std::cerr << sex.what() << ::std::endl;
    return EXIT_FAILURE;
}

return EXIT_SUCCESS;
}
