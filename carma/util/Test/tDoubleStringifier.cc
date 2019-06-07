#include <iostream>
#include <cfloat>

#include "carma/util/DoubleStringifier.h"
#include "carma/util/Program.h"


namespace {


carma::util::DoubleStringifier gDoubleStringifier;
bool gAllGood = true;


void
FailIfNoMatch( const double value,
               const char * valueDiagnosticChars,
               const char * expectedTrimmedChars ) {
    const ::size_t kResultMaxCharsCount =
        carma::util::DoubleStringifier::MAX_CHARS_PER_VALUE;
        
    char resultChars[ kResultMaxCharsCount ];
    
    const ::size_t resultCharsCount =
        ::gDoubleStringifier.stringifyDouble( value, resultChars, kResultMaxCharsCount );
    
    const ::std::string resultString( resultChars, resultChars + resultCharsCount );
    
    ::std::string resultTrimmedString = resultString;
    
    const ::size_t frontTrimCount =
        resultTrimmedString.find_first_not_of( ' ' );

    if ( frontTrimCount != 0 )
        resultTrimmedString.erase( 0, frontTrimCount );
    
    const ::size_t backTrimPrev =
        resultTrimmedString.find_last_not_of( ' ' );

    if ( backTrimPrev != ::std::string::npos )
        resultTrimmedString.erase( backTrimPrev + 1 );
    
    if ( resultTrimmedString != ::std::string( expectedTrimmedChars ) ) {
        ::std::cerr << "\"" << valueDiagnosticChars << "\""
                    << " mapped to "
                    << "\"" << resultString << "\""
                    << " which trims to "
                    << "\"" << resultTrimmedString << "\""
                    << " instead of "
                    << "\"" << expectedTrimmedChars << "\""
                    << ::std::endl;
        
        gAllGood = false;
    }
}


}  // anonymous namespace


#define CARMA_FAIL_IF_NO_MATCH(e, s) FailIfNoMatch( (e), #e, s )


//
// @version $Revision: 1.11 $ $Date: 2006/02/23 19:16:24 $
//
// @usage use it
//
// @description A test program
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tDoubleStringifier
//

int
carma::util::Program::main( ) {
    // CARMA_FAIL_IF_NO_MATCH( (1.0 / -3.0), "-3.333333233333e-001" ); DELIBERATE ERROR

    CARMA_FAIL_IF_NO_MATCH( 0.0,          "+0.000000000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( 0,            "+0.000000000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( +0.0,         "+0.000000000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( -0.0,         "+0.000000000000e+000" );
    
    CARMA_FAIL_IF_NO_MATCH( 3.1416,       "+3.141600000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( -24,          "-2.400000000000e+001" );
    CARMA_FAIL_IF_NO_MATCH( -1.e-100,     "-1.000000000000e-100" );
    
    CARMA_FAIL_IF_NO_MATCH( (1.0 / 3.0),  "+3.333333333333e-001" );
    CARMA_FAIL_IF_NO_MATCH( (1.0 / -3.0), "-3.333333333333e-001" );

    CARMA_FAIL_IF_NO_MATCH( DBL_MAX,      "+1.797693134862e+308" );
    CARMA_FAIL_IF_NO_MATCH( (-DBL_MAX),   "-1.797693134862e+308" );

    CARMA_FAIL_IF_NO_MATCH( (2.0 * DBL_MAX),  "+1.000000000000e+310" );
    CARMA_FAIL_IF_NO_MATCH( (2.0 * -DBL_MAX), "-1.000000000000e+310" );

    CARMA_FAIL_IF_NO_MATCH( 1.1e-311,     "+0.000000000000e+000" );

    // Test walking into positive overflow
    CARMA_FAIL_IF_NO_MATCH( 1.1e+280,     "+1.100000000000e+280" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+290,     "+1.100000000000e+290" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+300,     "+1.100000000000e+300" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+301,     "+1.100000000000e+301" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+302,     "+1.100000000000e+302" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+303,     "+1.100000000000e+303" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+304,     "+1.100000000000e+304" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+305,     "+1.100000000000e+305" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+306,     "+1.100000000000e+306" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+307,     "+1.100000000000e+307" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+308,     "+1.100000000000e+308" );
    CARMA_FAIL_IF_NO_MATCH( 1.797e+308,   "+1.797000000000e+308" );
    CARMA_FAIL_IF_NO_MATCH( 1.798e+308,   "+1.000000000000e+310" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+309,     "+1.000000000000e+310" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+310,     "+1.000000000000e+310" );

    // Test walking into negative overflow
    CARMA_FAIL_IF_NO_MATCH( -1.1e+280,    "-1.100000000000e+280" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+290,    "-1.100000000000e+290" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+300,    "-1.100000000000e+300" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+301,    "-1.100000000000e+301" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+302,    "-1.100000000000e+302" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+303,    "-1.100000000000e+303" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+304,    "-1.100000000000e+304" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+305,    "-1.100000000000e+305" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+306,    "-1.100000000000e+306" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+307,    "-1.100000000000e+307" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+308,    "-1.100000000000e+308" );
    CARMA_FAIL_IF_NO_MATCH( -1.797e+308,  "-1.797000000000e+308" );
    CARMA_FAIL_IF_NO_MATCH( -1.798e+308,  "-1.000000000000e+310" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+309,    "-1.000000000000e+310" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+310,    "-1.000000000000e+310" );

    // Test walking into positive underflow
    CARMA_FAIL_IF_NO_MATCH( 1.1e-270,     "+1.100000000000e-270" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-280,     "+1.100000000000e-280" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-290,     "+1.100000000000e-290" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-291,     "+1.100000000000e-291" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-292,     "+1.100000000000e-292" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-293,     "+1.100000000000e-293" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-294,     "+1.100000000000e-294" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-295,     "+1.100000000000e-295" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-296,     "+1.100000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-297,     "+0.110000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-298,     "+0.011000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-299,     "+0.001100000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-300,     "+0.000110000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-301,     "+0.000011000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-302,     "+0.000001100000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-303,     "+0.000000110000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-304,     "+0.000000011000e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-305,     "+0.000000001100e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-306,     "+0.000000000110e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-307,     "+0.000000000011e-296" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-308,     "+0.000000000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-309,     "+0.000000000000e+000" );

    // Test walking into negative underflow
    CARMA_FAIL_IF_NO_MATCH( -1.1e-270,    "-1.100000000000e-270" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-280,    "-1.100000000000e-280" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-290,    "-1.100000000000e-290" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-291,    "-1.100000000000e-291" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-292,    "-1.100000000000e-292" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-293,    "-1.100000000000e-293" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-294,    "-1.100000000000e-294" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-295,    "-1.100000000000e-295" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-296,    "-1.100000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-297,    "-0.110000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-298,    "-0.011000000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-299,    "-0.001100000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-300,    "-0.000110000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-301,    "-0.000011000000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-302,    "-0.000001100000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-303,    "-0.000000110000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-304,    "-0.000000011000e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-305,    "-0.000000001100e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-306,    "-0.000000000110e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-307,    "-0.000000000011e-296" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-308,    "-0.000000000000e+000" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-309,    "-0.000000000000e+000" );

    if ( gAllGood )
        return 0;
    else
        return 1;
}
