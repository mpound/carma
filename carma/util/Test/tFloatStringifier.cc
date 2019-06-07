#include <iostream>
#include <cfloat>

#include "carma/util/FloatStringifier.h"
#include "carma/util/Program.h"


namespace {


carma::util::FloatStringifier gFloatStringifier;
bool gAllGood = true;


void
FailIfNoMatch( const float  value,
               const char * valueDiagnosticChars,
               const char * expectedTrimmedChars ) {
    const ::size_t kResultMaxCharsCount =
        carma::util::FloatStringifier::MAX_CHARS_PER_VALUE;
        
    char resultChars[ kResultMaxCharsCount ];
    
    const ::size_t resultCharsCount =
        ::gFloatStringifier.stringifyFloat( value, resultChars, kResultMaxCharsCount );
    
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
                    << " evaluated to "
                    << value
                    << " which mapped to "
                    << "\"" << resultString << "\""
                    << " which trims to "
                    << "\"" << resultTrimmedString << "\""
                    << " instead trimming to "
                    << "\"" << expectedTrimmedChars << "\""
                    << ::std::endl;
        
        gAllGood = false;
    }
}


}  // anonymous namespace


#define CARMA_FAIL_IF_NO_MATCH(e, s) FailIfNoMatch( (e), #e, s )


//
// @version $Revision: 1.4 $ $Date: 2006/02/23 19:16:24 $
//
// @usage use it
//
// @description A test program
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.util.tFloatStringifier
//

int
carma::util::Program::main( ) {
    // CARMA_FAIL_IF_NO_MATCH( (1.0 / -3.0), "-3.3333332333333e-001" ); DELIBERATE ERROR

    CARMA_FAIL_IF_NO_MATCH( 1.0,          "+1.00000e+000" );
    CARMA_FAIL_IF_NO_MATCH( -1.0,         "-1.00000e+000" );

    CARMA_FAIL_IF_NO_MATCH( 10.0,         "+1.00000e+001" );
    CARMA_FAIL_IF_NO_MATCH( -10.0,        "-1.00000e+001" );

    CARMA_FAIL_IF_NO_MATCH( 0.0,          "+0.00000e+000" );
    
    CARMA_FAIL_IF_NO_MATCH( 0,            "+0.00000e+000" );
    CARMA_FAIL_IF_NO_MATCH( +0.0,         "+0.00000e+000" );
    CARMA_FAIL_IF_NO_MATCH( -0.0,         "+0.00000e+000" );
    
    CARMA_FAIL_IF_NO_MATCH( 3.1416,       "+3.14160e+000" );
    CARMA_FAIL_IF_NO_MATCH( -24,          "-2.40000e+001" );
    CARMA_FAIL_IF_NO_MATCH( -1.0e-100,    "+0.00000e+000" );
    
    CARMA_FAIL_IF_NO_MATCH( (1.0 / 3.0),  "+3.33333e-001" );
    CARMA_FAIL_IF_NO_MATCH( (1.0 / -3.0), "-3.33333e-001" );

    CARMA_FAIL_IF_NO_MATCH( FLT_MAX,      "+3.40282e+038" );
    CARMA_FAIL_IF_NO_MATCH( (-FLT_MAX),   "-3.40282e+038" );

    CARMA_FAIL_IF_NO_MATCH( (2.0 * FLT_MAX),  "+1.00000e+310" );
    CARMA_FAIL_IF_NO_MATCH( (2.0 * -FLT_MAX), "-1.00000e+310" );

#if 0
    CARMA_FAIL_IF_NO_MATCH( 1.1e+280,     "+1.10000e+280" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+290,     "+1.10000e+290" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+300,     "+1.10000e+300" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+301,     "+1.10000e+301" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+302,     "+1.10000e+302" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+303,     "+1.10000e+303" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+304,     "+1.10000e+304" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+305,     "+1.10000e+305" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+306,     "+1.10000e+306" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+307,     "+1.10000e+307" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e+308,     "+1.10000e+308" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e+309,     "+9.9999999999999e+310" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e+310,     "+9.9999999999999e+310" );

    CARMA_FAIL_IF_NO_MATCH( -1.1e+280,    "-1.10000e+280" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+290,    "-1.10000e+290" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+300,    "-1.10000e+300" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+301,    "-1.10000e+301" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+302,    "-1.10000e+302" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+303,    "-1.10000e+303" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+304,    "-1.10000e+304" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+305,    "-1.10000e+305" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+306,    "-1.10000e+306" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+307,    "-1.10000e+307" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e+308,    "-1.10000e+308" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e+309,    "-9.9999999999999e+310" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e+310,    "-9.9999999999999e+310" );

    CARMA_FAIL_IF_NO_MATCH( 1.1e-270,     "+1.09999e-270" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-280,     "+1.10000e-280" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-290,     "+1.10000e-290" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-291,     "+1.10000e-291" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-292,     "+1.10000e-292" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-293,     "+1.10000e-293" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-294,     "+1.10000e-294" );
    CARMA_FAIL_IF_NO_MATCH( 1.1e-295,     "+1.10000e-295" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e-296,     "+1.10000e-296" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e-297,     "+1.10000e-297" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e-298,     "+1.10000e-298" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e-299,     "+1.10000e-299" );
    // CARMA_FAIL_IF_NO_MATCH( 1.1e-300,     "+1.10000e-300" );

    CARMA_FAIL_IF_NO_MATCH( -1.1e-270,    "-1.09999e-270" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-280,    "-1.10000e-280" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-290,    "-1.10000e-290" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-291,    "-1.10000e-291" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-292,    "-1.10000e-292" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-293,    "-1.10000e-293" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-294,    "-1.10000e-294" );
    CARMA_FAIL_IF_NO_MATCH( -1.1e-295,    "-1.10000e-295" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e-296,    "-1.10000e-296" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e-297,    "-1.10000e-297" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e-298,    "-1.10000e-298" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e-299,    "-1.10000e-299" );
    // CARMA_FAIL_IF_NO_MATCH( -1.1e-300,    "-1.10000e+300" );
#endif

    if ( gAllGood )
        return 0;
    else
        return 1;
}
