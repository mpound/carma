
//
// @version	$Revision: 1.16 $ $Date: 2009/03/20 19:00:41 $ 
//
// @usage	test out some string utilities we often use
//
// @description
//	testing the StringUtils class.  A set of somewhat (un)related
//      static member functions for Strings.
//
// @key ra      12.3456  double   RA to be converted to H:M:S
// @key	dec     12.3456  double   DEC to be converted to D:M:S
//
// @logger TEST_FACILITY carma.test.util.tStringUtils
//

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include <iostream>
#include <cmath>
#include <cstdio>
#include <vector>

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


string
getStringForStringVec( const vector< string > & sVec )
{
    string result;
    
    result += "[";
    
    bool firstOne = true;
    
    vector< string >::const_iterator i = sVec.begin();
    const vector< string >::const_iterator iEnd = sVec.end();
    
    for ( ; i != iEnd; ++i ) {
        if ( firstOne )
            firstOne = false;
        else
            result += ", ";
            
        result += "<";
        result += *i;
        result += ">";
    }

    result += "]";
    
    return result;
}


bool
verifyTokens( const vector< string > & actualTokens,
              const string &           expectedResult )
{
    const string actual = getStringForStringVec( actualTokens );
    
    if ( actual == expectedResult ) {
        const string msg = "Good tokenize result " + actual;
        
        // cout << msg << endl;
        
        programLogInfoIfPossible( msg );

        return true;
    }
        
    {
        const string msg = "Bad tokenize result " + actual +
                           ". Expected result was " + expectedResult;
    
        // cout << msg << endl;
        
        programLogErrorIfPossible( msg );
    }
    
    return false;
}


bool
verifyTokenizeResults( const string & text,
                       const string & delimiterSet,
                       const string & expectedSkipNullsResult,
                       const string & expectedIncludeNullsResult )
{
    bool status = true;

    if ( verifyTokens( StringUtils::tokenize( text,
                                              delimiterSet,
                                              true ),
                       expectedSkipNullsResult ) == false )
        status = false;

    vector< string > actualTokens1;
    StringUtils::tokenizeInplace( actualTokens1,
                                  text,
                                  delimiterSet,
                                  true );
    
    if ( verifyTokens( actualTokens1, expectedSkipNullsResult ) == false )
        status = false;

    if ( verifyTokens( StringUtils::tokenize( text,
                                              delimiterSet,
                                              false ),
                       expectedIncludeNullsResult ) == false )
        status = false;

    vector< string > actualTokens2;
    actualTokens2.push_back( "bogus" );
    StringUtils::tokenizeInplace( actualTokens2,
                                  text,
                                  delimiterSet,
                                  false );
    
    if ( verifyTokens( actualTokens2, expectedIncludeNullsResult ) == false )
        status = false;

    return status;
}


bool
verifyTokenizeNResults( const string & text,
                        const size_t   maxTokens,
                        const string & delimiterSet,
                        const string & expectedSkipNullsResult,
                        const string & expectedIncludeNullsResult )
{
    bool status = true;

    if ( verifyTokens( StringUtils::tokenizeN( text,
                                               maxTokens,
                                               delimiterSet,
                                               true ),
                       expectedSkipNullsResult ) == false )
        status = false;

    vector< string > actualTokens1;
    StringUtils::tokenizeNInplace( actualTokens1,
                                   text,
                                   maxTokens,
                                   delimiterSet,
                                   true );
    
    if ( verifyTokens( actualTokens1, expectedSkipNullsResult ) == false )
        status = false;

    if ( verifyTokens( StringUtils::tokenizeN( text,
                                              maxTokens,
                                              delimiterSet,
                                              false ),
                       expectedIncludeNullsResult ) == false )
        status = false;

    vector< string > actualTokens2;
    actualTokens2.push_back( "bogus" );
    StringUtils::tokenizeNInplace( actualTokens2,
                                   text,
                                   maxTokens,
                                   delimiterSet,
                                   false );
    
    if ( verifyTokens( actualTokens2, expectedIncludeNullsResult ) == false )
        status = false;

    return status;
}


bool
testTokenize( )
{
    const string text = "a b  c aa bb ccc";
    
    bool status = true;
    
    if ( verifyTokenizeResults( text,
                                " ",
                                "[<a>, <b>, <c>, <aa>, <bb>, <ccc>]",
                                "[<a>, <b>, <>, <c>, <aa>, <bb>, <ccc>]" ) == false )
        status = false;

    if ( verifyTokenizeNResults( text,
                                 3,
                                 " ",
                                 "[<a>, <b>, <c>]",
                                 "[<a>, <b>, <>]" ) == false )
        status = false;

    if ( verifyTokenizeResults( text,
                                "a",
                                "[< b  c >, < bb ccc>]",
                                "[< b  c >, <>, < bb ccc>]" ) == false )
        status = false;

    if ( verifyTokenizeNResults( text,
                                 3,
                                 "a",
                                 "[< b  c >, < bb ccc>]",
                                 "[< b  c >, <>, < bb ccc>]" ) == false )
        status = false;

    if ( verifyTokenizeResults( text,
                                "c",
                                "[<a b  >, < aa bb >]",
                                "[<a b  >, < aa bb >, <>, <>]" ) == false )
        status = false;

    if ( verifyTokenizeNResults( text,
                                 3,
                                 "c",
                                 "[<a b  >, < aa bb >]",
                                 "[<a b  >, < aa bb >, <>]" ) == false )
        status = false;

    if ( verifyTokenizeResults( text,
                                " c",
                                "[<a>, <b>, <aa>, <bb>]",
                                "[<a>, <b>, <>, <>, <>, <aa>, <bb>, <>, <>, <>]" ) == false )
        status = false;

    if ( verifyTokenizeNResults( text,
                                 3,
                                 " c",
                                 "[<a>, <b>, <aa>]",
                                 "[<a>, <b>, <>]" ) == false )
        status = false;

    return status;
}

bool 
testCollapse() 
{
    const string kInput1       = "ooobbbAAAAAAAFFFFggggKKKKKMMMMM";
    const string kGoodCollapse = "ooobAFFFFgKKKKKM";
    vector<char> vc;
    vc.push_back('b');
    vc.push_back('A');
    vc.push_back(' ');
    vc.push_back('g');
    vc.push_back('M');
    const string output1 = StringUtils::collapse( kInput1, vc );
    //cout << "collapsed: " << output1 << endl;
    return ( output1 == kGoodCollapse );
}

bool 
testSort() 
{
    const string kForwardString = "dfklm";
    const string kReverseString = "mlkfd";
    const string kInput1        = "mdlfk";

    bool status = true;

    const string output1 = StringUtils::sort( kInput1 );
    bool isValid = ( output1 == kForwardString );
    status = ( status && isValid );

    const string output2 
	= StringUtils::sort( kInput1, StringUtils::DESCENDING_SORT );
    isValid = ( output2 == kReverseString );
    status = ( status && isValid );

    return status;
}

bool 
testContainsOnly() 
{

    // test the magic purpose characters
    const string kValidChars = "FGBPSO";

    bool status = true;
    bool isValid = StringUtils::containsOnly("fso",kValidChars);
    //cout << " fso is valid: " << boolalpha << isValid << endl;
    status = ( status && isValid );

    isValid = StringUtils::containsOnly("OSF",kValidChars);
    cout << " OSF is valid: " << boolalpha << isValid << endl;
    status = ( status && isValid );

    isValid = StringUtils::containsOnly("zYQ", kValidChars);
    //cout << " zYQ is valid: " << boolalpha << isValid << endl;
    status = ( status && !isValid );

    // empty is a subset of anything
    isValid = StringUtils::containsOnly("", kValidChars);
    //cout << " \"\" is valid: " << boolalpha << isValid << endl;
    status = ( status && isValid );

    // space is not subset of non-spaced chars
    isValid = StringUtils::containsOnly(" ", kValidChars);
    //cout << " \" \" is valid: " << boolalpha << isValid << endl;
    status = ( status && !isValid );

    // test special cases

    // Something vs. empty is false.
    isValid = StringUtils::containsOnly("x", "");
    //cout << " x contained in empty: " << boolalpha << isValid << endl;
    status = ( status && !isValid );

    // empty vs. empty is true.
    isValid = StringUtils::containsOnly("","");
    //cout << " empty contained in empty: " << boolalpha << isValid << endl;
    status = ( status && isValid );

    return status;
}

bool 
testComputeMessageDigest( )
{

    //cout << "Begin digest tests" << endl;
    
    const string kMsg         = "abcdefghijklmnopqrstuvwxyz";
    const string kCorrectSha1 = "32d10c7b8cf96570ca04ce37f2a19d84240d3a89";
    const string kCorrectMD5  = "c3fcd3d76192e4007dfb496cca67e13b";
    
    bool status = true;
    
    status = (status &&
              (StringUtils::computeMessageDigest(kMsg,SHA1) == kCorrectSha1));
    status = (status &&
              (StringUtils::computeMessageDigest(kMsg,MD5) == kCorrectMD5));
    
    return status;
}

bool
testUniq( )
{
    const string kInput1          = "ooobBbAAAAAAAFFFFggggKKKKKMMMMMm";
    const string kGoodUniq        = "obBbAFgKMm";
    const string kGoodReallyUniq  = "obBAFgKMm";
    const string kGoodUniqNoCase  = "OBAFGKM";

    bool status = true;
    const string output1 = StringUtils::uniq( kInput1 );
    bool isValid = ( output1 == kGoodUniq );
    status = ( status && isValid );
    //cout << " uniq("<<kInput1<<"): " << output1 << " good= " << kGoodUniq << endl;

    const string output2 = StringUtils::reallyUniq( kInput1 );
    isValid = ( output2 == kGoodReallyUniq );
    status = ( status && isValid );
    //cout << " really uniq("<<kInput1<<"): " << output2 << " good= " << kGoodReallyUniq << endl;

    const string ucase = StringUtils::lowASCIIAlphaNumericToUpper( kInput1 );
    const string output3 = StringUtils::uniq( ucase );
    isValid = ( output3 == kGoodUniqNoCase );
    status = ( status && isValid );
    //cout << " uniqNC("<<kInput1<<"): " << output3 << " good= " << kGoodUniqNoCase << endl;

    // try the examples in the doxygen descriptions of these methods.
    const string kInput2 = "abcdcefffghgi";
    const string kOutput2 = "abcdefghi";
    isValid = ( kOutput2 == StringUtils::reallyUniq( kInput2 ) );
    status = ( status && isValid );

    const string kOutput4 = "abcdcefghgi";
    isValid = ( kOutput4 == StringUtils::uniq( kInput2 ) );
    status = ( status && isValid );
 
    return status;
}

bool
testStringToInt()
{
        bool status = true;
	try {
	    const string kGoodInput = "8675309";
	    const int kGoodInt      =  8675309;
	    const string kBadInput  = "BadInput";
	    int gvalue = StringUtils::stringToInt( kGoodInput );
	    status = ( status && ( gvalue == kGoodInt ) );
	    int bvalue = StringUtils::stringToInt( kBadInput );

	    // we only get here if we don't catch the exception on 
	    // the bad input.
	    status = false;
	    // shut up the compiler warning about unused variable
	    bvalue++;

	} catch ( IllegalArgumentException & ex ) {
	    cout << "StringUtils test caught exception: "
	    << ex.what()
	    << endl
	    << " (Don't worry, I was supposed to catch this.)"
	    << endl;
	    // status unchanged because status && true = status.
	}
	return status;
}

bool 
testWhiteSpace(void)
{
    bool status = true;
    const string kTest4("this has white space");
    const string kTest5("");
    const string kTest6("NO_WHITE_SPACE_HERE");

    bool flag = StringUtils::isEmptyOrContainsWhiteSpace( kTest4 ) ;
    status = ( status && flag );

    flag = StringUtils::isEmptyOrContainsWhiteSpace( kTest5 ) ;
    status = ( status && flag );

    flag = ! StringUtils::isEmptyOrContainsWhiteSpace( kTest6 ) ;
    status = ( status && flag );

    return status;
}

bool
testTrimWhitespace( ) 
{
    bool status = true;
    const string empty("");
    const string trimmed("fit and trimmed");
    const string untrimmed("	" + trimmed + "  	\r\n");

    status = ( status && 
               ( empty == StringUtils::trimWhiteSpace( empty ) ) );
    status = ( status && 
               ( trimmed == StringUtils::trimWhiteSpace( trimmed ) ) );
    status = ( status && 
               ( trimmed == StringUtils::trimWhiteSpace( untrimmed ) ) );

    // Ditto for inplace versions (NC = NonConst)
    string emptyNC("");
    string trimmedNC("fit and trimmed");
    string untrimmedNC("	" + trimmed + "  	\r\n");

    StringUtils::trimWhiteSpaceInplace( emptyNC );
    StringUtils::trimWhiteSpaceInplace( trimmedNC );
    StringUtils::trimWhiteSpaceInplace( untrimmedNC );

    status = ( status && ( empty == emptyNC ) );
    status = ( status && ( trimmed == trimmedNC ) );
    status = ( status && ( trimmed == untrimmedNC ) );

    return status;
}

}  // namespace < anonymous >


int
Program::main( )
{
    bool status = false;
    try {
	const string ra_s  = getParameterRawValueString( "ra" );
	const string dec_s = getParameterRawValueString( "dec" );
	const double ra    = getDoubleParameter( "ra" ) * M_PI/12.0;
	const double dec   = getDoubleParameter( "dec" ) * M_PI/180.0;
	
	//@todo get rid of key inputs and make this a selfcontained test.
	cout << "ra:  " << ra_s << " h.hhh = " << ra << " radians" << endl;
	cout << "dec: " << dec_s << " d.ddd = " << dec << " radians" << endl;
	cout << "ra:  " << StringUtils::hms(ra) << " unsigned" << endl;
	cout << "dec: " << StringUtils::dms(dec) << " unsigned" << endl;
	cout << "ra:  " << StringUtils::shms(ra,3) << " signed" << endl;
	cout << "dec: " << StringUtils::sdms(dec,4) << " signed" << endl;

	status =    testComputeMessageDigest() 
		 && testTokenize() 
		 && testCollapse()
		 && testSort() 
		 && testUniq()
		 && testStringToInt()
		 && testWhiteSpace()
         && testTrimWhitespace( ) 
		 ;

    } catch (...) {
      cerr << getArg0() << " caught an exception. bye." <<endl;
      return EXIT_FAILURE;
    }
    cerr << getArg0() << " returning with" 
	 << (status ? " success!" : " FAILURE :-(" )
	 << endl;
    return ( status ? EXIT_SUCCESS : EXIT_FAILURE );

}
