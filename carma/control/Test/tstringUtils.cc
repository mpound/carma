//
// @noKeys
//
// @logger TEST_FACILITY carma.test.control.tstringUtils
//

#include <iostream>

#include "carma/util/Program.h"
#include "carma/control/stringUtils.h"

using namespace std;
using namespace carma::util;
using namespace carma::control;

namespace { //<anonymous>

bool 
testQuotes(void)
{
    bool status = true;
    const string kTest1("abec'desf");
    string kTest2("ksjdl\"llasdk");
    string kTest3("abcde");

    bool flag = containsQuote( kTest1 );
    status = ( status && flag );
    //cout << kTest1 << " contains quote? " << boolalpha << flag << endl;

    flag = containsQuote( kTest2 );
    status = ( status && flag );
    //cout << kTest2 << " contains quote? " << boolalpha << flag << endl;

    flag = ! containsQuote( kTest3 );
    cout << kTest3 << " contains quote? " << boolalpha << flag << endl;
    //status = ( status && flag );

    return status;
}


bool 
testDots(void)
{
    bool status = true;
    const string kTest1("abced");
    string kTest2("abced.12345");
    string kTest3("abced123.45.cuqjsl");

    bool flag = ! containsDot( kTest1 );
    status = ( status && flag );

    flag = containsDot( kTest2 );
    status = ( status && flag );

    flag = containsDot( kTest3 );
    status = ( status && flag );

    return status;
}


bool 
testReserveWords(void)
{
    bool status = true;
    const string kTest1("this has no reserve words ");
    const string kTest2("who.xml.1");
    const string kTest3("bad.DONE.2");
    const string kTest4("bad.done.3");
    const string kTest5("busy.write.mir.12");

    const string kReserveWordList("xml done write read mir busy error ");

    bool flag = ! containsReserveWords( kTest1 );
    status = ( status && flag );

    flag = containsReserveWords( kTest2 );
    status = ( status && flag );

    flag = containsReserveWords( kTest3 );
    status = ( status && flag );

    flag = containsReserveWords( kTest4 );
    status = ( status && flag );

    flag = containsReserveWords( kTest5 );
    status = ( status && flag );

    const string s = listReserveWords();
    status = ( status && ( s == kReserveWordList ) );

    return status;
}

bool 
testEscapeAndQuote(void)
{
    bool status = true;
    const string kNeedsEscapeAndQuote = "Wherefore art thou, Romeo?";
    const string kResult1= "\"Wherefore art thou, Romeo?\"";
    const string kDoesNotNeedsEscapeAndQuote = "NoSpacesOrCommas.";

    const string output1 
	= escapeAndQuoteStringAsNeeded( kNeedsEscapeAndQuote );
    status = ( status && ( output1 == kResult1 ) );

    const string output2 
	= escapeAndQuoteStringAsNeeded( kDoesNotNeedsEscapeAndQuote );
    status = ( status && ( output2 == kDoesNotNeedsEscapeAndQuote ) );

    return status;
}

} //<anonymous>
int Program::main()
{
    bool status = false;
    try {

	status =    testDots() 
		 && testReserveWords()
		 && testEscapeAndQuote()
		 && testQuotes()
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


