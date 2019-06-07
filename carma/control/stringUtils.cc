#include "carma/control/stringUtils.h"
#include "carma/util/StringUtils.h" //grrr
#include <sstream>

using namespace ::std;

namespace carma { 
namespace control { 

    // Note these should all be lower case. Comparison
    // is made by converting input to lower case first.
    static const string OBSBLOCK_RESERVE_WORDS[] = { 
	"xml",
	"done",
	"write",
	"read",
	"mir",
	"busy",
	"error"
    };

    static const unsigned short NUM_RESERVE_WORDS = 7;

} // control namespace 
} // carma namespace

string
carma::control::escapeAndQuoteStringAsNeeded( const string & s ) {
    string result;
    
    if ( s.find_first_of( " ," ) != string::npos )
        result = string( "\"" ) + s + string( "\"" );
    else
        result = s;

    return result;
}


bool 
carma::control::containsDot(const string & s ) 
{
    return ( s.find(".") != string::npos );
}


bool 
carma::control::containsQuote(const string & s ) 
{
    return (   ( s.find("'") != string::npos )
	    || ( s.find('"') != string::npos )
	   );
}

bool 
carma::control::containsAsterisk(const string & s ) 
{
    return ( s.find("*") != string::npos );
}

bool
carma::control::containsReserveWords( const string & s )
{
    string s2 = carma::util::StringUtils::lowASCIIAlphaNumericToLower(s);

    for (unsigned short i = 0 ; i < NUM_RESERVE_WORDS; i++ ) {
        if ( s2.find( OBSBLOCK_RESERVE_WORDS[i] ) != string::npos )
	    return true;
    }
    return false;
}

string carma::control::listReserveWords()
{
    ostringstream os;
    for (unsigned short i = 0 ; i < NUM_RESERVE_WORDS; i++ ) {
        os << OBSBLOCK_RESERVE_WORDS[i] << " ";
    }
    return os.str();
}

/*
bool
carma::control::isEmptyOrContainsWhiteSpace( const string & s )
{
    if ( s.empty() || s.find(" ") != string::npos )
	return true;
    else
	return false;
}
*/
