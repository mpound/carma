#include "carma/control/IntentInfo.h"

using namespace carma::control;
using namespace std;

const string IntentInfo::kUNSET = "O";
const string IntentInfo::VALID_PURPOSES = "ABFGPRSO" ;

IntentInfo::IntentInfo() { 
    this->invalidate();
}

IntentInfo::~IntentInfo( )
try { 
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}

IntentInfo::IntentInfo( 
	  const string & inPurpose,
	  bool inSelfcal, 
	  bool inFastSwitch ) :
    purpose( inPurpose ),
    selfcal( inSelfcal ),
    fastSwitch( inFastSwitch )
{
}

void 
IntentInfo::invalidate() {
    purpose    = string( kUNSET );
    selfcal    = false ;
    fastSwitch = false ;
}

IntentInfo::IntentInfo( const IntentInfo & info ) {
    this->purpose = string( info.purpose );
    this->selfcal = info.selfcal;
    this->fastSwitch = info.fastSwitch;
}

IntentInfo & IntentInfo::operator=( const IntentInfo & rhs ) {
    this->purpose = string( rhs.purpose );
    this->selfcal = rhs.selfcal;
    this->fastSwitch = rhs.fastSwitch;
    return *this;
}
