#include "carma/services/DecAngle.h"
#include "carma/services/UvTrack.h"
#include "carma/util/ErrorException.h" 
#include "carma/util/Program.h" 
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/services/Interpolator.h"
#include <vector>
#include <sstream>

using namespace std;
using namespace carma::util;
using namespace carma::services;

const double UvTrack::MAX_ALLOWABLE_LENGTH_HRS =   8.0;
const double UvTrack::HIGH_FREQ_CUTOFF_GHZ     = 113.0;;

UvTrack::UvTrack(const string & arrayConfig,
	         const Frequency & freq )
: arrayConfig_( StringUtils::lowASCIIAlphaNumericToUpper( arrayConfig ) ),
  freq_( freq )
{
    try {
	this->initialize();
    } catch ( ... ) {
	ostringstream os;
	os << "Unable to initialize in constructor UvTrack( " 
	   << arrayConfig 
	   << " , "
	   << freq 
	   << " )";
	programLogWarnIfPossible( os.str() );
	throw;
    }
}

void
UvTrack::initialize()
{
    ostringstream os;
    os << "data/obstime" ;
    if ( freq_.gigahertz() > HIGH_FREQ_CUTOFF_GHZ )
	os << ".hiFreq.tab";
    else
	os << ".lowFreq.tab";
    const string fileName = Program::getConfFile( os.str() );
    const string obstime = "OBSTIME_"+arrayConfig_;
    table_.open( fileName );
    vector<double> declination = table_.getDoubleColumn("DEC");
    vector<double> trackLength = table_.getDoubleColumn( obstime );
    //vector<double> elevation   = table_.getDoubleColumn("Elev.Cutoff");
    const size_t sz = declination.size();

    if ( sz != trackLength.size() ) {
	ostringstream erros;
	erros << "Vector sizes don't match in table " << fileName;
	throw CARMA_EXCEPTION( ErrorException, erros.str() );
    }

    // AKIMA does better job in flat portions of curve then CSPLINE.
    interp_ = new Interpolator( declination, trackLength, AKIMA ); 
}

UvTrack::~UvTrack() 
{
    delete interp_;
}

double
UvTrack::optimalLength( const double decDegrees )
{
    return interp_->evaluate( decDegrees );
}

double
UvTrack::optimalLength( const DecAngle & dec )
{
    return optimalLength( dec.degrees() );
}

double
UvTrack::policyLength( const double decDegrees ) 
{
    double opt = optimalLength( decDegrees );
    double max = maxLength();
    return opt > max ? max : opt;
}

double
UvTrack::policyLength( const DecAngle & dec )
{
    return policyLength( dec.degrees() );
}
