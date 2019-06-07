
#include "carma/services/SourceChecker.h"
#include "carma/services/Angle.h"
#include "carma/services/Astro.h"
#include "carma/services/DecAngle.h"
#include "carma/services/FluxCatalog.h"
#include "carma/services/OpticalCatalog.h"
#include "carma/services/Source.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/stringConstants.h"
#include "carma/services/UnsupportedCoordSysException.h"
#include "carma/services/Vector.h"

#include "carma/util/IllegalStateException.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/stlContainerUtils.h"
#include "carma/util/programLogging.h"

#include <iomanip>
#include <algorithm>
#include <map>
#include <cmath>

// debug
#include "carma/util/StopWatch.h"


using namespace carma;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace std;

namespace { //anonymous

    const double TWO_PI  = 2*M_PI;
    const double RAD2DEG = 180.0/M_PI;
    ::pthread_mutex_t gMasterGuard = PTHREAD_MUTEX_INITIALIZER;

    typedef ScopedLock< ::pthread_mutex_t > MasterGuardLockType;


}

const double SourceChecker::NEVER_SETS   = 25.0; //i.e. larger the LST 24:00
const double SourceChecker::NEVER_RISES  = -1.0;
const string SourceChecker::NEVER_RISES_STR = "NEVER RISES";
const string SourceChecker::NEVER_SETS_STR  = "NEVER SETS";
const string SourceChecker::UNSUPPORTED_COORDSYS_STR = "UNSUPPORTED COORDSYS";
//@todo maybe. make output configurable, from among many available
//fields e.g.
//setOutput(SOURCE, RA, DEC, EL, LST);
//setOutput(SOURCE, EL, RA, AZ, RISE_TIME);

SourceChecker::SourceChecker() :
   elevLimit_(new Angle(10.0,DEGREES)),
   elevUpperLimit_(new Angle(87.5,DEGREES)),
   azPositiveWrapLimit_( Angle(353.0,DEGREES) ), // 10-m limit
   azNegativeWrapLimit_( Angle(-88.0,DEGREES) ), // 10-m limit
   azPositiveWrapLimitLst_(0.0),
   needsMjd_(true),
   needsHeader_(true),
   sexa_(true),
   j2000_(true)
{
    Location carmaLoc(CARMA_OBSERVATORY);
    this->setLocation(carmaLoc);
}

SourceChecker::~SourceChecker()
{
    delete elevLimit_;
    delete elevUpperLimit_;
}

void 
SourceChecker::setSource(const Source& source) 
{
    ephem_.setSource(source);
}

void 
SourceChecker::setSource( const std::string& sourceName, 
	                       const std::string& catalogName
			     ) 
{
    ephem_.setSource(sourceName, catalogName);
}

/**
 */
void 
SourceChecker::setLocation(const Location& location)
{
    ephem_.setLocation(location);
    at_.setSite(location);
}

/**
 * Set the elevation limit for subsequent checking.
 * If the limit is less than zero, it will be set to zero.
 * If the limit is greater than 180, it will be set to (180 - elevDegrees).
 */
void 
SourceChecker::setElevLimit(double elevDegrees)
{
    // allow "over the top" elevations.
    if ( elevDegrees > 90.0 ) 
	elevDegrees = 180.0 - elevDegrees;

    // this must comes after over the top check, to catch
    // negative elevations after the subtraction.
    if ( elevDegrees < 0.0 )  
	elevDegrees = 0.0;

    elevLimit_->reset(elevDegrees, DEGREES);
}

void 
SourceChecker::setElevLimit(const Angle& elevation)
{
    setElevLimit(elevation.degrees());
}

void 
SourceChecker::setElevUpperLimit(double elevDegrees)
{
    if ( elevDegrees > 90.0 ) 
	elevDegrees = 180.0 - elevDegrees;
    if ( elevDegrees < 0.0 )  
	elevDegrees = 0.0;
    elevUpperLimit_->reset(elevDegrees, DEGREES);
}

void 
SourceChecker::setElevUpperLimit(const Angle& elevation)
{

    setElevUpperLimit(elevation.degrees());
}

void 
SourceChecker::setAzPositiveWrapLimit(const Angle & azimuth)
{
    azPositiveWrapLimit_.reset(azimuth.radians(false), RADIANS);
}

void 
SourceChecker::setAzNegativeWrapLimit(const Angle & azimuth)
{
    azNegativeWrapLimit_.reset(azimuth.radians(false), RADIANS);
}


Angle 
SourceChecker::getElevLimit() const
{
    return *elevLimit_;
}

Angle 
SourceChecker::getElevUpperLimit() const
{
    return *elevUpperLimit_;
}


Angle 
SourceChecker::getAzPositiveWrapLimit() const
{
    return azPositiveWrapLimit_;
}

Angle 
SourceChecker::getAzNegativeWrapLimit() const
{
    return azNegativeWrapLimit_;
}


void 
SourceChecker::setFrequency(double freq)
{
    ephem_.setFreq(freq);
}

void 
SourceChecker::setMJD(double mjd)
{
    ephem_.setMJD(mjd);
    needsMjd_ = false;
}

void 
SourceChecker::showHeader(bool showit)
{
    needsHeader_ = showit;
}

string 
SourceChecker::header(Source s)
{
    ostringstream hdr;
    hdr << "SOURCE     ";
    switch ( s.getCoordSysType() ) {
    case COORDSYS_RADEC:
    default:
	//hdr << "RA            DEC ";
	if (j2000_) hdr << "RA(2000)      DEC(2000)";
	else        hdr << " APP RA        APP DEC";
	break;
    case COORDSYS_AZEL:
	hdr << "AZ          EL    ";
	break;
    case COORDSYS_GALACTIC:
	hdr << "L           B     ";
	break;
    }
    hdr << "     UP?        ELEV     RISE LST     SET LST   VELDOP";
    if ( ! Astro::isPlanet( s.getName() ) )
	hdr << "   VELCAT";
    hdr << "   ELEVLIM";
    return hdr.str();

}
    
string 
SourceChecker::info()
{
    if ( needsMjd_ ) setMJD();

    ostringstream os;
    Source s = ephem_.getSource();
    double ra2000  = s.getXCoordinate().radians();
    double dec2000 = s.getYCoordinate().radians();
    //cout << " info thinks source dec2000 is " << dec2000 << " radians" << endl;
    if ( needsHeader_ )
	os << header( s ) << "\n";
    string up = isUp() ? "  YES" : "   NO";

    double appDec = ephem_.getDec();
    double appRa = ephem_.getRa();
    double el = ephem_.getEl();

    double displayRa, displayDec;
    if ( j2000_ ) {
	displayRa = ra2000;
	displayDec = dec2000;
    } else {
	displayRa  = appRa;
	displayDec = appDec;
    }

    DecAngle dec(displayDec,RADIANS);
    //cout << " DecAngle thinks source dec is " << dec << endl;
    //cout << " Dec.dms(1) " << dec.dms(1) << endl;
    //cout << " Dec.degrees() " << dec.degrees() << endl;

    os << setw(SOURCE_WIDTH)
       << setiosflags(ios::left)
       << s.getName() 
       << setiosflags(ios::fixed)
       << setw(ANGLE_WIDTH);
       ( sexa_   
         ? os << setprecision(2) 
         : os << setprecision(4) 
       );
       ( sexa_   
	 ? os << StringUtils::hms(displayRa,1) 
	 : os << displayRa*AstroTime::HOURS_PER_RADIAN
	 );
       os << setw(ANGLE_WIDTH);
       ( sexa_ 
	 ? os << dec.dms(1) 
	 : os << dec.degrees()
	 );
       os << setw(TEXT_WIDTH) 
       << up 
       << setiosflags(ios::left)
       << setw(ANGLE_WIDTH);
       ( sexa_ 
	 ?  os << DecAngle::getString(el,1) 
	 :  os << el*180.0/M_PI );
       os << setw(TIME_WIDTH)
       << rise() 
       << setw(TIME_WIDTH)
       << set()
       << setw(DOPPLER_WIDTH)
       << doppler().kms();
    if ( !Astro::isPlanet( s.getName() ) ) 
       os << " " << setw(DOPPLER_WIDTH) << s.getVelocity().kms();
    os << setprecision(2) << setw(DOPPLER_WIDTH) << elevLimit_->degrees();
    return os.str();
}

bool 
SourceChecker::isUp()
{
    if ( needsMjd_ ) setMJD();

    double elevRad = ephem_.getEl();
    if ( elevRad > elevLimit_->radians() )
        return true;
    else 
        return false;
}

double 
SourceChecker::lst() 
{
    if ( needsMjd_ ) setMJD();
    return at_.localSiderealTime( ephem_.getMJD() );
}

// perhaps these should be moved to astrotime?
string 
SourceChecker::localTime() 
{
    if ( needsMjd_ ) setMJD();
    return Time::getDateTimeString(ephem_.getMJD(),1);
}

string 
SourceChecker::ut() 
{
    if ( needsMjd_ ) setMJD();
    return Time::getTimeString( ephem_.getMJD() );
}

Angle 
SourceChecker::getAzimuth()
{
    if ( needsMjd_ ) setMJD();
    double azimRad = ephem_.getAz();
    return Angle(azimRad,RADIANS);
}

Angle 
SourceChecker::getElevation()
{
    if ( needsMjd_ ) setMJD();
    double elevRad = ephem_.getEl();
    return Angle(elevRad,RADIANS);
}

Velocity 
SourceChecker::doppler() 
{
    if ( needsMjd_ ) setMJD();
    double vel = ephem_.getDoppler() / 1000.0;
    return Velocity(vel, KMS);
}

double 
SourceChecker::getMJD() {
    return ephem_.getMJD();
}

double 
SourceChecker::riseTime() 
{
    if ( needsMjd_ ) setMJD();
    Source source = ephem_.getSource();
    coordSysType ctype = source.getCoordSysType();
    double el = 0.0;
    switch ( ctype ) {
	case COORDSYS_AZEL:
	    el = source.getYCoordinate().degrees();
	    if ( el > elevLimit_->degrees() )
		return NEVER_SETS;
	    else 
		return NEVER_RISES;
	case COORDSYS_RADEC:
	    // no action required
	    break;
	default:
	    ostringstream os;
	    // @todo create a coordsys enum<->string translator
	    os << "Coordinate system "
		<< ctype
		<< " of source "
		<< source.getName()
		<< " is not supported ";
	    throw CARMA_EXCEPTION(
		    services::UnsupportedCoordSysException,os
		  );
    }

    double HA = hourAngleAtElevLimit();
    if ( HA < 0.0 || HA > AstroTime::HOURS_PER_DAY )
	return HA ;

    double ra  = ephem_.getRa();
    double riseTimeHrs = (ra - HA) * AstroTime::HOURS_PER_RADIAN;

    return AstroTime::modulo24(riseTimeHrs);
}

double 
SourceChecker::setTime() 
{
    if ( needsMjd_ ) setMJD();
    Source source = ephem_.getSource();
    coordSysType ctype = source.getCoordSysType();
    double el = 0.0;
    switch ( ctype ) {
	case COORDSYS_AZEL:
	    el = source.getYCoordinate().degrees();
	    if ( el > elevLimit_->degrees() )
		return NEVER_SETS;
	    else 
		return NEVER_RISES;
	case COORDSYS_RADEC:
	    // no action required
	    break;
	default:
	    ostringstream os;
	    // @todo create a coordsys enum<->string translator
	    os << "Coordinate system "
		<< ctype
		<< " of source "
		<< source.getName()
		<< " is not supported ";
	    throw CARMA_EXCEPTION(
		    services::UnsupportedCoordSysException,os
		  );
    }

    double HA = hourAngleAtElevLimit();

    if ( HA < 0.0 || HA > AstroTime::HOURS_PER_DAY )
	return HA ;

    double ra  = ephem_.getRa();
    double setTimeHrs  = (ra + HA) * AstroTime::HOURS_PER_RADIAN ;

    return AstroTime::modulo24(setTimeHrs);
}

double 
SourceChecker::minutesSinceRise() 
{
    double l = lst();
    double riseT = riseTime();
    if ( l < riseT ) l += AstroTime::HOURS_PER_DAY;

    if ( riseT < 0 )
	// never rises
	return 0.0;

    if ( isUp() && riseT < AstroTime::HOURS_PER_DAY ) {
	return ( ( l - riseT ) * AstroTime::MINUTES_PER_HOUR );
    } else {
	return 0.0;
    }
}

double 
SourceChecker::minutesSinceSet() 
{
    double l = lst();
    double setT= setTime();
    //if ( l < setT ) l += AstroTime::HOURS_PER_DAY;

    if ( setT < 0 )
	// never rises
	return AstroTime::MINUTES_PER_DAY;

    if ( !isUp() && setT < AstroTime::HOURS_PER_DAY ) {
	return ( ( setT - l ) * AstroTime::MINUTES_PER_HOUR );
    } else {
	return 0.0;
    }
}

double 
SourceChecker::hourAngleAtElevation( Angle * elev )
{
    // Compute the hourangle at which the source is at the
    // elevation limit.  This will be added or subtracted to
    // the RA to get the source settSeting and rising times, respectively.
    // Altitude is already taken into account by getRa(), getDec().
    double dec = ephem_.getDec();
    double lat = ephem_.getLocation().getLatitude().radians();
    double cosHA = ( sin( elev->radians() ) - sin(dec)*sin(lat) );
    cosHA /= ( cos(dec)*cos(lat) );

    // trap |cosHA| close to 1 which indicates HA is pi or zero
    // cos(PI) = -1
    // cos(0)  = 1
    // If HA is zero that means the source will never rise above
    // the elevation limit -- i.e. it's rise AND set time are equal
    // to its RA.
    // It HA is pi then it will always be above the
    // elevation limit.  
    //
    if ( cosHA >= 1.0 )
        return NEVER_RISES;
    else if( cosHA <= -1.0 ) 
        return NEVER_SETS;

    double HA = fabs( acos(cosHA) ); // radians
    //cout << " HOUR ANGLE @ ELEVLIM: " <<  HA * 12.0 / M_PI << endl;
    return HA;
}

double 
SourceChecker::hourAngleAtElevLimit()
{
    return hourAngleAtElevation( elevLimit_ );
}


double 
SourceChecker::hourAngleAtElevUpperLimit()
{
    return hourAngleAtElevation( elevUpperLimit_ );
}

string 
SourceChecker::rise() 
{
    try {
	double riseT = riseTime();
	if ( riseT < 0 )
	    return NEVER_RISES_STR;
	if ( riseT > AstroTime::HOURS_PER_DAY )
	    return NEVER_SETS_STR;

	return StringUtils::hms(riseT/AstroTime::HOURS_PER_RADIAN,0);
    } catch (UnsupportedCoordSysException& ucse) {
	return UNSUPPORTED_COORDSYS_STR;
    }
}

string 
SourceChecker::set() 
{
    try {
	double setT = setTime();
	if ( setT < 0 )
	    return NEVER_RISES_STR;
	if ( setT > AstroTime::HOURS_PER_DAY )
	    return NEVER_SETS_STR;

	return StringUtils::hms(setT/AstroTime::HOURS_PER_RADIAN,0);
    } catch (UnsupportedCoordSysException& ucse) {
	return UNSUPPORTED_COORDSYS_STR;
    }
}

double 
SourceChecker::minutesUntilRise()
{
    if ( isUp() ) return 0.0;

    double factor;

    double riseT = riseTime();
    if ( riseT < 0 ) {
    // if the source never rises, return the number of minutes in a day
	factor = AstroTime::HOURS_PER_DAY;
    } else {
	double theLst = lst();
	// ensure answer is not negative if lst is past rise time.
	if ( theLst > riseT )
	    riseT += AstroTime::HOURS_PER_DAY;
        factor = riseT - theLst; // hours
    }

    return  factor * AstroTime::MINUTES_PER_HOUR;
}

double 
SourceChecker::minutesUntilTransit()
{
    if ( needsMjd_ ) setMJD();

    // hour angle in radians
    double HA = at_.hourAngle( getMJD() , ephem_.getRa() );

    // its the negative of the hour angle!
    return ( -HA * AstroTime::HOURS_PER_RADIAN * AstroTime::MINUTES_PER_HOUR );
}

double 
SourceChecker::minutesUntilSet()
{
    if ( ! isUp() ) return 0.0;

    double factor;

    double setT = setTime(); 
    if ( setT > AstroTime::HOURS_PER_DAY )  {
    // if the source never sets , return the number of minutes in a day
	factor = AstroTime::HOURS_PER_DAY;
    } else {
	double theLst = lst();
	// ensure answer is not negative if lst is past set time.
	if ( theLst > setT )
	    setT += AstroTime::HOURS_PER_DAY;
        factor = setT - theLst; // hours
    }
    
    return factor * AstroTime::MINUTES_PER_HOUR;
}


TelescopeStatus 
SourceChecker::getTelescopeStatus()
{ 
    computeTelescopeStatus();
    return telescopeStatus_;
}

double
SourceChecker::minutesUntilBlind()
{
    computeTelescopeStatus();
    if ( telescopeStatus_.canBlind_ == false )
	return 0.0;

    double HAlimit = hourAngleAtElevUpperLimit();
    double HA = at_.hourAngle( getMJD() , ephem_.getRa() );
    double value;
    if ( telescopeStatus_.blinded_ )
	value = HAlimit - HA;
    else 
	value = -HAlimit - HA;

    return ( value * AstroTime::HOURS_PER_RADIAN * AstroTime::MINUTES_PER_HOUR);
}

double
SourceChecker::minutesUntilAzWrapLimit()
{
    computeTelescopeStatus();

    switch ( telescopeStatus_.limits_ ) {
	case LIMIT_NEVER_RISES:
	    //cout << "minutesUntilAzWrap returning minutesUntilRise " << endl;
	    return minutesUntilRise();

	// these two cases go together, both stop on elevation limit
	case LIMIT_HORIZON_STOP:
	case NO_LIMIT:
	    //cout << "minutesUntilAzWrap returning minutesUntil" << endl;
	    return minutesUntilSet();

	// these two cases go together, both stop on positive az wrap
	case LIMIT_AZ_HORIZON_STOP:
	case LIMIT_AZ_STOP:
	default: // to shut the compiler up
	    /*
	    cout << setprecision(10)
		 << "azPositiveWrapLimitLst_ (hr) =" << azPositiveWrapLimitLst_ 
	         << " internal LST = (hr) " << lst()
	         << " internal MJD = " << ephem_.getMJD()
		 << endl;
		 */
	    double hours = azPositiveWrapLimitLst_ - lst();
	    if ( azPositiveWrapLimit_.radians(true) <= M_PI  && hours < 0 )
		hours+=24;
	    return hours*AstroTime::MINUTES_PER_HOUR; //*SIDEREAL_DAY?
    }
}


void
SourceChecker::computeTelescopeStatus() 
{
    if ( needsMjd_ ) setMJD();

    // @see LIMITS TO OBSERVING TIMES FOR ALTAZIMUTH TELESCOPES
    //       by R.A. Laing.  RGO Technical Note 71
    double aplusRadians = azPositiveWrapLimit_.radians();
    double ta2   = tan( aplusRadians );
    ta2 *= ta2;
    double zenithDistance = M_PI_2 - elevLimit_->radians();
    double cz    = cos( zenithDistance );
    double lat   = ephem_.getLocation().getLatitude().radians();
    double slat  = sin( lat );
    double clat  = cos( lat );
    double slat2 = slat*slat;
    double clat2 = clat*clat;
    double dec   = ephem_.getDec();
    double cd    = cos( dec );
    double cd2   = cd*cd;
    double sd    = sin( dec );
    double decNeverSets = M_PI - lat - zenithDistance;
    double decNeverRises = lat - zenithDistance;
    // Dec above which object cannot hit positive wrap limit
    double decAzPositiveWrap = acos(abs(sin( aplusRadians )*clat));
    double a = 1.0 + ta2;
    double b = -2.0*cz*slat*a;
    double c = cz*cz * (1.0 + ta2*slat2) - clat2;
    // Dec below which object hits elevation limit before positive wrap limit
    double decAzNegativeWrap = asin( (-b + sqrt(b*b-4.0*a*c))/(2.0*a) );
    /*
    cout << " SOURCE " << ephem_.getSource().getName()
	 << endl
	 << setprecision(10)
	 << " RA " << ephem_.getRa() * AstroTime::HOURS_PER_RADIAN
	 << endl
	 << " DEC " << ephem_.getDec()
	 << endl
	 << " LAT " << lat*180/M_PI
	 << endl
	 << " ZD  " << zenithDistance*180/M_PI
	 << endl
	 << " AZPLUS " << azPositiveWrapLimit_.degrees()
	 << endl
	 << " AZMINUS" << azNegativeWrapLimit_.degrees()
	 << endl
	 << " DEC NEVER SETS " << decNeverSets*180/M_PI
	 << endl
         << " DEC NEVER RISES " << decNeverRises*180/M_PI
	 << endl
         << " DEC AZ POS WRAP " << decAzPositiveWrap*180/M_PI
	 << endl
         << " DEC AZ NEG WRAP " << decAzNegativeWrap*180/M_PI
	 << endl;
	 */
    azPositiveWrapLimitLst_ = 0.0;
    if ( dec < decNeverRises ) 
	telescopeStatus_.limits_ = LIMIT_NEVER_RISES;
    else {
	if (dec > decAzPositiveWrap ) 
	    telescopeStatus_.limits_ = NO_LIMIT;
	else {
	    if ( dec < decAzNegativeWrap ) 
		telescopeStatus_.limits_ = LIMIT_HORIZON_STOP;
	    else {
		double t1 = ta2*sd*slat*clat;
		double t2 = sqrt(ta2*(cd2-clat2)+cd2);
		double t3 = cd*(1.0+ta2*slat2);
		double ha = acos( (t1 - t2) / t3 );
		// If the wrap limit is in the first two
		// quadrants, then change the sign on 
		// the hour angle. (This case not covered by Laing!)
		if ( azPositiveWrapLimit_.radians(true) <= M_PI )
		    ha = -1.0 * acos( (t1 + t2) / t3 );
		// LST = RA + HA
                azPositiveWrapLimitLst_ = ephem_.getRa() + ha;
		if ( dec < decNeverSets )
		    telescopeStatus_.limits_ = LIMIT_AZ_HORIZON_STOP;
		else 
		    telescopeStatus_.limits_ = LIMIT_AZ_STOP;
	    }
	}
    }
    // convert to hours
    azPositiveWrapLimitLst_ *= AstroTime::HOURS_PER_RADIAN;

    // Now figure out if the source will pass through or is currently in
    // the zenith blind spot.
    // Note we do not have to worry about limited azimuth speed
    // causing a blind spot as in Laing paper, since our 
    // elevation limit due to CloudSat creates a much large blind
    // spot.
    double decCanBlind = lat - ( M_PI_2 - elevUpperLimit_->radians() );
    telescopeStatus_.canBlind_ = ( dec >= decCanBlind );
    telescopeStatus_.blinded_ = false;
    if ( telescopeStatus_.canBlind_ ) {
	double HAlimit = hourAngleAtElevUpperLimit();
        double HA = at_.hourAngle( getMJD() , ephem_.getRa() );
	if ( HA > -HAlimit && HA < HAlimit ) 
	    telescopeStatus_.blinded_ = true;
    }
}

bool
SourceChecker::canBeTrackedOnNegativeWrap()
{
    computeTelescopeStatus();

    double sourceAzMinus = ephem_.getAz() - 2*M_PI; // radians
    // do not return modulo 2PI!
    double azNWLrad = azNegativeWrapLimit_.radians(false); 

    // if the source azimuth can be represented by a value
    // between negative wrap limit and zero, then it
    // can be tracked on the negative wrap
    if ( ( sourceAzMinus >= azNWLrad ) && 
         ( sourceAzMinus <= 0 ) )
        return true;

    return false;

}

services::AzWrapType 
SourceChecker::computeOptimumWrapValue( 
        services::AntennaType antType,
        double antAzDegrees,
        double timeToTrack )
{
    // target source azimuth is always a number between 0 and 360
    double sourceAzDegrees = getAzimuth().degrees();
    bool   negTrack        = canBeTrackedOnNegativeWrap();
    double posLimitDegrees = getAzPositiveWrapLimit().degrees(false);

    // mustNegTrack indicates that the antenna MUST track on the 
    // negative azimuth wrap.  This happens if the source
    // azimuth is larger than the positive azimuth limit of the
    // antenna.
    // mustNegTrack can be true for OVRO and SZA antennas since
    // their positive limits are less than 360 degrees.
    bool   mustNegTrack    = ( sourceAzDegrees > posLimitDegrees );

    // The following are the distances in degrees required to slew to
    // source for the negative, positive, and zero wrap options.
    double slewNegDistance = 0;
    double slewPosDistance  = abs(sourceAzDegrees + 360 - antAzDegrees );
    double slewZeroDistance = abs(sourceAzDegrees - antAzDegrees);

    if ( negTrack && antAzDegrees < 0 ) {
        slewNegDistance = abs( antAzDegrees + 360 - sourceAzDegrees );
        slewZeroDistance = 360 - slewNegDistance;
    }

    if ( negTrack && antAzDegrees > 0 )  {
        slewNegDistance = abs( sourceAzDegrees - antAzDegrees - 360 );
        slewZeroDistance = 360 - slewNegDistance;
    }


    double minutes = minutesUntilAzWrapLimit();
    bool   canTrackWithoutHittingLimit = (minutes >= timeToTrack);
    /*
    cout << boolalpha << "CANTRACK " 
	 << canTrackWithoutHittingLimit 
	 << "[" << minutes << " >= " << timeToTrack << " ?]"
	 << " CANNEGTRACK " 
	 << negTrack  
	 << " MUSTNEGTRACK " 
	 << mustNegTrack  
	 << " AntAz = " << antAzDegrees
	 << " SrcAz = " << sourceAzDegrees
	 << " AZTEST-SLEWNEG [" << slewNegDistance << " < 180"
	 << "] "
	 << ( slewNegDistance < 180 )
	 << " AZTEST-SLEWPOS [" << slewPosDistance << " < 180"
	 << "] "
	 << ( slewPosDistance < 180 )
	 << " AZTEST-SLEWZERo [" << slewZeroDistance << " < 180"
	 << "] "
	 << ( slewZeroDistance < 180 )
	 << endl;
	 */

    switch( antType ) {
        default: /* should never happen */
            return AZWRAP_ZERO;

        case ANT_TYPE_BIMA :
            // Antenna can track for the specified time
            // without hitting the limit AND
            // the antenna is currently pointed in
            // a direction that makes slewing towards
            // the positive limit the shortest slew.
            if ( canTrackWithoutHittingLimit 
              // source is between 0 and 100
              && ( sourceAzDegrees + 360 < posLimitDegrees ) 
              // antenna is in quadrant 3, 4, or 5 and
              // slewing towards positive wrap is shortest distance
              && ( antAzDegrees > 180 && slewPosDistance < 180 )
            )
            {
            // Bima antennas can track past 360, so
            // choose a short slew in this case.
                return AZWRAP_ADD;
            }

            // If either of the above cases is untrue, then
            // decide between negative tracking and zero tracking.
            // If the source can be tracked on the negative wrap
            // (which means the source is between az ~270 and 360) 
            // AND the antenna is currently pointed in a direction 
            // that makes slewing towards the negative wrap limit
            // the shortest slew, return AZWRAP_SUB.
            if ( negTrack && ( slewNegDistance < 180 ) )
                return AZWRAP_SUB;

            return AZWRAP_ZERO;

        // SZA and OVRO obey the same rules.
        // They should never get an AZWRAP_ADD since they
        // cannot track past 360 degrees.
        case ANT_TYPE_SZA :
        case ANT_TYPE_OVRO :
            if ( mustNegTrack )
                return AZWRAP_SUB;
            if ( canTrackWithoutHittingLimit ) {
                if ( negTrack && ( slewNegDistance < 180 ) )
                return AZWRAP_SUB;
            else 
                return AZWRAP_ZERO;
            } else {
                return AZWRAP_SUB;
            }
    }

}

services::NeighborSet 
SourceChecker::getNearest( const std::set<string> & sourceList, bool include,
                   unsigned short numReturn, bool ignoreNorthSouth,
                   coordSysType coordSys, 
                   sourcePntType pType, 
                   float fluxLimit)
{
    ScopedLogNdc ndc("SourceChecker::getNearest");
    
    // must have a reference source.  By default Ephemeris
    // as source == "NONE".
    if ( ephem_.getSource().getName() == NO_SOURCE ) {
        const string msg("A reference source has not been set.") ;
        throw CARMA_EXCEPTION( util::IllegalStateException,  msg );
    }

    NeighborSet neighborSet;

    // debug
    ostringstream  tableOS;
    ostringstream headOS;
    headOS << "# N  SCOPEN FCOPEN KEYS SET_DIFF SET_INT  SRC_LK  FL_LK OPT_LK NB_CT WHILE  TOTAL";
    //programLogNoticeIfPossible( headOS.str() );
    StopWatch timer( StopWatch::CPU_TIME );
    timer.start();
    double elapsed = 0;

    // This is the catalog of all system sources
    SourceCatalog sourceCatalog;
    sourceCatalog.open( SourceCatalog::defaultCatalog() );

    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0;
    ostringstream debugOS;
    debugOS << " Time for SourceCatalog::open(): " << elapsed << endl;
    timer.start();
    tableOS << numReturn << "   " << elapsed;


    // Locate the radio or optical sources in SourceCatalog by
    // intersecting it with the Flux or Optical catalog.
    // NB: could also do this by looking at Source.pntType for all
    // sources in SourceCatalog?
    std::set<string> compareSourceNames;
    FluxCatalog fcatalog;
    OpticalCatalog ocatalog;
    switch ( pType ) {
	// for now if pType = BOTH, you get RADIO.
        default:
        case PNT_RADIO:
            fcatalog.open( FluxCatalog::defaultCatalog() );
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0;
    debugOS << " Time for FluxCatalog::open(): " << elapsed << endl;
    tableOS << "   " << elapsed;
    timer.start();
            compareSourceNames = keys( fcatalog.getSourceMap() );
            break;
        case PNT_OPTICAL:
            ocatalog.open( OpticalCatalog::defaultCatalog() );
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0 ;
    debugOS << " Time for OpticalCatalog::open(): " << elapsed << endl;
    tableOS << "   " << elapsed;
    timer.start();
            compareSourceNames = keys( ocatalog.getSourceMap() );
            break;
    }
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0 ;
    tableOS << "   " << elapsed;
    debugOS << " Time for for keys(catalog): " << elapsed << endl;
    timer.start();

    /* DEBUG
    {
	ostringstream os;
	os << " compareSourceNames return length is " 
	    << compareSourceNames.size()
	    ;
	programLogNoticeIfPossible( os.str() );
    }
    */

    // Of the optical or radio sources, either exclude or
    // include the sources given in sourceList.
    std::set<string> filteredCatalogSources;
    std::set<string>::iterator si;
    std::set<string>::iterator se;
    if ( ! sourceList.empty() ) {
	// Note we cannot use filteredCatalogSources().begin() as the last
	// parameter to set_intersection() or set_difference()
	// because it is not an OutputIterator.  
	// inserter() is an OutputIterator.
	// Using filteredCatalogSources().begin() gives a less than
	// useful compiler error that passing const as 'this; argument
	// discards qualifiers.
	if ( include ) {
	    set_intersection( compareSourceNames.begin(), 
                          compareSourceNames.end(),
                          sourceList.begin(), sourceList.end(),
                          inserter( filteredCatalogSources, 
                          filteredCatalogSources.begin() ) 
			    );
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0 ;
    tableOS << "   " << elapsed;
    debugOS << " Time for for set_intersection: " << elapsed << endl;
    timer.start();
	} else {
	    set_difference( compareSourceNames.begin(), 
                        compareSourceNames.end(),
                        sourceList.begin(), sourceList.end(),
                        inserter( filteredCatalogSources, 
                        filteredCatalogSources.begin() ) 
			  );
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0 ;
    tableOS << "   " << elapsed;
    debugOS << " Time for for set_difference: " << elapsed << endl;
    timer.start();
	}

	/* DEBUG
	{
	    ostringstream os;
	    os << " filteredCatalogSources returned size is " 
		<< filteredCatalogSources .size();
	    programLogNoticeIfPossible( os.str() );
	}
	*/
	    
        si = filteredCatalogSources.begin();
        se = filteredCatalogSources.end();

    } else {
        si = compareSourceNames.begin();
        se = compareSourceNames.end();
    }

    // Now we must also intersect the System source catalog with the Flux or
    // Optical catalog because the latter may contain source that are
    // not in the System catalog (e.g. pre-J2000 flux source names).

    std::set<string> sourcesToUse;
    std::set<string> systemSources = keys( sourceCatalog.getSourceMap() );
    set_intersection( systemSources.begin(), systemSources.end(),
	              si, se,
	              inserter( sourcesToUse, sourcesToUse.begin() ) );

    si = sourcesToUse.begin();
    se = sourcesToUse.end();

    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0 ;
    tableOS << "   " << elapsed;
    debugOS << " Time for for set_intersection: " << elapsed << endl;
    timer.start();

    /* DEBUG
	{
	    ostringstream os;
	    os << " sourcesToUse returned size is " 
		<< sourcesToUse.size();
	    programLogNoticeIfPossible( os.str() );
	}
	*/

    const double elevLowDegrees  = elevLimit_->degrees();
    const double elevHighDegrees = elevUpperLimit_->degrees();
    const Frequency frequency( ephem_.getFreq(), services::HZ ) ;
    const Frequency deltaFreq( 55, services::GHZ ) ;
    bool okFlux;
    double fltime = 0;
    double sltime = 0;
    double oltime = 0;
    double nbtime = 0;
    StopWatch lookupTimer( StopWatch::CPU_TIME );
    double brightness;
    bool optical;
    double mjd = ephem_.getMJD();
    Ephemeris localEphemeris;
    while ( si != se ) {
        // @todo handle lookup failure here
        const string skey = *si;
        lookupTimer.start();
        Source source = sourceCatalog.lookup( skey );
        lookupTimer.stop();
        sltime += lookupTimer.getElapsedTime();
        // If user does not want to cross north-south line
        // for sources near or transit (i.e. large slew) then
        // weed out said sources here.
        if (   !ignoreNorthSouth 
            && slewWouldCrossNorthSouthBoundary( source )
               ) 
        {
        /* DEBUG
            ostringstream os;
            os << " Skipping source " << source.getName()
            << " due to boundary conditions ";
            programLogNoticeIfPossible( os.str() );
        */
            si++;
            continue;
        }
        switch ( pType ) {
            default:
            case PNT_RADIO:
            {
              try {

                // limit lookback to 100 days before the MJD that
                // has been set internally, NOT today's MJD.
                double now = Time::MJD();
                double diffMJD = now - mjd;
                // Set lookback time to be 100 days previous to neighbor time.
                // Allow for neigbor time to be in the future up to
                // 100 days, else set search to be 100 days before now.
                double lookback = diffMJD > -100 ? diffMJD + 100 : 100;

                /* DEBUG
                 ostringstream os;
                os << " Looking up "
                    << skey 
                    << " with freq= " << frequency 
                    << " deltaF = " << deltaFreq
                    << " daysback = " << lookback
                    ;
                programLogNoticeIfPossible( os.str() );
                */

                lookupTimer.start();
                FluxSource fs = 
                    fcatalog.lookup(skey, frequency, deltaFreq, lookback);
                lookupTimer.stop();
                fltime += lookupTimer.getElapsedTime();
                brightness = fs.getFlux().jansky();
                optical = false;
                okFlux = ( brightness >= fluxLimit );
              } catch ( const SourceNotFoundException & ex ) {
                lookupTimer.stop();
                /* actually this will happen a lot because
                 * there are many sources for which CARMA
                 * has never measured a flux
                ostringstream os;
                os << "Failed to find "
                    << skey
                    << "in flux catalog . Skipping this source."
                    ;
                programLogWarnIfPossible( os.str() );
                */
                ++si;
                continue;
              }
            }
            break;
            case PNT_OPTICAL:
            {
              try {
                lookupTimer.start();
                Star star = ocatalog.lookup( skey );
                lookupTimer.stop();
                oltime += lookupTimer.getElapsedTime();
                brightness = star.getMagnitude();
                optical = true ;
                // magnitudes are backwards; higher number is less
                // bright.
                okFlux = ( brightness <= fluxLimit );
              } catch ( const SourceNotFoundException & ex ) {
                lookupTimer.stop();
                ostringstream os;
                os << "Failed to find "
                    << skey 
                    << "in optical catalog. Skipping this source."
                    ;
                programLogWarnIfPossible( os.str() );
                ++si;
                continue;
              }
            }
            break;
            }

        if ( okFlux )
        {
            // Neighbor creation moderately expensive, so
            // don't do it unless the source passes the flux test.
            // Precomputing the elevation doesn't save us
            // any time over doing it inside createNeighbor()
            // as both cases require instantiating an Ephemeris
            // for computation.
            lookupTimer.start();
            Neighbor neighbor = createNeighbor( source, coordSys, localEphemeris );
            lookupTimer.stop();
            nbtime += lookupTimer.getElapsedTime();
            const double nElev = neighbor.getElevation();
            if (   nElev >= elevLowDegrees 
                && nElev <= elevHighDegrees ) 
            {
                neighbor.setBrightness( brightness );
                neighbor.setOptical( optical );
                neighborSet.insert( neighbor );
                /* DEBUG
                ostringstream os ;
                os << " Inserting neighbor " << neighbor.getName();
                programLogNoticeIfPossible( os.str() );
                */
            }
        }
        si++;
    } // end while

    if ( lookupTimer.isRunning() ) lookupTimer.stop();
    sltime *= 1000000.0;
    fltime *= 1000000.0;
    oltime *= 1000000.0;
    nbtime *= 1000000.0;
    timer.stop();
    elapsed = timer.getElapsedTime()*1000000.0;
    debugOS 
        << " Time for source lookups " << sltime << endl
        << " Time for flux lookups " << fltime << endl
        << " Time for optical lookups " << oltime << endl
        << " Time for Neighbor creation " << nbtime << endl
        << " Time for big while loop " << elapsed  << endl;
    tableOS << "   " << sltime 
            << "   " << fltime 
            << "   " << oltime 
            << "   " << nbtime 
            << "   " << elapsed; 
    timer.start();

 
    if ( numReturn < neighborSet.size() ) {
	// remove all but top numReturn entries
        NeighborSet::iterator ip = neighborSet.begin();

        // increment the pointer.  note + and +=
        // are not defined for std::set
        for( int i=0; i< numReturn; ++i, ++ip) { }; // empty loop;
        neighborSet.erase( ip, neighborSet.end() );
    }

    timer.stop();
    elapsed = timer.getCumulativeElapsedTime()*1000000.0;
    debugOS << " Total Time (microsec): " << elapsed << endl;
    tableOS << "   " << elapsed;

    //programLogNoticeIfPossible( debugOS.str() );
    //programLogNoticeIfPossible( tableOS.str() );
    return neighborSet;
}

Neighbor 
SourceChecker::createNeighbor( const Source & neighborSource,
			       const coordSysType coordSys, Ephemeris & localEphemeris)
{
    ScopedLogNdc ndc(" SourceChecker::createNeighbor ");
    Neighbor neighbor;
    neighbor.setReference( ephem_.getSource().getName() );
    neighbor.setMJD( ephem_.getMJD() );
    double refAzimuth   = ephem_.getAz(); // radians
    double refElevation = ephem_.getEl(); // radians

    double srcAzimuth;
    double srcElevation;
    {
        MasterGuardLockType lock( gMasterGuard );
        localEphemeris.setSource( neighborSource );
        localEphemeris.setMJD( neighbor.getMJD() );
        srcAzimuth   = localEphemeris.getAz(); // radians
        srcElevation = localEphemeris.getEl(); // radians
    }
    /*{
	ostringstream os;
	os << "Source = " 
	   << neighborSource.getName()
	   << "AZ = " << srcAzimuth*RAD2DEG
	   << "EL = " << srcElevation*RAD2DEG
	   << "Ref = " << neighbor.getReference()
	   << "AZ  = "<< refAzimuth*RAD2DEG
	   << "EL = " << refElevation*RAD2DEG
	   ;
	programLogNoticeIfPossible( os.str() );
    }*/
    double diffAz, diffEl, distance, dsq;
    switch( coordSys ) {
	default:
	case COORDSYS_AZEL:
	    // if the difference between the azimuths is
	    // greater than 180, then the antennas will
	    // slew in the shorter direction.  
	    // @TODO Handle azimuth wraps with computeOptimumWrap()?
	    diffAz = refAzimuth - srcAzimuth;
	    if ( diffAz > M_PI ) diffAz -= TWO_PI;
	    if ( diffAz < -M_PI ) diffAz += TWO_PI;
	    diffEl = refElevation - srcElevation;
	    dsq = diffAz*diffAz + diffEl*diffEl;
	    distance = pow(dsq, 0.5);
	    break;
	case COORDSYS_RADEC:
	    // spherical geometric distance is the same regardless
	    // of whether we actually use RA,DEC or AZ,EL
	    distance = sin( refElevation )*sin( srcElevation ) 
		     + cos( refElevation)*cos( srcElevation )
		     * cos( refAzimuth - srcAzimuth );
	    distance = acos( distance );
	    break;
    }


    neighbor.setName( neighborSource.getName() );
    neighbor.setDistance( distance * RAD2DEG );
    neighbor.setAzimuth( srcAzimuth * RAD2DEG );
    neighbor.setElevation (srcElevation * RAD2DEG);
    return neighbor;
}

bool
SourceChecker::slewWouldCrossNorthSouthBoundary( const Source & source )
{
    static const double ONE_TENTH_HOUR_IN_RADIANS = 0.01745;
    const double refRa  = ephem_.getRa(); // radians
    const double refDec = ephem_.getDec(); // radians
    const double latitude = ephem_.getLocation().getLatitude().radians();
    const double refHA  = at_.hourAngle( getMJD(), refRa );
    const double srcRa = source.getXCoordinate().radians();
    const double srcDec = source.getYCoordinate().radians();
    const double srcHA  = at_.hourAngle( getMJD(), srcRa );
    return (  (   ( refDec > latitude && srcDec < latitude )
	       || ( refDec < latitude && srcDec > latitude ) 
	      )
	   && (   ( srcHA > -ONE_TENTH_HOUR_IN_RADIANS )
	       || ( refHA > -ONE_TENTH_HOUR_IN_RADIANS )
	      )
	   );
}
