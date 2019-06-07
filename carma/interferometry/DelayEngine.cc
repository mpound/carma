/**
 * @file 
 * This is the implementation of the Delay Engine class.
 * $Id: DelayEngine.cc,v 1.47 2013/10/01 23:27:45 scott Exp $
 * @author Marc Pound
 */
#include <cmath>
#include <iostream>
#include <iomanip>
#include <limits>
#include <stdexcept>
#include <unistd.h>
#include <valarray>
#include <vector>

#include "carma/interferometry/DelayEngine.h"
#include "carma/interferometry/DelayInfo.h"

#include "carma/monitor/DelayEngineSubsystem.h"

#include "carma/services/Angle.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Delay.h"
#include "carma/services/Distance.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Frequency.h"
#include "carma/services/HourAngle.h"
#include "carma/services/Location.h"
#include "carma/services/Pressure.h"
#include "carma/services/Physical.h"
#include "carma/services/SourceCatalog.h"
#include "carma/services/SourceNotFoundException.h"
#include "carma/services/Temperature.h"
#include "carma/services/Vector.h"
#include "carma/services/Velocity.h"

#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Logger.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma::interferometry;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace carma::monitor;


namespace { // anonymous
    const Trace::TraceLevel lowTraceLevel      = Trace::TRACE3;
    const Trace::TraceLevel moderateTraceLevel = Trace::TRACE5;
    const Trace::TraceLevel verboseTraceLevel  = Trace::TRACE7;
    ::pthread_mutex_t gEphemGuard = PTHREAD_MUTEX_INITIALIZER;

    typedef ScopedLock< ::pthread_mutex_t > EphemLock;
}

const string carma::interferometry::DelayEngine::RaDec = "RADEC";
const string carma::interferometry::DelayEngine::AzEl  = "AZEL";

DelayEngine::DelayEngine(void)
    : arrayRefPtSet_( false ),
      freqSet_( false ),
      freq_( 100.0, "GHz" )
{
    initialize();
}

DelayEngine::~DelayEngine()
{
}

//=======================================
// Public methods
//=======================================
void 
DelayEngine::setAntennaCoordinates(unsigned short antennaNo,
                                   const Location& location,
                                   const Length& axisMis)
{
    setAntennaCoordinates(antennaNo, 
            location.getLongitude().radians(),
            location.getLatitude().radians(),
            location.getAltitude().meters(),
            ANTCOORD_LLA, axisMis.meters());
}

void 
DelayEngine::setAxisMisalignment(unsigned short antennaNo,
                                 const Length& axisMis)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        modified_.axisMis.at(antid)  = axisMis.meters();
        computeDelays(antennaNo); // compute and push to monitor stream

    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }

}

void 
DelayEngine::setAntennaCoordinates(unsigned short antennaNo,
                                   double x,
                                   double y,
                                   double z,
                                   AntennaCoordinateType acType,
                                   double axisMis)
{
    try {

        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;

        ostringstream os;
        os << "Setting location ["
                << x << " , "
                << y << " , "
                << z << "]   "
                << "ACType=" 
                << acType 
                << " for antenna "
                << antennaNo;
        programLogInfoIfPossible(os.str());
        // If the array reference point has not yet been set, then
        // we cannot compute XYZ given LLA or UEN.  Therefore, throw
        // an exception here.
        if ( ! arrayRefPtSet_ ) {
            os << "Array Reference Point has not yet been set. "
               << "Call setArrayReferencePoint(Location) first.";
            throw CARMA_EXCEPTION( ErrorException, os.str().c_str());
        }

        // if the frequency is still < 0, then it was never set.
        if ( ! freqSet_ ) {
            os << "LO1 frequency has not yet been set for Antenna #" 
               << antennaNo
               <<". Call setAntennaLOFreq(" << antennaNo
               <<",freq)."
           << "  Defaulting to 100 GHz. ";

            programLogErrorIfPossible( os.str() );
        }

        modified_.axisMis.at(antid)  = axisMis;

        Vector<double> xyz;
        Vector<double> xyzref;
        Vector<double> lla;
        Vector<double> uen;


        ostringstream osa;
        switch (acType) {

            default:
            case ANTCOORD_TOPO_XYZ:
                //**************************
                // x is X, y is Y, z is Z
                //**************************

                CARMA_CPTRACE(verboseTraceLevel,
                        "setAntennaCoordinates in TOPO_XYZ");
                modified_.X.at(antid) = x;
                modified_.Y.at(antid) = y;
                modified_.Z.at(antid) = z;
                // first get UEN for the reference latitude
                
                uen = arrayRefPt_.topoXYZToUen(x,y,z,arrayRefPt_.latitude());

                // then convert to LLA of this antenna
                lla = arrayRefPt_.getLla(uen[0],uen[1],uen[2]);
                modified_.longitude.at(antid) = lla[0];
                modified_.latitude.at(antid)  = lla[1];
                modified_.altitude.at(antid)  = lla[2];
                osa << "Antenna " << antennaNo 
                    << "\nCALCULATED UEN VECTOR: " << uen 
                    << "\nCALCULATED LLA VECTOR: " << lla
                    << "\nINPUT XYZ: "
                    << modified_.X.at(antid) << " "
                    << modified_.Y.at(antid) << " "
                    << modified_.Z.at(antid) << " "
                    ;
                CARMA_CPTRACE(verboseTraceLevel,osa.str());
                break;

            case ANTCOORD_GEO_XYZ:
                //**************************************
                // x is X+refX, y is Y+refY, z is Z+refZ
                //**************************************
                
                // compute the xyz of the reference point, so
                // we can subtract it off.
                CARMA_CPTRACE(verboseTraceLevel,
                        "setAntennaCoordinates in GEO_XYZ");
                xyzref = arrayRefPt_.getXyz(0.0,0.0,0.0,true);
                modified_.X.at(antid) = x - xyzref[0];
                modified_.Y.at(antid) = y - xyzref[1];
                modified_.Z.at(antid) = z - xyzref[2];

                // first get UEN for the reference latitude
                uen = arrayRefPt_.topoXYZToUen(
                        modified_.X.at(antid),
                        modified_.Y.at(antid),
                        modified_.Z.at(antid),
                        arrayRefPt_.latitude()
                        );
                // then convert to LLA of this antenna
                lla = arrayRefPt_.getLla(uen[0],uen[1],uen[2]);
                modified_.longitude.at(antid) = lla[0];
                modified_.latitude.at(antid)  = lla[1];
                modified_.altitude.at(antid)  = lla[2];
                osa << "Antenna " << antennaNo 
                    << "\nCALCULATED UEN VECTOR: " << uen 
                    << "\nCALCULATED LLA VECTOR: " << lla
                    << "\nCALCULATED XYZ: "
                    << modified_.X.at(antid) << " "
                    << modified_.Y.at(antid) << " "
                    << modified_.Z.at(antid) << " "
                    ;
                CARMA_CPTRACE(verboseTraceLevel,osa.str());

                break;

            case ANTCOORD_LLA:
                //**********************************************
                // x is Longitude, y is Latitude, z is Altitude
                //**********************************************
                
                CARMA_CPTRACE(verboseTraceLevel,
                        "setAntennaCoordinates in LLA");
                modified_.longitude.at(antid) = x;
                modified_.latitude.at(antid)  = y;
                modified_.altitude.at(antid)  = z;
                uen = arrayRefPt_.getUen(
                            Angle(x,"radians"),
                            Angle(y,"radians"),
                            Length(z,"meters")
                            );

                // boolean is false because we want topocentric
                xyz = arrayRefPt_.getXyz(uen[0], uen[1], uen[2], false);
                xyzref = arrayRefPt_.getXyz(0.0, 0.0, 0.0, false);
                modified_.X.at(antid) = xyz[0] - xyzref[0];
                modified_.Y.at(antid) = xyz[1] - xyzref[1];
                modified_.Z.at(antid) = xyz[2] - xyzref[2];
                osa << "Antenna " << antennaNo 
                    << "\nCALCULATED UEN VECTOR: " << uen 
                    << "\nCALCULATED XYZ VECTOR: " << xyz
                    << "\nCALCULATED XYZREF VECTOR: " << xyzref;
                CARMA_CPTRACE(verboseTraceLevel,osa.str());
                break;

            case ANTCOORD_UEN:
                //********************************
                // x is Up, y is East, z is North
                //********************************
                
                // get the X,Y,Z coords. The boolean is false 
                // because we want topocentric
                CARMA_CPTRACE(verboseTraceLevel,
                        "setAntennaCoordinates in UEN");
                xyz = arrayRefPt_.getXyz(x, y, z, false);
                xyzref = arrayRefPt_.getXyz(0.0, 0.0, 0.0, false);
                modified_.X.at(antid) = xyz[0] - xyzref[0];
                modified_.Y.at(antid) = xyz[1] - xyzref[1];
                modified_.Z.at(antid) = xyz[2] - xyzref[2];

                // Get the Lon,Lat,Alt coords.
                lla = arrayRefPt_.getLla(x, y, z);
                modified_.longitude.at(antid) = lla[0];
                modified_.latitude.at(antid)  = lla[1];
                modified_.altitude.at(antid)  = lla[2];
                osa << "Antenna " << antennaNo 
                    << "\nCALCULATED LLA VECTOR: " << lla 
                    << "\nCALCULATED XYZ VECTOR: " << xyz
                    << "\nCALCULATED XYZREF VECTOR: " << xyzref;
                CARMA_CPTRACE(verboseTraceLevel,osa.str());
                break;

        } // switch

        computeDelays(antennaNo); // compute and push to monitor stream

    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }

}

void 
DelayEngine::setArrayReferencePoint(const Location& location)
{
    /*
    { 
        ostringstream os;
        os.setf(ios::fixed);
        os << "setArrayReferencePoint called with Location = " << location;
        programLogInfoIfPossible(os.str());
    }
    */

    // create the AntennaCoordinates object for the array reference point
    arrayRefPt_.setLongitude(location.getLongitude());
    arrayRefPt_.setLatitude(location.getLatitude());
    arrayRefPt_.setAltitude(location.getAltitude());

    // Copy position over for calculating the tropospheric delay
    // at the reference point. Assign to all members so the
    // calculation is the same regardless of the antenna index.
    unsigned short rpsize = refPtDelay_.size();
    for ( unsigned short i = 0; i < rpsize; i++)
    {
    refPtDelay_[i].longitude.assign(
        refPtDelay_[i].longitude.size(),
        arrayRefPt_.longitude().radians()
        );
    refPtDelay_[i].latitude.assign(
        refPtDelay_[i].latitude.size(),
        arrayRefPt_.latitude().radians()
        );
    refPtDelay_[i].altitude.assign(
        refPtDelay_[i].altitude.size(),
        arrayRefPt_.altitude().meters()
        );
    }
    arrayRefPtSet_ = true;
}

void 
DelayEngine::setAntennaRaDec(unsigned short antennaNo,
                             double mjd,
                             double pntra,
                             double pntdec,
                             double phsra,
                             double phsdec,
                             bool logAction,
                             const string& source )
{
    if ( logAction ) {
    ostringstream logOs;
    logOs
            << " setAntennaRaDec: Antenna #" << antennaNo
            << " RA  = " << pntra
            << " DEC = " << pntdec
            << " phaseRA  = " << phsra
            << " phaseDEC = " << phsdec
            << " MJD = " << mjd
            << " SOURCE = " << source;
    programLogInfoIfPossible( logOs.str() );
    }


    // Convert the J2000 coordinates to current equinox
    // Proper motion = parallax = radial velocity = zero.
    //FIX: DONT USE EPHEMERIS SOURCE TO PASS THIS INFO THROUGH.
    //JUST SET MODIFIED.PHSRA ETC DIRECTLY AND ONLY CALCULATE
    //AZEL IN !isJ2000 BRANCH OF finishCoordinates
    Source tmpSrc("DelayEngineTmpSrc",
            Angle(pntra,"radians"),
            Angle(pntdec,"radians"),
            Velocity(0.0,"km/s"), 
            Distance(0.0,"km")
          );

    {
    const EphemLock lock( gEphemGuard );
    ephemeris_.setSource( tmpSrc );
    }

    finishCoordinates(antennaNo, 
                           mjd, 
                           (double)0.0, (double)0.0,
                           (double)0.0, (double)0.0,
                           //phsra-pntra, phsdec-pntdec,
                           false, source);
}

/*
void 
DelayEngine::setAntennaSource(unsigned short antennaNo,
                              const string& name,
                              double mjd,
                              double raPointingOffset,
                              double decPointingOffset,
                              double raPhaseCenterOffset,
                              double decPhaseCenterOffset)
{
    try {

        ostringstream logOs;
    logOs << setiosflags(ios::fixed)
            << " setAntennaSource: Antenna#"  << antennaNo
            << " Source: " << name
            << " MJD = " << setprecision(5) << mjd;
    //programLogInfoIfPossible( logOs.str() );

    {
        const EphemLock lock( gEphemGuard );
        ephemeris_.setSource( name );
    }

        finishCoordinates(antennaNo, mjd, 
                raPointingOffset, decPointingOffset, 
                raPhaseCenterOffset, decPhaseCenterOffset,
                true, name);
    } catch ( const SourceNotFoundException& snfe ) {
        snfe.logException(log4cpp::Priority::ERROR);
        return;
    } catch (...)  {
        ostringstream os;
        os << "DelayEngine::setAntennaSource - "
           << "Unknown exception caught "
           << "Source name is " << name
           << " antenna # is " << antennaNo;
        programLogErrorIfPossible( os.str() );
    }
}
*/

void 
DelayEngine::finishCoordinates(unsigned short antennaNo,
                                  double mjd,
                                  double raPointingOffset,
                                  double decPointingOffset,
                                  double raPhaseCenterOffset,
                                  double decPhaseCenterOffset,
                                  bool isJ2000, const string & source )
{
    try {

    const string methodName = "DE::finishCoordinates()";
    CARMA_CPTRACE(lowTraceLevel, methodName << " - Entering" );

        // mutex protect this method.
    const EphemLock lock( gEphemGuard );

        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
    // save stuff like antenna position and mjd.
    antepenultimate_.copy( antid, penultimate_ );
    penultimate_.copy( antid, current_ );
    //must also copy over reference point values so that
    //delays computed from differences use the same MJD.
    refPtDelay_.at(ANTEPENULTIMATE)
        .copy( antid, refPtDelay_.at(PENULTIMATE) );
    refPtDelay_.at(PENULTIMATE)
        .copy( antid, refPtDelay_.at(CURRENT) );

        // now copy new values into "modified" DelayInfo.
        // modified will be copied to current just before
        // a delay calcultion is done.
        modified_.timestamp.at(antid) = mjd;
        current_.timestamp.at(antid)  = mjd;
        refPtDelay_.at(CURRENT).timestamp.at(antid)  = mjd;

    modified_.source.at(antid) = source;
    refPtDelay_.at(CURRENT).source.at(antid) = source;

        ephemeris_.setMJD(mjd);
        Location loc( Angle(modified_.longitude.at(antid),"radians"),
                      Angle(modified_.latitude.at(antid),"radians"),
                      Length(modified_.altitude.at(antid),"meters")
                         );
        ephemeris_.setLocation(loc);
        ephemeris_.setRaDecOffsets(raPointingOffset,decPointingOffset);


        if (isJ2000) {
            // Source coordinates are J2000, so we need to
            // do topocentric correction.

            // POINTING COORDINATES
            modified_.pntRa.at(antid)    = ephemeris_.getRa(),
            modified_.pntDec.at(antid)   = ephemeris_.getDec(),
            // for completeness, set the AzEl too.
            modified_.pntAz.at(antid)    = ephemeris_.getAz();
            double elev = ephemeris_.getEl();
            modified_.pntEl.at(antid)    = elev < 0.0 ? 0.0 : elev;

            // PHASE CENTER COORDINATES
            ephemeris_.setRaDecOffsets(raPhaseCenterOffset,decPhaseCenterOffset);
            ephemeris_.setMJD(mjd); // don't know if we need this a 2nd time.
            modified_.phsRa.at(antid)    = ephemeris_.getRa();
            modified_.phsDec.at(antid)   = ephemeris_.getDec();
            // for completeness, set the AzEl too.
            modified_.phsAz.at(antid)    = ephemeris_.getAz();
            elev                        = ephemeris_.getEl();
            modified_.phsEl.at(antid)    = elev < 0.0 ? 0.0 : elev;

        } else { 
            // Topocentric correction already done. Just use 
            // Ephemeris to calculate the AzEl.

            // POINTING COORDINATES
            Source s = ephemeris_.getSource();
            modified_.pntRa.at(antid)    = s.getXCoordinate().radians();
            modified_.pntDec.at(antid)   = s.getYCoordinate().radians();
            Vector<double>azel          = ephemeris_.getAzEl(
                                            mjd,
                                            modified_.pntRa.at(antid),
                                            modified_.pntDec.at(antid)
                                            );
            modified_.pntAz.at(antid)    = azel[0];
            modified_.pntEl.at(antid)    = azel[1] < 0.0 ? 0.0 : azel[1];

            // PHASE CENTER COORDINATES
            ephemeris_.setRaDecOffsets(raPhaseCenterOffset,decPhaseCenterOffset);
        //note the above offsets do not take effect if ephemeris_.setSource(Source)
        //was called. peter is fixing the ephem code. 12/21/2005
        //in this case we are ok, since this branch is not reached unless offsets=0,0 anyway.
            s = ephemeris_.getSource();
            modified_.phsRa.at(antid)    = s.getXCoordinate().radians();
            modified_.phsDec.at(antid)   = s.getYCoordinate().radians();
            azel                        = ephemeris_.getAzEl(
                                            mjd,
                                            modified_.phsRa.at(antid),
                                            modified_.phsDec.at(antid)
                                            );
            modified_.phsAz.at(antid)    = azel[0];
            modified_.phsEl.at(antid)    = azel[1] < 0.0 ? 0.0 : azel[1];
        }

        // reset the pointing state. A call to this method means RA,DEC.
        modified_.pntState.at(antid)  = DelayInfo::RADEC; 

        // now repeat the exercise for the reference point delay object.
        // so that we can calculate its tropospheric delay
        Location refloc ( 
        Angle(refPtDelay_.at(CURRENT).longitude.at(antid),"radians"),
                Angle(refPtDelay_.at(CURRENT).latitude.at(antid),"radians"),
                Length(refPtDelay_.at(CURRENT).altitude.at(antid),"meters")
                         );
        ephemeris_.setLocation(refloc);
        ephemeris_.setRaDecOffsets(raPointingOffset,decPointingOffset);

        if (isJ2000) {
            // Source coordiantes are J2000, so we need to
            // do topocentric correction.

            // POINTING COORDINATES
            refPtDelay_.at(CURRENT).pntRa.at(antid)    = ephemeris_.getRa();
            refPtDelay_.at(CURRENT).pntDec.at(antid)   = ephemeris_.getDec();
            // for completeness, set the AzEl too.
            refPtDelay_.at(CURRENT).pntAz.at(antid)    = ephemeris_.getAz();
            double elev = ephemeris_.getEl();
            refPtDelay_.at(CURRENT).pntEl.at(antid)    = elev < 0.0 ? 0.0 : elev;

            // PHASE CENTER COORDINATES
            ephemeris_.setRaDecOffsets(raPhaseCenterOffset,decPhaseCenterOffset);
            ephemeris_.setMJD(mjd); // don't know if we need this a 2nd time.
            refPtDelay_.at(CURRENT).phsRa.at(antid)    = ephemeris_.getRa();
            refPtDelay_.at(CURRENT).phsDec.at(antid)   = ephemeris_.getDec();
            // for completeness, set the AzEl too.
            refPtDelay_.at(CURRENT).phsAz.at(antid)    = ephemeris_.getAz();
            elev                          = ephemeris_.getEl();
            refPtDelay_.at(CURRENT).phsEl.at(antid)    = elev < 0.0 ? 0.0 : elev;

        } else { 
            // Topocentric correction already done. Just use 
            // Ephemeris to calculate the AzEl.

            // POINTING COORDINATES
            Source s = ephemeris_.getSource();
            refPtDelay_.at(CURRENT).pntRa.at(antid)  = s.getXCoordinate().radians();
            refPtDelay_.at(CURRENT).pntDec.at(antid) = s.getYCoordinate().radians();
            Vector<double>azel          = ephemeris_.getAzEl(
                                            mjd,
                                            refPtDelay_.at(CURRENT).pntRa.at(antid),
                                            refPtDelay_.at(CURRENT).pntDec.at(antid)
                                            );
            refPtDelay_.at(CURRENT).pntAz.at(antid)  = azel[0];
            refPtDelay_.at(CURRENT).pntEl.at(antid)  = azel[1] < 0.0 ? 0.0 : azel[1];

            // PHASE CENTER COORDINATES
            ephemeris_.setRaDecOffsets(raPhaseCenterOffset,decPhaseCenterOffset);
            s = ephemeris_.getSource();
            refPtDelay_.at(CURRENT).phsRa.at(antid)  = s.getXCoordinate().radians();
            refPtDelay_.at(CURRENT).phsDec.at(antid) = s.getYCoordinate().radians();
            azel                        = ephemeris_.getAzEl(
                                            mjd,
                                            refPtDelay_.at(CURRENT).phsRa.at(antid),
                                            refPtDelay_.at(CURRENT).phsDec.at(antid)
                                            );
            refPtDelay_.at(CURRENT).phsAz.at(antid) = azel[0];
            refPtDelay_.at(CURRENT).phsEl.at(antid) = azel[1] < 0.0 ? 0.0 : azel[1];
        }

        // reset the pointing state. A call to this method means RA,DEC.
        refPtDelay_.at(CURRENT).pntState.at(antid)  = DelayInfo::RADEC; 
        ostringstream os;
        os << methodName << " - with coordinates: "
           << " Source = "  << modified_.source.at(antid)
           << " RA = "  << modified_.pntRa.at(antid)
           << " DEC = " << modified_.pntDec.at(antid)
           << " AZ = "  << modified_.pntAz.at(antid)
           << " EL = "  << modified_.pntEl.at(antid);

    // must copy all this over or it will take 3 intervals
    // for delays to catch up.
        current_.copy( antid, modified_ );
        CARMA_CPTRACE(lowTraceLevel,os.str());
    CARMA_CPTRACE(lowTraceLevel, methodName << " - Exiting" );


    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }

}

void 
DelayEngine::setAntennaAzEl(unsigned short antennaNo,
                                double mjd,
                                double pntaz,
                                double pntel,
                                double phsaz,
                                double phsel)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
    {
        // avoid dual-lock on gEphemGuard in computeDelays
            ScopedLockManager< ::pthread_mutex_t > lockManager( gEphemGuard );
        lockManager.lock();

        antepenultimate_.copy( antid,penultimate_ );
        penultimate_.copy( antid, current_ );
        refPtDelay_.at(ANTEPENULTIMATE)
        .copy( antid, refPtDelay_.at(PENULTIMATE) );
        refPtDelay_.at(PENULTIMATE)
        .copy( antid, refPtDelay_.at(CURRENT) );

        // now copy new values into "modified" DelayInfo.
        // modified will be copied to current just before
        // a delay calculation is done.
        modified_.timestamp.at(antid) = mjd;
        modified_.pntAz.at(antid)     = pntaz;
        modified_.pntEl.at(antid)     = pntel;
        modified_.phsAz.at(antid)     = phsaz;
        modified_.phsEl.at(antid)     = phsel;
        // reset the pointing state. A call to this method means Az,El
        modified_.pntState.at(antid)  = DelayInfo::AZEL; 
        modified_.source.at(antid)    = AzEl;

        refPtDelay_.at(CURRENT).timestamp.at(antid) = mjd;
        refPtDelay_.at(CURRENT).pntAz.at(antid)     = pntaz;
        refPtDelay_.at(CURRENT).pntEl.at(antid)     = pntel;
        refPtDelay_.at(CURRENT).phsAz.at(antid)     = phsaz;
        refPtDelay_.at(CURRENT).phsEl.at(antid)     = phsel;
        refPtDelay_.at(CURRENT).pntState.at(antid)  = DelayInfo::AZEL; 
        refPtDelay_.at(CURRENT).source.at(antid)    = AzEl;

        lockManager.unlock();
    }

    computeDelays(antennaNo);

    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }

}

void
DelayEngine::setAntennaLOFreq(unsigned short antennaNo,
                              const Frequency& LO1freq)
{
    try {

        freq_ = LO1freq;
        static const double fHigh = 350.0; // GHz
        static const double fLow  = 10.0; // GHz

        double fGHz = LO1freq.gigahertz();
        if ( fGHz > fHigh || fGHz < fLow ) {
            ostringstream errOs;
            errOs << "DelayEngine::setAntennaLOFreq - "
                << " frequency(" 
                << setprecision(1) 
                << fGHz
                << ") is out of range ["
                  << fLow << "-" << fHigh << "]"; 
            throw CARMA_EXCEPTION(IllegalArgumentException, errOs.str().c_str());
        }
    computeDelays(antennaNo);
    freqSet_ = true;
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::setAllAntennaLOFreqs( const Frequency & LO1freq ) {

    ostringstream os;
    os << "Calling DelayEngine::setAntennaLOFreq - "
       << " LO1Freq = " << LO1freq << " (and Computing Delays) for antennas ";

    const unsigned short maxAnts = DelayInfo::numAntennas( );
    for ( unsigned short antNo = 1; antNo <= maxAnts; ++antNo ) {
        setAntennaLOFreq( antNo, LO1freq );
        os << antNo;
        if ( antNo < maxAnts ) 
            os << ", ";
        else 
            os << ".";
    }
    //programLogInfoIfPossible( os.str() );
}

void 
DelayEngine::setWeather(double airTemp,
                        double atmPressure,
                        double relHumid)
    
{
    // assign weather data both to modified and to reference point
    // delay info objects.  Need it in reference point so that
    // we can compute the tropospheric delay at the reference point.

    // convert the temperature from C to K.
    const Temperature t(airTemp,"C");
    modified_.airTemp     = atm_.safeAirTemperature( t.kelvin() );
    modified_.atmPressure = atm_.safeAtmPressure( atmPressure );
    modified_.relHumid    = atm_.safeRelativeHumidity( relHumid );
    
    refPtDelay_.at(CURRENT).airTemp     = modified_.airTemp;
    refPtDelay_.at(CURRENT).atmPressure = modified_.atmPressure;
    refPtDelay_.at(CURRENT).relHumid    = modified_.relHumid;

}

void 
DelayEngine::setWeather( const Temperature & airTemp,
                         const Pressure & atmPressure,
                         double relHumid ) 
{

          setWeather(airTemp.celsius(),atmPressure.millibar(),relHumid);
}

void 
DelayEngine::setDelayOffset(unsigned short antennaNo,
                            double delay)
{
    try {
    // zero means do all antennas.
    if ( antennaNo == 0 ) {
            const unsigned short maxAnts = DelayInfo::numAntennas( );
        for(unsigned short i = 0; i < maxAnts; i++ ) {
        delayOffset_.at(i) = delay;
        }
        computeDelays();
    } else {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayOffset_.at(antid) = delay;
        computeDelays(antennaNo);
    }
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void 
DelayEngine::setPadDelay(unsigned short antennaNo,
                         double delay)
{
    try {
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    padDelay_.at(antid) = delay;
    computeDelays(antennaNo);
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void 
DelayEngine::setAntennaDelays(unsigned short antennaNo,
            double antDelay, double opticsDelayMM, double opticsDelayCM, 
            double loCableDelayMM, double loCableDelayCM)
{
    try {
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    antDelay_.at(antid) = antDelay;
    opticsDelayMM_.at(antid)  = opticsDelayMM;
    opticsDelayCM_.at(antid)  = opticsDelayCM;
    loCableDelayMM_.at(antid) = loCableDelayMM;
    loCableDelayCM_.at(antid) = loCableDelayCM;
    computeDelays(antennaNo);
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void 
DelayEngine::setAdjustableDelay(unsigned short antennaNo,
                                double delay)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        adjustableDelay_.at(antid) = delay;
        computeDelays(antennaNo);
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void 
DelayEngine::setRxDelayPol1(unsigned short antennaNo, double delay)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        rxDelayPol1_.at(antid) = delay;
        computeDelays(antennaNo);
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void 
DelayEngine::setRxDelayPol2(unsigned short antennaNo, double delay)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        rxDelayPol2_.at(antid) = delay;
        computeDelays(antennaNo);
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}


void
DelayEngine::useAdjustableDelay(unsigned short antennaNo,
                                bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useAdjustable.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::useGeometricDelay(unsigned short antennaNo,
                               bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useGeometric.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::useHeightDelay(unsigned short antennaNo,
                            bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useHeight.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::useIonosphericDelay(unsigned short antennaNo,
                                 bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useIonospheric.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::useTroposphericDelay(unsigned short antennaNo,
                                  bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useTropospheric.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

void
DelayEngine::useThermalDelay(unsigned short antennaNo,
                             bool useit)
{
    try {
        // antenna indices run from zero, antenna numbers from 1
        int antid = antennaNo - 1;
        delayStatus_->useThermal.at(antid) = useit;
        computeDelays( antennaNo );
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}

// Do not put this into computeDelays(bool)!! 
// because the correlator calls are less efficient if done one by one.
void
DelayEngine::computeDelays(unsigned short antennaNo) 
{
  try {

    CARMA_CPTRACE(verboseTraceLevel,"Entering computeDelays(ushort)");
    unsigned short antid = antennaNo - 1;

    {
    // mutex protect the copies and computation
    const EphemLock lock( gEphemGuard );

    current_.copy(antid, modified_);
    antepenultimate_.totalDelay.at(antid) 
                 = computeTotalDelay(antennaNo, antepenultimate_,
                        refPtDelay_.at(ANTEPENULTIMATE) );
    penultimate_.totalDelay.at(antid) 
                 = computeTotalDelay(antennaNo, penultimate_,
                        refPtDelay_.at(PENULTIMATE) );
    current_.totalDelay.at(antid) 
                 = computeTotalDelay(antennaNo, current_,
                        refPtDelay_.at(CURRENT) );

    }
    CARMA_CPTRACE(verboseTraceLevel,"Exiting computeDelays(ushort)");

  } catch (const BaseException& ex) {
        ostringstream errStr;
        errStr << "Delay Engine caught BASE exception in "
               << "computeDelays(unsigned short antennaNo): "
               << ex.what();
        programLogErrorIfPossible(errStr.str());
  } catch (const ::std::exception& stdex) {
        ostringstream errStr;
        errStr << "Delay Engine caught STD exception in "
               << "computeDelays(unsigned short antennaNo): "
               << stdex.what();
        programLogErrorIfPossible(errStr.str());
  } catch ( ... ) {
        programLogErrorIfPossible("DelayEngine computeDelays(ushort) caught UNKNOWN exception...returning");
    }

}


DelayEngineSubsystem &
DelayEngine::ensureAllocated( auto_ptr< DelayEngineSubsystem > & ap ) 
{
    DelayEngineSubsystem * result = ap.get();

    if ( result == 0 ) {
        ap = auto_ptr< DelayEngineSubsystem >( new DelayEngineSubsystem );
        
        result = ap.get();
        
        if ( result == 0 )
            throw CARMA_EXCEPTION( ErrorException,
                                   "DelayEngineSubsystem allocation bad" );
    }

    return *result;
}


DelayFrameVec
DelayEngine::computeDelays( void )
{

    try {
    ScopedLogNdc ndc("DelayEngine::computeDelays(void) -- ");
    // mutex protect this method.
    const EphemLock lock( gEphemGuard );

    int antennaNo ;

    // always calculate the current delays from the modified DelayInfo.
    current_ = modified_;
    current_.name = "current"; // name got overwritten above.

    long curSize = current_.totalDelay.size();
    long penSize = penultimate_.totalDelay.size();
    long antSize = antepenultimate_.totalDelay.size();


    // NEW as of 06/06/06 - ALWAYS calculate the full triplet.
    // Do the calculations in triplet time order.

    // For older members of the time-tagged triplet, we will only send 
    // as many delay and frequency sets to the lobe rotator as 
    // there are initialized antennas. 
    // First calculate them, oldest first.
    ostringstream yadda;
    for (int i=0; i < antSize; i++) {
        antennaNo = i+1;
        yadda 
        << " ap["<<antennaNo<<"] before="
            << antepenultimate_.source.at(i) << "/" 
        << setprecision(8) << antepenultimate_.totalDelay.at(i) ;
        antepenultimate_.totalDelay.at(i) 
                         = computeTotalDelay(antennaNo, antepenultimate_,
                                        refPtDelay_.at(ANTEPENULTIMATE) );
        yadda
        << " ap["<<antennaNo<<"] after="
            << antepenultimate_.source.at(i) << "/" 
            << setprecision(8) << antepenultimate_.totalDelay.at(i) ;
    }

    for (int i=0; i < penSize; i++) {
        antennaNo = i+1;
        yadda
        << " p["<<antennaNo<<"] before="
            << penultimate_.source.at(i) << "/" 
            << setprecision(8) << penultimate_.totalDelay.at(i) ;
        penultimate_.totalDelay.at(i) 
                         = computeTotalDelay(antennaNo, penultimate_,
                                        refPtDelay_.at(PENULTIMATE) );
        yadda
        << " p["<<antennaNo<<"] after="
            << penultimate_.source.at(i) << "/" 
            << setprecision(8) << penultimate_.totalDelay.at(i) ;
    }

    for (int i=0; i < curSize; i++) {
        // Yes DO set data for uninitialized antennas
        // otherwise they'll never get initialized!
        antennaNo = i+1;
        current_.totalDelay.at(i) 
                         = computeTotalDelay(antennaNo, current_,
                                        refPtDelay_.at(CURRENT) );
        CARMA_CPTRACE(moderateTraceLevel,
            " total delay["<<antennaNo<<"] = "
                << current_.source.at(i) << "/" 
            << setprecision(8)
            <<current_.totalDelay[i]);
    }
    CARMA_CPTRACE( verboseTraceLevel, yadda.str() );

    ostringstream report;
    report << "DE RETURNING: ";
    for (int i=0; i < curSize; i++ ) {
        antennaNo = i+1;
        report << "Ant#" << antennaNo << ":("
           << setprecision(9)
           << antepenultimate_.totalDelay[i]
           << ","
           << antepenultimate_.timestamp[i]
           << ","
           << antepenultimate_.source[i]
           << ")/("
           << penultimate_.totalDelay[i]
           << ","
           << penultimate_.timestamp[i]
           << ","
           << penultimate_.source[i]
           << ")/("
           << current_.totalDelay[i]
           << ","
           << current_.timestamp[i]
           << ","
           << current_.source[i]
           << ") ";
    }
    CARMA_CPTRACE( lowTraceLevel, report.str() );


    //
    // Uncomment if we want to test against the BIMA delays.
    // (development only).
    //   testAgainstBima();
    //

    //CARMA_CPTRACE(verboseTraceLevel, debugPrint(current) );

    // Wait until we're done the real work to publish 
    // the monitor points. 

    DelayFrameVec vd;
    
    DelayEngineSubsystem & des2 = ensureAllocated( des2_ );
    DelayEngineSubsystem & des1 = ensureAllocated( des1_ );
    DelayEngineSubsystem & des0 = ensureAllocated( des0_ );
    
    fillInMonitorPoints(antepenultimate_, des2);
    fillInMonitorPoints(penultimate_, des1);
    fillInMonitorPoints(current_, des0);
    
    vd.push_back( &des2 );
    vd.push_back( &des1 );
    vd.push_back( &des0 );
    
    CARMA_CPTRACE(verboseTraceLevel,"Exiting computeDelays(void)");
    return vd;

    } catch (const BaseException& ex) {
        ostringstream errStr;
        errStr << "Delay Engine caught BASE exception in "
               << "computeDelays(void): "
               << ex.what();
        programLogErrorIfPossible(errStr.str());
        throw;
    } catch (const ::std::exception& stdex) {
        ostringstream errStr;
        errStr << "Delay Engine caught STD exception in "
               << "computeDelays(void): " << stdex.what();
        programLogErrorIfPossible(errStr.str());
        throw;
    } catch ( ... ) {
        programLogErrorIfPossible("DelayEngine computeDelays(void) caught UNKNOWN exception...returning");
        throw;
    }
                           
}


bool
DelayEngine::selfTest(bool verbose) 
{
    try {
        // did the selftest succeed
        bool success = false;
        // did one of the 7 antenna tests succeed
        bool localsuccess;

        HourAngle testHa(0.0,"radians"); 
        double tdec = M_PIl/4.0;
        Angle testDec(tdec,"radians");

        // sin(dec)/C and cos(dec)/C with C in meters/nanoseconds
        double cosDec = cos(tdec) / (Physical::C * 1.0E-9 );
        double sinDec = sin(tdec) / (Physical::C * 1.0E-9 );
        long double normalizedDiff;
        numeric_limits<double> doubleLimits;
        double epsilon = 10.0 * doubleLimits.epsilon();
        if( verbose ) {
        CARMA_CPTRACE(verboseTraceLevel,
            " cosDec = "  << cosDec
            << " sinDec = "  << sinDec
            << " epsilon = " << epsilon);
        }
        
        DelayInfo testInfo;
        testInfo.name = "TEST DELAYINFO OBJECT";

        AntennaCoordinates ref;
        // use the ovro array reference point.
        // west long = 118.283391388889 = 118d17m00s.209,   el=1.2083 km 
        double lat = 37.23337666667l*M_PIl/180.0;   // radians
        double lon = -118.283391388889l*M_PIl/180.0;  // radians
        double alt = 1208.3;  // Meters

        Angle latRadians (lat, "radians");
        Angle lonRadians (lon, "radians");
        Length altMeters (alt, "meters");
        ref.setLla (latRadians, lonRadians, altMeters);

        // set up a cross array with 1 meter offsets.
        
        testInfo.longitude.at(0) = lon;
        testInfo.latitude.at(0)  = lat;
        testInfo.altitude.at(0)  = alt;
        testInfo.X.at(0)  = 0.0;
        testInfo.Y.at(0)  = 0.0;
        testInfo.Z.at(0)  = 0.0;

        testInfo.longitude.at(1) = lon;
        testInfo.latitude.at(1)  = lat;
        testInfo.altitude.at(1)  = alt;
        testInfo.X.at(1)  = 1.0;
        testInfo.Y.at(1)  = 0.0;
        testInfo.Z.at(1)  = 0.0;

        testInfo.longitude.at(2) = lon;
        testInfo.latitude.at(2)  = lat;
        testInfo.altitude.at(2)  = alt;
        testInfo.X.at(2)  = 0.0;
        testInfo.Y.at(2)  = 1.0;
        testInfo.Z.at(2)  = 0.0;

        testInfo.longitude.at(3) = lon;
        testInfo.latitude.at(3)  = lat;
        testInfo.altitude.at(3)  = alt;
        testInfo.X.at(3)  = -1.0;
        testInfo.Y.at(3)  = 0.0;
        testInfo.Z.at(3)  = 0.0;
            
        testInfo.longitude.at(4) = lon;
        testInfo.latitude.at(4)  = lat;
        testInfo.altitude.at(4)  = alt;
        testInfo.X.at(4)  = 0.0;
        testInfo.Y.at(4)  = -1.0;
        testInfo.Z.at(4)  = 0.0;

        testInfo.longitude.at(5) = lon;
        testInfo.latitude.at(5)  = lat;
        testInfo.altitude.at(5)  = alt;
        testInfo.X.at(5)  = 0.0;
        testInfo.Y.at(5)  = 0.0;
        testInfo.Z.at(5)  = 1.0;

        testInfo.longitude.at(6) = lon;
        testInfo.latitude.at(6)  = lat;
        testInfo.altitude.at(6)  = alt;
        testInfo.X.at(6)  = 0.0;
        testInfo.Y.at(6)  = 0.0;
        testInfo.Z.at(6)  = -1.0;

        Delay geoDelay;
        ostringstream os;
        int numants = 7;
        for(int antid = 0; antid < numants; antid++) {
           /* useGeometricDelay(antid+1,true);
            useAdjustableDelay(antid+1,false);
            useTroposphericDelay(antid+1,false)
            useIonosphericDelay(antid+1,false)
            useThermalDelay(antid+1,false)
            setDelayOffset(antid+1,0.0);
            setAntennaLOFreq(antid+1,115.27124E9,1,1);
            setAntennaCoordinates(antid+1,
                    testInfo.X.at(antid),
                    testInfo.Y.at(antid),
                    testInfo.Z.at(antid),
                    TOPO_XYZ, 0.0);
                    */

            geoDelay = ref.getGeometricDelay(
                    testHa,
                    testDec,
                    testInfo.X.at(antid),
                    testInfo.Y.at(antid),
                    testInfo.Z.at(antid),
                    0.0,0.0,0.0, false); // nanoseconds
            testInfo.geometricDelay.at(antid) = geoDelay.nanoSeconds();
        }
        
    /**
     * Here are the correct answers (for hour angle=0).
     *
     * Antenna    Delay
     *   0         0.0
     *   1       -cos(Dec)/C   (C in meters/nanosec)
     *   2         0.0
     *   3       +cos(Dec)/C   (C in meters/nanosec)
     *   4         0.0
     *   5       -sin(Dec)/C   (C in meters/nanosec)
     *   6       +sin(Dec)/C   (C in meters/nanosec)
     */

        // The general comparison method for success or
        normalizedDiff = fabsl(testInfo.geometricDelay.at(0));
        success = (normalizedDiff < epsilon );
        string s;

        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: FIRST TEST IS " 
            << normalizedDiff  << "  "
            << success);
        }

        normalizedDiff = fabsl( 
                (testInfo.geometricDelay.at(1) + cosDec)/cosDec
                );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: SECOND TEST IS " 
            << normalizedDiff  << "  "
            << localsuccess);
        }

        normalizedDiff = fabsl( testInfo.geometricDelay.at(2) );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
                "DELAY ENGINE SELF_TEST: THIRD TEST IS "
                << normalizedDiff  << "  "
                << localsuccess);
        }

        normalizedDiff = fabsl( 
                (testInfo.geometricDelay.at(3) - cosDec)/cosDec
                );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: FOURTH TEST IS " 
            << normalizedDiff  << "  "
            << localsuccess);
        };

        normalizedDiff = fabsl( testInfo.geometricDelay.at(4) );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: FIFTH TEST IS " 
            << normalizedDiff  << "  "
            << localsuccess);
        }

        normalizedDiff = fabsl( 
                (testInfo.geometricDelay.at(5) + sinDec)/sinDec
                );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: SIXTH TEST IS " 
            << normalizedDiff  << "  "
            << localsuccess);
        };

        normalizedDiff = fabsl( 
                (testInfo.geometricDelay.at(6) - sinDec)/sinDec
                );
        localsuccess = (normalizedDiff < epsilon );
        success = success && localsuccess;
        if (verbose) {
        CARMA_CPTRACE(verboseTraceLevel,
            "DELAY ENGINE SELF_TEST: SEVENTH TEST IS " 
            << normalizedDiff  << "  "
            << localsuccess);
        }

        return success;
    } catch (...) {
            programLogErrorIfPossible( "Trouble on selfTest()" );
            return false;
    }

}


//=======================================
// Private methods
//=======================================

void DelayEngine::initialize() 
{
    current_.name         = "current";
    penultimate_.name     = "penultimate";
    antepenultimate_.name = "antepenultimate";
    modified_.name        = "modified";
    bima_.name            = "bima"; // this object used for debugging

    DelayInfo refpt0, refpt1, refpt2;
    refpt0.name = "array ref point antepenultimate_";
    refpt1.name = "array ref point penultimate_";
    refpt2.name = "array ref point current";

    // do an initial time alignment so we are sane
    // even before sac comes up.
    const double now = Time::MJD();
    const double soon  = now + (20.0/60.0)*(1.0 / AstroTime::MINUTES_PER_DAY);
    const double later = now + (40.0/60.0)*(1.0 / AstroTime::MINUTES_PER_DAY);
    unsigned short maxAnts = DelayInfo::numAntennas();
    for (unsigned short j = 0; j < maxAnts; j++ ) {
        antepenultimate_.timestamp.at(j) = now;
        penultimate_.timestamp.at(j)     = soon;
        current_.timestamp.at(j)         = later;
        refpt0.timestamp.at(j)          = now;
        refpt1.timestamp.at(j)          = soon;
        refpt2.timestamp.at(j)          = later;
        modified_.timestamp.at(j)        = later;
    }
    // to keep same order as DelayFrameVec
    // xx[0] = oldest = antepenultimate_
    // xx[1] = middle = penultimate_
    // xx[2] = current
    refPtDelay_.push_back( refpt0 );
    refPtDelay_.push_back( refpt1 );
    refPtDelay_.push_back( refpt2 );

    dtriplet_.push_back( antepenultimate_ );
    dtriplet_.push_back( penultimate_ );
    dtriplet_.push_back( current_ );
    
    totalFixedDelay_ = vector<double>(maxAnts);
    delayOffset_     = vector<double>(maxAnts);
    padDelay_        = vector<double>(maxAnts);
    antDelay_        = vector<double>(maxAnts);
    opticsDelayMM_   = vector<double>(maxAnts);
    opticsDelayCM_   = vector<double>(maxAnts);
    loCableDelayMM_   = vector<double>(maxAnts);
    loCableDelayCM_   = vector<double>(maxAnts);
    adjustableDelay_ = vector<double>(maxAnts);
    rxDelayPol1_     = vector<double>(maxAnts);
    rxDelayPol2_     = vector<double>(maxAnts);
    delayStatus_     = new DelayStatus(maxAnts);
    

    // Copy position over for calculating the delays
    // at the reference point. Assign to all members so the
    // calculation is the same regardless of the antenna index.
    unsigned short rpsize = refPtDelay_.size();
    for ( unsigned short i = 0; i < rpsize; i++)
    {
        refPtDelay_[i].longitude.assign(
            refPtDelay_[i].longitude.size(),
            arrayRefPt_.longitude().radians()
            );
        refPtDelay_[i].latitude.assign(
            refPtDelay_[i].latitude.size(),
            arrayRefPt_.latitude().radians()
            );
        refPtDelay_[i].altitude.assign(
            refPtDelay_[i].altitude.size(),
            arrayRefPt_.altitude().meters()
            );
    }
    arrayRefPtSet_ = true;
}


double 
DelayEngine::computeGeometricDelay( unsigned short antennaNo, 
                                    DelayInfo& delayInfo )
{
    ScopedLogNdc ndc("DelayEngine::computeGeometricDelay(ant,delayInfo) ");
    //conversion factor: 1/C in nanosec/meter
    static const double NSEC_PER_METER = 3.335641734;  
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    // These terms are calculated differently depending on pntState.
    double HA;          // hour angle, radians

    // the axisDelay in nanoseconds
    double axisDelay = computeAxisDelay(antennaNo, delayInfo);

    // depending pointing state, we may need to do a
    // coordinate conversion.
    // See TMS appendix 4.1 for HA,Dec <--> Az,El conversion equations.
    
    Delay geoDelay;  // using AntennaCoordinate class for comparison
    ostringstream os;
    // UVW coordinates.  Note these are in the equinox of date. The Control
    // subsystem MUST precess them to J2000.
    Vector<double> uvw;
    switch ( delayInfo.pntState.at(antid) ) {
        case DelayInfo::RADEC:
        default:
            // we need the hour angle 
            HA = computePhaseCenterHourAngle(antennaNo, delayInfo);
            // u,v,w in meters
            uvw = AntennaCoordinates::haDecAndXyzToUvw(
                    HourAngle(HA,"radians"),
                    Angle(delayInfo.phsDec.at(antid),"radians"),
                    delayInfo.X.at(antid),
                    delayInfo.Y.at(antid),
                    delayInfo.Z.at(antid));
            delayInfo.U.at(antid) = uvw[0];
            delayInfo.V.at(antid) = uvw[1];
            delayInfo.W.at(antid) = uvw[2];

        /**  uvwref is 0,0,0 as expected */
        /* TEST CODE
          {
        astroTime_.setSite(arrayRefPt_.longitude(),
            arrayRefPt_.latitude(),
            arrayRefPt_.altitude()
            );
            double refHA = astroTime_.hourAngle( delayInfo.timestamp.at(antid),
                                    delayInfo.phsRa.at(antid)
                                  );
        Vector<double> uvwref = AntennaCoordinates::haDecAndXyzToUvw(
                    HourAngle(refHA,"radians"),
                    Angle(delayInfo.phsDec.at(antid),"radians")        data.adjustableDelay().setValue( adjustableDelay_.at(i) );
,
                    0,0,0);
        Vector<double> reluvw = uvw - uvwref;
            delayInfo.U.at(antid) = reluvw[0];
            delayInfo.V.at(antid) = reluvw[1];
            delayInfo.W.at(antid) = reluvw[2];
        //cout << "### SUBTRACTED REFUVW ###"<<endl;
        //cout << uvwref<<endl; 
        //cout << "### ###"<<endl;
        }
        */

            // output delay is in meters
            geoDelay = arrayRefPt_.getGeometricDelay(
                    HourAngle(HA,"radians"),
                    Angle(delayInfo.phsDec.at(antid),"radians"),
                    delayInfo.X.at(antid),
                    delayInfo.Y.at(antid),
                    delayInfo.Z.at(antid),
                    0.0,0.0,0.0, false); // nanoseconds
            os << " getGeometricDelay: " 
                    << " Antenna " << antennaNo
                    << " RA = " << delayInfo.phsRa.at(antid) 
                    << " DEC = " << delayInfo.phsDec.at(antid) 
                    << " HA = " << HA 
            << " MJD = " << delayInfo.timestamp.at(antid) 
            << " LST = " << HA - delayInfo.phsRa.at(antid) 
                    << " X(m) = " << delayInfo.X.at(antid)
                    << " Y(m) = " << delayInfo.Y.at(antid)
                    << " Z(m) = " << delayInfo.Z.at(antid) 
                    << " U(ns) = " << delayInfo.U.at(antid)*NSEC_PER_METER 
                    << " V(ns)= " << delayInfo.V.at(antid)*NSEC_PER_METER 
                    << " W(ns)= " << delayInfo.W.at(antid)*NSEC_PER_METER ;
            break;
        case DelayInfo::AZEL:
            // Note we use the PHASE CENTER Az,El, not the pointing
            // center!
            // output delay is in meters

            geoDelay = arrayRefPt_.getGeometricDelay(
                    Angle(delayInfo.latitude.at(antid),"radians"),
                    Length(delayInfo.altitude.at(antid),"meters"),
                    Angle(delayInfo.phsAz.at(antid),"radians"),
                    Angle(delayInfo.phsEl.at(antid),"radians"),
                    delayInfo.X.at(antid),
                    delayInfo.Y.at(antid),
                    delayInfo.Z.at(antid)
                    ); 

        /* this delay zero as expected
        Delay refDelay = arrayRefPt_.getGeometricDelay(
                    Angle(delayInfo.latitude.at(antid),"radians"),
                    Length(delayInfo.altitude.at(antid),"meters"),
                    Angle(delayInfo.phsAz.at(antid),"radians"),
                    Angle(delayInfo.phsEl.at(antid),"radians"),
                    refPtDelay_.at(CURRENT).X.at(antid),
                    refPtDelay_.at(CURRENT).Y.at(antid),
                    refPtDelay_.at(CURRENT).Z.at(antid)
                    ); 
        */

        ostringstream debugOS;
        debugOS 
          << "called arrayRefPt_.getGeometricDelay( "
          << setiosflags(ios::fixed) << setprecision(4)
          << "LAT="<<delayInfo.latitude.at(antid) << " rad "
              << "ALT="<<delayInfo.altitude.at(antid) << " m "
              << "AZ="<<delayInfo.phsAz.at(antid) << " rad "
              << "El="<<delayInfo.phsEl.at(antid) << " rad "
              << "X="<<delayInfo.X.at(antid) << " m "
              << "Y="<<delayInfo.Y.at(antid) << " m "
              << "Z="<<delayInfo.Z.at(antid) << " m ) : "
              << "DELAY ="<< geoDelay.meters() 
          ;

            delayInfo.W.at(antid) = -geoDelay.meters();

        CARMA_CPTRACE(lowTraceLevel, debugOS.str() );

//        In the uvw system w is parallel to s, u is
//        perpendicular to the plane defined by w and pole P, and v is
//      perpendicular to the plane defined by u and w.
//      Don't bother computing U and V for AZEL coords.
            delayInfo.U.at(antid) = 0.0; 
            delayInfo.V.at(antid) = 0.0; 
            os << " getGeometricDelay[AZEL]: " 
                    << " Antenna " << antennaNo
                << setiosflags(ios::fixed) << setprecision(4)
                    << " LAT = "   << delayInfo.latitude.at(antid) 
                    << " AZ = "    << delayInfo.phsAz.at(antid)
                    << " EL = "    << delayInfo.phsEl.at(antid)
                    << " X = "     << delayInfo.X.at(antid)
                    << " Y = "     << delayInfo.Y.at(antid)
                    << " Z = "     << delayInfo.Z.at(antid)
                    << " DELAY ="  << geoDelay.meters()
            ; 

            break;
    }


    os << " Delay (ns) = " << geoDelay.nanoSeconds();
    // stem the tide of log messages, restrict to currently operating antennas
    if (antennaNo < 16) CARMA_CPTRACE(verboseTraceLevel, os.str());

    // convert to nanoseconds  and
    // add the axis misaligment delay, which is already in nanoseconds
    double delay = geoDelay.nanoSeconds() + axisDelay; 


    // copy to DelayInfo object.
    delayInfo.geometricDelay.at(antid) = delay;
    return delay;
}

// NOTE: computeTropospheric should be called first, so that
// refractivity gets set!!
double 
DelayEngine::computeHeightDelay( unsigned short antennaNo, 
                                 DelayInfo & delayInfo)
{
    unsigned short antid = antennaNo - 1;
    double cosLat = cos( delayInfo.latitude.at(antid) );
    double sinLat = sin( delayInfo.latitude.at(antid) );
    double sinEl = sin( atm_.safeElevation( delayInfo.pntEl.at(antid) ) );
    double h = delayInfo.X.at(antid)*cosLat + delayInfo.Z.at(antid)*sinLat;
    double n  = 1.0E-6 * delayInfo.refractivity.at(antid);
    double hd = n * h / sinEl;
    // convert meters to nanoseconds
    hd *= ( units_.giga() / Physical::C );

    /****** TEST CODE ****** 
    bima_.copy( antid, delayInfo );
    computeBimaDelay( antennaNo, bima );
    CARMA_CPTRACE(lowTraceLevel,
        "computeHeightDelay ["
        << delayInfo.name 
        <<"] - sin(el) for ant " 
        << antennaNo << " is " << sinEl
        << ". pntEl = " << delayInfo.pntEl.at(antid)
        << ". phsEl = " << delayInfo.phsEl.at(antid) 
        << ". n = " << n
        << ". hd = " << hd
        << ". BIMA hd = " << bima_.heightDelay.at(antid)
        );
    ****** TEST CODE ******/

    return hd;
}
 
double 
DelayEngine::computeIonosphericDelay( unsigned short antennaNo, 
                                      DelayInfo& delayInfo)
{
    // As decided in design doc, do not compute this delay 
    // for first light. Return zero here, until good model for
    // total atmospheric electron column density is available.
    
    // WHEN FULLY IMPLEMENTED THIS SHOULD RETURN A NUMBER
    // LESS THAN OR EQUAL TO ZERO. See design document.
    
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    double delay = 0.0;
    // copy to DelayInfo object.
    delayInfo.ionosphericDelay.at(antid) = delay;
    return delay;
}

double 
DelayEngine::computeThermalDelay( unsigned short antennaNo, 
                                  DelayInfo& delayInfo)
{
    // As decided in design doc, do not compute this delay 
    // for first light. Return zero here, until antenna structure 
    // measurements (temperature, reference pt. height above
    // ground) are available.
    
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;

    double delay = 0.0;
    // copy to DelayInfo object.
    delayInfo.thermalDelay.at(antid) = delay;
    return delay;
}

double 
DelayEngine::computeTroposphericDelay( unsigned short antennaNo, DelayInfo& delayInfo)
{
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;

    // elevation in radians, this has already been computed
    // by ephemeris_.getAzEl()
    double elevation = delayInfo.pntEl.at(antid);

    // add the refraction correction
    elevation += atm_.computeRefractionCorrection(
                                delayInfo.airTemp,
                                delayInfo.atmPressure,
                                delayInfo.relHumid,
                                elevation,
                                freq_.hertz(),
                                delayInfo.altitude.at(antid)
            );

    // pathlength in meters, including negative correction
    // for antenna height above reference plane. This method has been
    // tested against TMS (for antenna height=0) and can reproduce TMS figure
    // 13.5. The method computePathlength() guards against low elevations.

    double pathlength = atm_.computePathlength(
                                delayInfo.airTemp,
                                delayInfo.atmPressure,
                                delayInfo.relHumid,
                                elevation,
                                freq_.hertz(),
                                delayInfo.altitude.at(antid)
                               );

    delayInfo.pathlength.at(antid) = pathlength;

    /*
    ostringstream os;
    os << "Delay Engine computed pathlength with " 
        << " Antenna = " << antennaNo
        << " Pressure (mbar) = " << delayInfo.atmPressure
        << " Airtemp (K) =  " << delayInfo.airTemp
        << " EL (rad) = " << elevation
        << " freq (Hz) = " << delayInfo.frequency.at(antid)
        << " alt (m)   = " << delayInfo.altitude.at(antid)
        << " path (m) = " << pathlength;
    programLogInfoIfPossible( os.str() );
    */

    delayInfo.refractivity.at(antid) = atm_.computeZenithRefractivity( 
                                       delayInfo.airTemp,
                                       delayInfo.atmPressure,
                                       delayInfo.relHumid,
                                       freq_.hertz()
                                             );

    //Make sure all units are correct.
    //delay in nanoseconds = 1E9 * delay in seconds
    double delay = units_.giga() * pathlength / Physical::C;

    // Do not assign to the delayInfo object here, since
    // De will assign the difference antenna-refpt outside this method.
    return delay;
}

double 
DelayEngine::computeAxisDelay (unsigned short antennaNo, DelayInfo& delayInfo )
{
    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    // the delay in meters
    double delay = 0.0;
    // cosine of elevation
    double cosEl;

    // elevation in radians, this has already been computed
    // by ephemeris_.getAzEl()
    double elevation = delayInfo.pntEl.at(antid);

    // IS AXIS MISALIGNMENT COSINE TERM ALWAYS POSITIVE???
    // That is, if we are observing "over the top", 
    // should cos(elevation) be negative?  
    
    // Cos(PI/2) = 0, so avoid a divide by zero
    if ( (elevation > (M_PI_2 - 0.001)) ) {
        cosEl = units_.milli(); // set to some small number
    } else {
            cosEl = cos(elevation);
    }

    try {
    // sign convention is that the axis delay is added to the
    // w coordinate, so multiply by minus 1 here because
    // geometric delay is -w.
        delay = -delayInfo.axisMis.at(antid) * cosEl;

        // scale by speed o' light to get seconds
        delay /= Physical::C;

        // scale to nanoseconds
        //delay in nanoseconds = 1E9 * delay in seconds
        delay *= units_.giga();

        // copy to DelayInfo object.
        delayInfo.axisDelay.at(antid) = delay;
    } catch ( const out_of_range& ex ) {
        ostringstream os;
        os << "Antenna Number out of Range. Wanted [1-23], got: " << antennaNo;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
    return delay;
}

double 
DelayEngine::computePointingCenterHourAngle( unsigned short antennaNo,
                                             const DelayInfo& delayInfo) 
{
        setLocation( antennaNo, delayInfo );
        int antid = antennaNo - 1;
        // use the pointing center RA member.
        return astroTime_.hourAngle( delayInfo.timestamp.at(antid),
                                    delayInfo.pntRa.at(antid)
                                  );
}


double 
DelayEngine::computePhaseCenterHourAngle( unsigned short antennaNo,
                                          const DelayInfo& delayInfo) 
{
        setLocation( antennaNo, delayInfo );
        int antid = antennaNo - 1;
        // use the phase center RA member.
        return astroTime_.hourAngle( delayInfo.timestamp.at(antid),
                                    delayInfo.phsRa.at(antid)
                                  );
}

void
DelayEngine::setLocation( unsigned short antennaNo, const DelayInfo& delayInfo )
{
        int antid = antennaNo - 1;
        astroTime_.setSite(
                delayInfo.longitude.at(antid),
                delayInfo.latitude.at(antid),
                delayInfo.altitude.at(antid)
                );
}

// Fixed delay is delayOffset, padDelay, antDelay, and adjustableDelay.
double 
DelayEngine::computeTotalFixedDelay(unsigned short antennaNo) 
{
    ScopedLogNdc ndc("DelayEngine::computeTotalFixedDelay(ant,delayInfo) ");
    CARMA_CPTRACE(moderateTraceLevel, "Entering antNo = "<<antennaNo);

    // antenna indices run from zero, antenna numbers from 1
    int antidx = antennaNo - 1;

    // Note there is no delayStatus_->useDelayOffet member. If
    // the delay offset is to be "unused" it should be set to zero.
    // Ditto pad and antenna delays.
    double totalFixedDelay = 0.0;
    totalFixedDelay += delayOffset_.at(antidx); 
    totalFixedDelay += padDelay_.at(antidx); 
    totalFixedDelay += antDelay_.at(antidx);
    
    if ( delayStatus_->useAdjustable.at(antidx) ) {
        totalFixedDelay += adjustableDelay_.at(antidx);
    }
    totalFixedDelay_.at(antidx) = totalFixedDelay;
    return totalFixedDelay;
}    
    
double 
DelayEngine::computeTotalDelay( 
    unsigned short antennaNo, 
    DelayInfo& delayInfo,
    DelayInfo& refPtInfo) 
{

    ScopedLogNdc ndc("DelayEngine::computeTotalFixedDelay(ant,delayInfo,refPtInfo) ");
    CARMA_CPTRACE(moderateTraceLevel, 
        "Entering antNo = "<<antennaNo << " DelayInfo = " << delayInfo.name );

    // antenna indices run from zero, antenna numbers from 1
    int antid = antennaNo - 1;
    // make sure times match. use seconds.
    double checktime = 86400.0*
        fabs(refPtInfo.timestamp.at(antid) - delayInfo.timestamp.at(antid));
    if ( checktime > 0.01 ){
        ostringstream badwolf;
        badwolf 
            << " Refpt and antenna timestamps don't match! Delta="
            << setprecision(5)
            << checktime
            << "  antenna number= " << antennaNo
            << "  refPt DI = " << refPtInfo.name;
        programLogErrorIfPossible( badwolf.str() );
    }
    double totalDelay = 0.0;
    // Start off with fixed part of delay
    totalDelay = computeTotalFixedDelay(antennaNo);

    // compute the delays regardless of whether or not we
    // are adding them to the total.
    
    double delay = computeGeometricDelay(antennaNo,delayInfo);
    if ( delayStatus_->useGeometric.at(antid) ) {
        totalDelay += delay;
    }

    delay = computeIonosphericDelay(antennaNo,delayInfo);
    if ( delayStatus_->useIonospheric.at(antid) ) {
        // Note the ionospheric delay is in fact negative,
        // but computeIonosphericDelay will return the negative number.
        totalDelay += delay;
    }

    // differential refractive delay is tropospheric(ant) - tropospheric(ref).
    // Note the differential delay will be zero in AZEL mode.
    refPtInfo.troposphericDelay.at(antid) = 
              computeTroposphericDelay(antennaNo,refPtInfo);
    delayInfo.troposphericDelay.at(antid) = 
              computeTroposphericDelay(antennaNo,delayInfo)
            - refPtInfo.troposphericDelay.at(antid);
    if ( delayStatus_->useTropospheric.at(antid) ) {
        // Add the difference between the tropospheric delay at 
        // the reference point and at this antenna's location,
        // in the sense (antenna - reference point).
        totalDelay += delayInfo.troposphericDelay.at(antid);
    }

    // Additional height delay. must be called AFTER tropospheric calculation.
    refPtInfo.heightDelay.at(antid) =
        computeHeightDelay( antennaNo, refPtInfo);
    // The height delay has the OPPOSITE sense as other delays
    // so it is (REFPT - ANTENNA) rather than (ANTENNA - REFPT)
    delayInfo.heightDelay.at(antid) = 
       refPtInfo.heightDelay.at( antid )  -
        computeHeightDelay( antennaNo, delayInfo )  ;

    if ( delayStatus_->useHeight.at(antid) ) {
        // Add the difference between the tropospheric delay at 
        // the reference point and at this antenna's location,
        // in the sense (antenna - reference point).
        totalDelay += delayInfo.heightDelay.at(antid);
    }
    
    if (freq_.gigahertz() < 50.0) {
         totalDelay += opticsDelayCM_.at(antid);
    }
    else {
         totalDelay += opticsDelayMM_.at(antid);
    }

    // Set the relevant timestamps.
    
    // when we did this.
    const double now = Time::MJD();
    delayInfo.calculatedAt.at(antid) = now;
    refPtInfo.calculatedAt.at(antid) = now;

    // this value is good for 40 seconds
    const double fact = (40.0/60.0) * (1.0 / AstroTime::MINUTES_PER_DAY);
    delayInfo.validUntil.at(antid) = delayInfo.timestamp.at(antid) + fact;
    refPtInfo.validUntil.at(antid) = refPtInfo.timestamp.at(antid) + fact;

    delayInfo.totalDelayPol1.at(antid) 
        = totalDelay + rxDelayPol1_.at(antid);
    delayInfo.totalDelayPol2.at(antid) 
        = totalDelay + rxDelayPol2_.at(antid);

    return totalDelay;

}


void
DelayEngine::fillInMonitorPoints( const DelayInfo      & delayInfo,
                                  DelayEngineSubsystem & monitorFrame )
{
    /**  
     * NOTE: If you add any new monitor points, you must also 
     * put them in ::copyAntDelayData in SubarrayControlDelays.cc !
     * Otherwise the new MPs will always be invalid in RTD.
     */
    unsigned int i = 0;
    try {
     ScopedLogNdc ndc("DelayEngine::fillInMonitorPoints [ "+delayInfo.name+"]");
     unsigned int size = delayInfo.totalDelay.size();

     for (i=0; i < size; i++) {
        DelayEngineSubsystem::DelayData & data = monitorFrame.delayData(i);

        data.totalFixedDelay().setValue(totalFixedDelay_.at(i));
        
        data.adjustableDelay().setValue( adjustableDelay_.at(i) );
        data.adjustableDelayStatus()
            .setValue( delayStatus_->useAdjustable.at(i) );
        data.delayOffset().setValue( delayOffset_.at(i) );

        data.padDelay().setValue( padDelay_.at(i) );
        data.padDelay().setValidity( MonitorPoint::VALID_GOOD );
        data.antennaDelay().setValue( antDelay_.at(i) );
        data.antennaDelay().setValidity( MonitorPoint::VALID_GOOD );
        data.opticsDelayMM().setValue(opticsDelayMM_.at(i));
        data.opticsDelayMM().setValidity(MonitorPoint::VALID_GOOD);
        data.opticsDelayCM().setValue(opticsDelayCM_.at(i));
        data.opticsDelayCM().setValidity(MonitorPoint::VALID_GOOD);
        data.loCableDelayMM().setValue(loCableDelayMM_.at(i));
        data.loCableDelayMM().setValidity(MonitorPoint::VALID_GOOD);
        data.loCableDelayCM().setValue(loCableDelayCM_.at(i));
        data.loCableDelayCM().setValidity(MonitorPoint::VALID_GOOD);

        // string monitor points can only have one sample
        switch ( delayInfo.pntState.at(i) ) {
            case DelayInfo::RADEC:
            default:
                data.pointState().setValue(RaDec);
                break;
            case DelayInfo::AZEL:
                data.pointState().setValue(AzEl);
                break;
        }

        data.calculatedAt().setValue( delayInfo.calculatedAt.at(i) );
        data.calculatedFor().setValue( delayInfo.timestamp.at(i) );
        data.validUntil().setValue( delayInfo.validUntil.at(i) );
        data.axisDelay().setValue( delayInfo.axisDelay.at(i) );
        data.geometricDelay()
                      .setValue( delayInfo.geometricDelay.at(i) );
        data.geometricDelayStatus()
                      .setValue( delayStatus_->useGeometric.at(i) );
        data.heightDelay()
                      .setValue( delayInfo.heightDelay.at(i) );
        data.heightDelayStatus()
                      .setValue( delayStatus_->useHeight.at(i) );
        data.ionosphericDelay()
                      .setValue( delayInfo.ionosphericDelay.at(i) );
        data.ionosphericDelayStatus()
                      .setValue( delayStatus_->useIonospheric.at(i) );
        data.pathLength().setValue( delayInfo.pathlength.at(i) );
        data.refractivity().setValue( delayInfo.refractivity.at(i) );
        data.rxDelayPol1().setValue( rxDelayPol1_.at(i) );
        data.rxDelayPol2().setValue( rxDelayPol2_.at(i) );
        data.thermalDelay().setValue( delayInfo.thermalDelay.at(i) );
        data.thermalDelayStatus()
                      .setValue( delayStatus_->useThermal.at(i) );
        data.totalDelay().setValue( delayInfo.totalDelay.at(i) );
        data.totalDelayPol1().setValue( delayInfo.totalDelayPol1.at(i) );
        data.totalDelayPol2().setValue( delayInfo.totalDelayPol2.at(i) );
        data.troposphericDelay()
                         .setValue( delayInfo.troposphericDelay.at(i) );
        data.troposphericDelayStatus()
                        .setValue( delayStatus_->useTropospheric.at(i) );
        data.u().setValue( delayInfo.U.at(i) );
        data.v().setValue( delayInfo.V.at(i) );
        data.w().setValue( delayInfo.W.at(i) );
        data.x().setValue( delayInfo.X.at(i) );
        data.y().setValue( delayInfo.Y.at(i) );
        data.z().setValue( delayInfo.Z.at(i) );
     }
    } catch ( const out_of_range& ex ) {
        // This should never happen.
        ostringstream os;
        os << "Out of range error in "
           << "DelayEngine::fillInMonitorPoints at index "
           << i;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }
}


//========================================================================
//   CODE EXCLUSIVELY FOR TESTING BELOW
//========================================================================

string DelayEngine::debugPrint(const DelayInfo & delayInfo) {
    ostringstream os;
    os.setf(ios_base::scientific,ios_base::floatfield);
    os.precision(4);
    os << "\n Data for DelayInfo object: " << delayInfo.name;
    os << "\n ANT   RA     DEC     LST     HA      AZ     EL        MJD "
       << "   PNT DISC     OFFSET      GEO      "
       << "   TROPO        PATH        AXIS        TOTAL\n";
    unsigned short antennaNo;
    unsigned int i;
    try {
        for(i=0;i<delayInfo.totalDelay.size() ;i++) { 
            antennaNo = i+1;
            double elevation = delayInfo.pntEl.at(i);
            elevation *= ( 180.0/M_PI );  // degrees for printout
            double azimuth   = delayInfo.pntAz.at(i);
            azimuth   *= ( 180.0/M_PI );  // degrees for printout
            double RAhours   = delayInfo.pntRa.at(i)*(12.0/M_PI);
            double DECdegrees = delayInfo.pntDec.at(i)*(180.0/M_PI);
            setLocation(antennaNo, delayInfo);
            double LSThours = astroTime_.localSiderealTime(
                                    delayInfo.timestamp.at(i)
                                );
            double HAhours  = astroTime_.hourAngle(
                                          delayInfo.timestamp.at(i),
                                          delayInfo.pntRa.at(i)
                                         ) * 12.0/M_PI;
            os.setf(ios_base::fixed,ios_base::floatfield);
            os.precision(2);
            os  << "  "
                << antennaNo  << "   "
                << RAhours    << "   "
                << DECdegrees << "   "
                << LSThours   << "   "
                << HAhours    << "   "
                << azimuth    << "   "
                << elevation  << "   "
                << delayInfo.timestamp.at(i) << "   ";
            os.precision(4);
            os.setf(ios_base::scientific,ios_base::floatfield);

    //            << delayInfo.latitude.at(i) << "   "
    //            << delayInfo.longitude.at(i) << "   "
            os  << delayInfo.pntState.at(i)          << "   "
                << delayOffset_.at(i)                 << "   "
                << delayInfo.geometricDelay.at(i)    << "   "
                << delayInfo.troposphericDelay.at(i) << "   "
                << delayInfo.pathlength.at(i)        << "   "
                << delayInfo.axisDelay.at(i)         << "   "
                << delayInfo.totalDelay.at(i) ;
        }
    //   programLogInfoIfPossible(
    //            monitorFrame->hierarchyToString(true,true,true);
    //            );
    //            os << monitorFrame->hierarchyToString(true,true,true);
    //
        return os.str();
    } catch ( const out_of_range& ex ) {
        // This should never happen.
        ostringstream os;
        os << "Out of range error in "
           << "DelayEngine::debugPrint at index "
           << i;
        throw CARMA_EXCEPTION(ErrorException, os.str().c_str());
    }

}

void DelayEngine::testAgainstBima() 
{
    int size = current_.totalDelay.size();
    
    double ha;
    unsigned short antennaNo;
    log4cpp::Category& myLogger = Logger::getFilelogger(
            "Test Against BIMA Delays",
            "/tmp/bimadelay.out",
            "testAgainstBima",
            true); 
    // override FileLayout with a simple layout
    //log4cpp::Appender* app        = myLogger.getAppender(name);
    //log4cpp::SimpleLayout* layout = new log4cpp::SimpleLayout();
    //app->setLayout(layout);
    myLogger.info("#   TEST AGAINST BIMA CODE:");
    ostringstream bbb;
    bbb << "#   MJD = " << current_.timestamp[0];
    myLogger.info(bbb.str());
    myLogger.info("#                                                                                  |    CARMA                                                   |    BIMA                                                                      |        DIFFS");
    myLogger.info("#Ant   X      Y         Z       Ra       Dec     HA           El           dEl     | Geo              dPath           Tropo          Total      |   Geo            dPath             TropoOnly       Tropo+H        Height                Total  | TropoDiff_C-B    (C-B)/C    TotalDiff_C-B     (C-B)/C");
    myLogger.info("#   (m)     (m)       (m)      (hr)     (deg)   (hr)          (deg)        (arcsec)| (ps)            (mm)             (ps)             (ps)     |   (ps)            (mm)            (ps)           (ps)           (ps)                   (ps) | (ps)              (%)             (ps)          (%)");

    double rad2hr = (180.0/M_PI)*(24./360.);
    for (int antid = 0; antid < size; antid++) {
       antennaNo = antid+1;
       useGeometricDelay(antennaNo,true);
       useAdjustableDelay(antennaNo,false);
       useTroposphericDelay(antennaNo,true);
       useIonosphericDelay(antennaNo,false);
       useThermalDelay(antennaNo,false);
       ha = computePhaseCenterHourAngle( antennaNo, current_ );
       bima_.copy( antid, current_ );
       bima_.totalDelay.at(antid) = -computeBimaDelay( antennaNo, bima_ );
       // assume computeDelays was previously called.
       current_.totalDelay.at(antid) = 
           computeTotalDelay(antennaNo, current_, refPtDelay_.at(CURRENT));
       ostringstream astream;
       double diff = ( current_.troposphericDelay.at(antid) -
                       bima_.troposphericDelay.at(antid) );
       double diffT = ( current_.totalDelay.at(antid) -
                       bima_.totalDelay.at(antid) );
       double percent = diff / current_.troposphericDelay.at(antid);
       double percentT = diffT / current_.totalDelay.at(antid);
       double dEl = (current_.phsEl.at(antid)-current_.phsEl.at(0))*180*3600/M_PI;
       // The test array config may not have had 23 antennas in it.
       // In this case percent will be a NaN, so don't try to output
       // superfluous antenna data.  But we always want antenna 1
       // which is at the reference point.
       if ( (! isnan(percent)) || antid == 0) { 
        astream << antennaNo 
                << " "
                << setprecision(4)
                << current_.X.at(antid)
                << "    "
                << current_.Y.at(antid)
                << "    "
                << current_.Z.at(antid)
                << "    "
                << current_.phsRa.at(antid)*rad2hr 
                << "    "
                << current_.phsDec.at(antid)*180.0/M_PI 
                << "    "
                << ha*rad2hr
                << "    "
                << scientific
                << current_.phsEl.at(antid)*180.0/M_PI
                << "    "
                << setprecision(2)
                << dEl
                << " | "
                << setprecision(4)
                << current_.geometricDelay.at(antid)*1000.0
                << "     "
                << 1000.0*(current_.pathlength.at(antid)-refPtDelay_.at(CURRENT).pathlength.at(antid))
                << "     "
                << current_.troposphericDelay.at(antid)*1.0E3
                << "     "
                << current_.totalDelay.at(antid)*1000.0
                << " | "
                << bima_.geometricDelay.at(antid)*1000.0
                << "     "
                << bima_.pathlength.at(antid)*1000.0
                << "     "
        // add height delay back in to get pure bima tropospheric
                << (bima_.troposphericDelay.at(antid)+bima_.heightDelay.at(antid))*1.0E3
                << "     "
                << bima_.troposphericDelay.at(antid)*1.0E3
                << "       "
                << bima_.heightDelay.at(antid)*1.0E3
                << "     "
                << bima_.totalDelay.at(antid)*1000.0
                << " | "
                << diff*1.0e3
                << "     "
                << percent*100.0
                << "     "
                << diffT*1.0e3
                << "     "
                << percentT*100.0;
               myLogger << log4cpp::Priority::INFO << astream.str();
       }

    }
}

// Hat Creek code that is used to compare results from the CARMA code
double 
DelayEngine::computeBimaDelay(unsigned short antennaNo, 
                              const DelayInfo & delayInfo)
{
    int i = antennaNo - 1; // array index runs from zero.

    //
    // Use floats where BIMA uses floats
    double kelvin  = delayInfo.airTemp;
    float relhumid = delayInfo.relHumid;
    float pressmb  = delayInfo.atmPressure;
    double es;          // saturated water pressure
    double refraction;  // Pathlength coefficient
    double arefraction; // refractivity from Smith-Weintraub equation
    double TAU;         // total delay
    double ANTPOS[3];   // antenna position, nanoseconds

    double COSLAT  = cos( delayInfo.latitude.at(i) );
    double SINLAT  = sin( delayInfo.latitude.at(i) );
    double COSD    = cos( delayInfo.phsDec.at(i) );
    double SIND    = sin( delayInfo.phsDec.at(i) );
    double elevrad = delayInfo.phsEl.at(i);
    double ha      = computePhaseCenterHourAngle(antennaNo, delayInfo);
    double COSH    = cos(ha);
    double SINH    = sin(ha);
    double ANTMISS = delayInfo.axisMis.at(i);
    // convert antenna position meters to nanoseconds; BIMA uses
    // topocentric X,Y,Z.
    ANTPOS[0] = delayInfo.X.at(i) * 1.0E9 / Physical::C ; 
    ANTPOS[1] = delayInfo.Y.at(i) * 1.0E9 / Physical::C ; 
    ANTPOS[2] = delayInfo.Z.at(i) * 1.0E9 / Physical::C ;

    //==================================================================
    // Calculate geometric delay plus axis misalignment as in 
    // BIMA delay.c::setup_siderial (sic)
    //==================================================================
    // cut and paste code exactly from delay.c (deleting array index i 
    // in bima code):
    TAU = -(ANTPOS[0]*COSD*COSH - ANTPOS[1]*COSD*SINH 
                + ANTPOS[2]*SIND) + ANTMISS*cos(elevrad) ;
    //==================================================================

    //==================================================================
    // Calculate the refraction quantities as in BIMA delay.c::calc_refract
    //==================================================================
    // cut and paste code exactly from delay.c:
    es = 6.105 * 
         exp((25.22*(kelvin-273.2)/kelvin) - (5.31*log(kelvin/273.2))) ;
    es = es * (double)relhumid /100. ;
    refraction = 1.e-6 / 6.28318530718 *
      ((.53*(double)pressmb/kelvin) + (915. *es /(kelvin*kelvin))) ;
    arefraction = 1.e-6  *
          ((77.6*(double)pressmb/kelvin) + (3.73e5 *es /(kelvin*kelvin))) ;
    /*
    ostringstream cmpbimaOS;
    cmpbimaOS
        << " kelvin   = " << kelvin 
        << " relhumid = " << relhumid 
        << " pressmb  = " << pressmb
        << " ppw(B) = " << es
        << " ppw(C) = " << atm_.computeSaturatedPressure(kelvin)*relhumid*.01
        << " refraction = " << refraction
        << " zRefrac(B) = " << 1.0E6 * arefraction
        << " zRefrac(C) = " << atm_.computeZenithRefractivity(kelvin,pressmb,relhumid,115.271E9)
        << " TAUg = " << TAU;
    programLogInfoIfPossible( cmpbimaOS.str() );
    */
    //==================================================================

    double hd = arefraction/sin(elevrad) *
                  (ANTPOS[0] * COSLAT + ANTPOS[2] * SINLAT) ;
    bima_.heightDelay.at(i) = hd;
    bima_.troposphericDelay.at(i) = TAU * refraction/(pow(sin(elevrad),2.)) ;
    bima_.troposphericDelay.at(i) -= hd;
    bima_.pathlength.at(i) = bima_.troposphericDelay.at(i) 
                            * Physical::C/units_.giga(); 
    //==================================================================
    // cut and paste refraction correction from delay.c::siderial_time
    // (again, minus the index i). comment is also from delay.c
    //==================================================================
    
    /* The first correction is for differential refraction; the
       second corection is for antennas at different heights.
       The calculations come from  Thompson,moran,swenson as
       interpreted by Welch.
     */
    TAU += TAU * refraction/(pow(sin(elevrad),2.)) ;
    TAU -= hd;
    //==================================================================
    

    // Total delay
    // from delay.c: DELAY[i] = DELAY0[i] -TAU[i] ;
    double delay = delayOffset_.at(i) - TAU;

    // Save individual parts of the BIMA delay calc in a delay info 
    // object with CARMA delay definitions.
    bima_.geometricDelay.at(i) = -(ANTPOS[0]*COSD*COSH - ANTPOS[1]*COSD*SINH 
                                + ANTPOS[2]*SIND) - arefraction/sin(elevrad) 
                                * (ANTPOS[0] * COSLAT + ANTPOS[2] * SINLAT) ;
    /*bima_.troposphericDelay.at(i) = 
        (bima_.geometricDelay.at(i) + bima_.axisDelay.at(i)) 
          * refraction/(pow(sin(elevrad),2.)) ; 
     */

    return delay;
}
