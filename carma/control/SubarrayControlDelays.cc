/**
 * Carma control interface server implementation for various 
 * methods that handle delay data,
 * e.g. broadcast delays to correlator and loberotator, 
 * convert delay formats for clients, etc.
 *
 * @author: Marc Pound
 *
 * $Id: SubarrayControlDelays.cc,v 1.49 2013/10/01 23:27:43 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <sstream>
#include <cstdlib>

#include "carma/control/AntennaControls.h"
#include "carma/control/errorMsgs.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/FunctorWorkRequest.h"
#include "carma/control/HandleMethodFunctorGroup.h"
#include "carma/control/LoberotatorHandle.h"
#include "carma/control/RemoteObjMethodFunctorGroup.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/control/SubarrayTrackerThread.h"
#include "carma/control/SatThreadSync.h"
#include "carma/control/WorkerPool.h"
#include "carma/interferometry/DelayEngine.h"
#include "carma/loberotator/LoberotatorControl.h"
#include "carma/services/DecAngle.h"
#include "carma/services/Ephemeris.h"
#include "carma/signalpath/SignalPathMapperControl.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StringUtils.h"
#include "carma/util/WorkResult.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::interferometry;
using namespace carma::loberotator;
using namespace log4cpp;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::signalpath;
using namespace carma::util;

namespace {

// mutex for broadcasting delays and making calls to delay engine.
// yes they are identical, but i want the names to indicate function.
::pthread_mutex_t gDelayGuard = PTHREAD_MUTEX_INITIALIZER;
typedef ScopedLock< ::pthread_mutex_t > DelayBroadcastLock;
typedef ScopedLock< ::pthread_mutex_t > DelayEngineLock;

::size_t NUM_DELAY_FRAMES = 3;
} // namespace < anonymous >


/**
 * Precess the UV coordinates from the current epoch, to J2000.
 * (MIRIAD requires J2000). Note that W does not change with epoch.
 *
 * @param The current epoch U in meters
 * @param The current epoch V in meters
 * @param theta The rotation angle returned from Ephemeris::angle2000(),
 * in radians
 */
vector<double>
SubarrayControlImpl::rotateUVtoJ2000( double U, double V, double theta )
{
    CARMA_CPTRACE(Trace::TRACE7,"rotating U and V by " << theta);
    double costh = cos(theta);
    double sinth = sin(theta);
    double u2000 =  (U * costh) + (V * sinth);
    double v2000 =  (V * costh) - (U * sinth);

    vector<double> uv2000;
    uv2000.reserve(2);
    uv2000[0] = u2000;
    uv2000[1] = v2000;
    CARMA_CPTRACE(Trace::TRACE7,"done rotating U and V ");
    return uv2000;
}

// theta is the rotation angle for precessing U and V to J2000.
void
SubarrayControlImpl::
copyAntDelayData( DelayEngineSubsystem::DelayData &       lhs,
                  const DelayEngineSubsystem::DelayData & rhs,
                  double theta )
{
    // Ugh, this is painful. We have to copy each monitor point in
    // each monitor container individually, because MonitorComponent
    // has a private copy constructor.

    lhs.adjustableDelay().setValue( rhs.adjustableDelay().getValue() );
    lhs.adjustableDelayStatus().setValue(
        rhs.adjustableDelayStatus().getValue()
        );
    lhs.antennaDelay().setValue( rhs.antennaDelay().getValue() );
    lhs.opticsDelayMM().setValue( rhs.opticsDelayMM().getValue() );
    lhs.opticsDelayCM().setValue( rhs.opticsDelayCM().getValue() );
    lhs.loCableDelayMM().setValue( rhs.loCableDelayMM().getValue() );
    lhs.loCableDelayCM().setValue( rhs.loCableDelayCM().getValue() );
    lhs.delayOffset().setValue( rhs.delayOffset().getValue() );
    lhs.pointState().setValue( rhs.pointState().getValue() );
    lhs.calculatedAt().setValue( rhs.calculatedAt().getValue() );
    lhs.calculatedFor().setValue( rhs.calculatedFor().getValue() );
    lhs.validUntil().setValue( rhs.validUntil().getValue() );
    lhs.axisDelay().setValue( rhs.axisDelay().getValue() );
    lhs.geometricDelay().setValue( rhs.geometricDelay().getValue() );
    lhs.geometricDelayStatus().setValue(
        rhs.geometricDelayStatus().getValue()
        );
    lhs.heightDelay().setValue( rhs.heightDelay().getValue() );
    lhs.heightDelayStatus().setValue( rhs.heightDelayStatus().getValue() );
    lhs.ionosphericDelay().setValue( rhs.ionosphericDelay().getValue() );
    lhs.ionosphericDelayStatus().setValue(
        rhs.ionosphericDelayStatus().getValue()
        );
    lhs.pathLength().setValue( rhs.pathLength().getValue() );
    lhs.padDelay().setValue( rhs.padDelay().getValue() );
    lhs.refractivity().setValue( rhs.refractivity().getValue() );
    lhs.rxDelayPol1().setValue( rhs.rxDelayPol1().getValue() );
    lhs.rxDelayPol2().setValue( rhs.rxDelayPol2().getValue() );
    lhs.thermalDelay().setValue( rhs.thermalDelay().getValue() );
    lhs.thermalDelayStatus().setValue( rhs.thermalDelayStatus().getValue() );
    lhs.totalDelay().setValue( rhs.totalDelay().getValue());
    lhs.totalFixedDelay().setValue(rhs.totalFixedDelay().getValue() );
    lhs.totalDelayPol1().setValue( rhs.totalDelayPol1().getValue() );
    lhs.totalDelayPol2().setValue( rhs.totalDelayPol2().getValue() );
    lhs.troposphericDelay().setValue( rhs.troposphericDelay().getValue() );
    lhs.troposphericDelayStatus().setValue(
        rhs.troposphericDelayStatus().getValue()
        );
    vector<double> uv2000 = rotateUVtoJ2000(
        rhs.u().getValue(),
        rhs.v().getValue(),
        theta
        );
    lhs.u().setValue( uv2000[0] );
    lhs.v().setValue( uv2000[1] );
    lhs.w().setValue( rhs.w().getValue() );
    lhs.x().setValue( rhs.x().getValue() );
    lhs.y().setValue( rhs.y().getValue() );
    lhs.z().setValue( rhs.z().getValue() );
}


short 
SubarrayControlImpl::getWalshCol(WalshColVec assignments, short antNo) {
    WalshColVec::const_iterator i = assignments.begin();
    const WalshColVec::const_iterator iEnd = assignments.end();
    for ( ; i != iEnd; ++i ) {
        if ( (*i).antNo == antNo) {
            short wc = (*i).walshColNo;
            if (wc == 0) {
                ostringstream o;
                o << "SPM walsh col is 0 for ant" << (*i).antNo
                  << ", setting walsh column to 1";
                Program::getLogger() << Priority::WARN<< o.str();
                return 1;
            }
            return wc;
        }
    }
    ostringstream o;
    o << "getWalshCol: AntNo " << antNo << " not found in WalshCol assignments;"
      << " column defaulted to 1. ";
    if (assignments.size() == 0) {
        o << "No antenna assignments available.";
    }
    else {
        o << assignments.size() << " assignments available";   
        if (true) {
            o << ":\n Ant Wal";
            for ( ; i != iEnd; ++i ) {
                o <<  "\n" << setw(4) << (*i).antNo
                        << setw(4) << (*i).walshColNo;
            }
        }
        else {
            o << ".";
        }
    }
    //SubarrayControlImpl::loginfo()<< o.str(); 
    Program::getLogger() << Priority::WARN<< o.str();
    return 1;   
}


void
SubarrayControlImpl::broadcastDelayData( const DelayFrameVec & delayFrameVec )
try {
    // Sync this to tracker thread.
    const TrackerThreadSync sync( *this );
    const ScopedLogNdc ndc( "SaCI::broadcastDelayData" );

    CARMA_CPTRACE( Trace::TRACE4, "Begin" );

    // write the new delays to the monitor stream.
    // the most recent delays will be in the highest
    // index of the vector

    const ::size_t numDelayFrames = delayFrameVec.size();

    if ( numDelayFrames != NUM_DELAY_FRAMES ) {
        ostringstream oss;
        oss << "Missing delay frames [nframes ="
            << numDelayFrames << "], expected "
            << NUM_DELAY_FRAMES;
        throw CARMA_ERROR( oss.str() );
    }

    const AntControlsGroup antControlsGroup = getAntControlsGroup();
    
    if ( antControlsGroup.empty() ) {
        // If we don't actually have antennas assigned to the subarray,
        // don't bother to update clients
        /*programLogInfoIfPossible(
          "No antennas owned by this subarray -- skipped Loberotator and"
          " Correlator calls; no delay/UVW monitor points to update either." 
        );
        */

        return;
    }

    if ( initializationFlag_ == false ) {
        // possibly verbose on startup.
        programLogInfoIfPossible("Not broadcasting delays because subarray is not yet initialized.");
    } else {
        updateLoberotator( delayFrameVec );

        {
            // I would prefer this lock inside updateCorrelator.
            // But as of now (6/20/2006), this is the only place
            // where updateCorrelator is called. -mwp
            const DelayBroadcastLock dlock( gDelayGuard );

            // send delays to correlator using signalpath mapper
            updateCorrelator( delayFrameVec );
        }
    }

    const DelayEngineSubsystem * const delayFrame =
        delayFrameVec.at(  numDelayFrames - 1 );

    CARMA_CPTRACE( Trace::TRACE1, "Copying ant delay data" );

    AntControlsGroup::const_iterator i = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    for ( ; i != iEnd; ++i ) {
        AntennaControls * const antControls = *i;

        if ( antControls != 0 ) {
            const unsigned short carmaAntNo =
                antControls->getCarmaAntennaNo();

            // rotation angle to precess from current coordinates
            // to J2000.  on call on ephemeris if the subarray has been
            // initialized. otherwise, Ephem throws an MJD not set error.
            double theta = 0.0;
            if ( initializationFlag_ ) {
                try {
                    theta = antControls->driveHandle()->getEphemeris().angle2000();
                } catch ( util::ErrorException& be ) {
                    ostringstream ugh;
                    // This is not fatal and is expected on startup,
                    // before the drives are tracking.
                    ugh << " Error trying to set angle2000 from driveHandle "
                    << " for antenna " << carmaAntNo
                    << " : " << be.getMessage ();
                    programLogNoticeIfPossible( ugh.str() );
                }
            }

            // copy the delay data, and while you're at it
            // rotate U and V to J2000
            copyAntDelayData( delaySubsystem_.delayData( carmaAntNo - 1 ),
                              delayFrame->delayData( carmaAntNo - 1 ),
                      theta );
            //
            // UVW do not depend on the delay value, so no
            // modification of this section of code needed for
            // dual polarization.
            //
            // This is a good time & place to update the UVW interpolators
            // in AntennaControls objects.
            float U, V, W; 
            double mjd;
            //
            // If we had a triplet reset all interp UVW values,
            // otherwise just extend the container.
            const DelayEngineSubsystem * dFrame =
                  delayFrameVec.at( 0 );
            U = dFrame->delayData( carmaAntNo - 1 ).u().getValue();
            V = dFrame->delayData( carmaAntNo - 1 ).v().getValue();
            W = dFrame->delayData( carmaAntNo - 1 ).w().getValue();
            mjd = dFrame->delayData( carmaAntNo - 1 )
                                 .calculatedFor().getValue();
                                                             
            antControls->updateUVWInterpolators(U, V, W, mjd, true);
            dFrame = delayFrameVec.at( 1 );
            U = dFrame->delayData( carmaAntNo - 1 ).u().getValue();
            V = dFrame->delayData( carmaAntNo - 1 ).v().getValue();
            W = dFrame->delayData( carmaAntNo - 1 ).w().getValue();
            mjd = dFrame->delayData( carmaAntNo - 1 )
                                 .calculatedFor().getValue();
            antControls->updateUVWInterpolators(U, V, W, mjd, false );

            U = delayFrame->delayData( carmaAntNo - 1 ).u().getValue();
            V = delayFrame->delayData( carmaAntNo - 1 ).v().getValue();
            W = delayFrame->delayData( carmaAntNo - 1 ).w().getValue();
            mjd = delayFrame->delayData( carmaAntNo - 1 )
                                 .calculatedFor().getValue();
            antControls->updateUVWInterpolators(U, V, W, mjd, false );
        }
    }

    CARMA_CPTRACE( Trace::TRACE1, "Ant delay data copied" );

    CARMA_CPTRACE( Trace::TRACE4, "End" );
} catch ( ... ) {
    programLogErrorIfPossible( 
        "Coming out of SaCI::broadcastDelayData() on an exception" 
        );

    throw;
}

SubarrayControlImpl::LoberotatorGroup
SubarrayControlImpl::getLoberotatorGroup( )
{
    LoberotatorGroup result;
    LoberotatorHandle* const lhp = loberotator_.get();

    if ( lhp != 0 ) result.insert( lhp );

    return result;
}

void
SubarrayControlImpl::updateLoberotator(const DelayFrameVec & delayFrameVec)
try {
    // mutex protect this call
    const DelayBroadcastLock dlock( gDelayGuard );

    const string methodName = "SaCI::updateLoberotator" ;
    const ScopedLogNdc ndc( methodName );
    const string handleMethod 
        = "LoberotatorHandle::setDelayUpdate(DelayFrameVec)";

    CARMA_CPTRACE( Trace::TRACE4, "Calling " << handleMethod << "via WRS" );

    WorkResultSet wrs( handleMethod + " result set" );

    const LoberotatorGroup loberotatorGroup = getLoberotatorGroup();

    const LoberotatorControl::DelayFreqPacket dfp
        = convertDelayFrameVecToDelayFreqPacket(delayFrameVec);

    queueFunctorWorkRequestGroup(
        handleMethod,
        makeHandleMethodFunctorGroup(
            loberotatorGroup,
            &LoberotatorHandle::updateDelayAndFreq,
            dfp),
        wrs,
        *workerPool_,
        false );

    waitForAllNormal( wrs, false );

    CARMA_CPTRACE( Trace::TRACE4, "Exiting" );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


LoberotatorControl::DelayFreqPacket 
SubarrayControlImpl::convertDelayFrameVecToDelayFreqPacket( 
        const DelayFrameVec& delayFrameVec)
try {
    const string methodName = "SacI::convertDelayFrameVecToDelayFreqPacket";
    const ScopedLogNdc ndc( methodName );
    CARMA_CPTRACE( Trace::TRACE4, "Entering" );

    const size_t numDelayFrames = delayFrameVec.size();
    if ( numDelayFrames != NUM_DELAY_FRAMES ) {
        ostringstream oss;
        oss << "Missing delay frames [nframes ="
            << numDelayFrames << "], expected "
            << NUM_DELAY_FRAMES;
        throw CARMA_ERROR( oss.str() );
    }

    const AntControlsGroup antControlsGroup     = getAntControlsGroup();
    AntControlsGroup::const_iterator i          = antControlsGroup.begin();
    const AntControlsGroup::const_iterator iEnd = antControlsGroup.end();

    vector<LoberotatorControl::DelayChan> dcVec; 
    // Get assignments for all antennas
    WalshColVec walshColVec = signalPathMapper_->getWalshColumnAssignment(0);  

    ostringstream tracer;
    tracer << methodName << " - ";
    tracer << " [Ant/(delay,mjd)] = ";
    // Loop over all antennas in the subarray
    for ( ; i != iEnd; ++i ) {
        const AntennaControls* const antControls = *i;
        if ( antControls == 0 ) {
            throw CARMA_ERROR( NULL_ANTENNA );
        }

        LoberotatorControl::DelayChan dc;

        const short carmaAntNo = antControls->getCarmaAntennaNo() ;
        short walshColNo = getWalshCol(walshColVec, carmaAntNo); 

        // The loberotator channel is always mapped to the antenna 
        // with the same #
        dc.channelID = carmaAntNo;
        
        // Set phase switching walsh column
        dc.walshColumn = walshColNo;

        tracer << " ["<< carmaAntNo << "/";
        for ( size_t j = 0; j < numDelayFrames; j++ ) {

            const DelayEngineSubsystem* const df = delayFrameVec.at( j );

            if ( df == 0 ) {
                ostringstream oss;
                oss << methodName << " - NULL delay frame pointer at frame " 
                    << j << ")";
                throw CARMA_ERROR( oss.str() );
            }

            DelayEngineSubsystem::DelayData& antDelayData =
                df->delayData( carmaAntNo - 1 );
            dc.triplet[ j ].mjd   = antDelayData.calculatedFor().getValue( );

            // Loberotator needs delay before first downconversion;
            // including the fixed parts from the IF delays gives a
            // frequency dependence to the phase, so we remove them.
            // We also correct for the LO cable delay after the LO termination.
            double loCableDelay = 0.0;
            if (loFreq_ < 50.0) {
                loCableDelay = extraLOcableDelayCM_.at(carmaAntNo-1);
            }
            else {
                loCableDelay = extraLOcableDelayMM_.at(carmaAntNo-1);
            }
            double preDownconversionDelay = 
                    antDelayData.totalDelay().getValue()
                    - antDelayData.totalFixedDelay().getValue()
                    - loCableDelay;
            dc.triplet[j].delay = preDownconversionDelay;
            tracer << "(" 
                   << setprecision(9)
                   << dc.triplet[ j ].delay << ","
                   << dc.triplet[ j ].mjd << ")";
        }
        tracer << "] ";

        dcVec.push_back(dc);
    }

    CARMA_CPTRACE( Trace::TRACE6, tracer.str() );

    LoberotatorControl::DelayFreqPacket dfp;
    dfp.delaySeq = 
        convertVectorToSequence< LoberotatorControl::DelayChanSeq >( dcVec );

    // These are by definition the same for all antennas in the subarray.
    dfp.frequency  = loFreq_;
    dfp.multiplier = LOchain_.getOscillatorMultiplier();
    dfp.divisor    = 1; // @todo: eventually must not be hardcoded.
    dfp.sign       = 1; // @todo: eventually must not be hardcoded.

    CARMA_CPTRACE( Trace::TRACE4, "Exiting" );

    return dfp;
} catch ( ... ) {
    rethrowCaughtAsUser();    
    // This is just to shut the compiler warning
    throw CARMA_EXCEPTION( util::UserException,
                           "Very bad things in"
                           " SaCI::convertDelayFrameVecToDelayFreqPacket" );
}

void
//SubarrayControlImpl::renewDelays( const AntControlsGroup & antGroup, 
SubarrayControlImpl::renewDelays(const DriveGroup & driveGroup, double now) 
{
    const string methodName = "SacI::renewDelays";
    const ScopedLogNdc ndc( methodName );
    CARMA_CPTRACE( Trace::TRACE4, "Entering ");

    // this method throws if trackerThread_ is null.
    double updateTime = computeTruncatedInterfUpdateTime( now );

    trackerThread_->setLastIntferUpdate( updateTime );
    const MjdTriplet mjdTriplet = getAlignedInterfUpdateTimes( now );
    //DriveGroup driveGroup 
    // = getDriveGroupForAntControlsGroup( "renewDelays", antGroup );
    
    DriveGroup::const_iterator i = driveGroup.begin( );
    const DriveGroup::const_iterator iEnd = driveGroup.end( );

    DelayFrameVec v;
    {
    const DelayEngineLock dlock( gDelayGuard );
    
    for ( ; i != iEnd; ++i ) {
        DriveHandle * const driveHandle = *i;

        if ( driveHandle == 0 )
        continue;

        const unsigned short carmaAntNo =
        driveHandle->getCarmaAntennaNo( );

        //  Get a copy of the ephemeris of the DriveHandle,
        Ephemeris ephem = driveHandle->getEphemeris();

        // Depending on whether the drive is in AZEL or RADEC
        // mode, we will make different calls the the delay engine.
        // If IDLE mode, make no call to the delay engine.
        DriveHandle::SourceMode mode = driveHandle->getSourceMode();
        const string sourceName = driveHandle->getSourceName() ;


        // Loop over all values in the triplet
        for ( ::size_t j = 0; j < NUM_DELAY_FRAMES ; ++j ) {

        // Retrieve the phase center sky position, including any phase
        // center offsets (but NOT drive offsets).
        // Formally this should be done using the antenna position from
        // DriveHandle ephemeris.  However, even for the longest CARMA 
        // baselines, the difference in the resulting RA/DEC is negligible. 
        // (< 1 milliarcsecond)
        arrayRefEphem_->setMJD( mjdTriplet.mjd[j] );
        const double phaseRa  = arrayRefEphem_->getRa();
        const double phaseDec = arrayRefEphem_->getDec();
        string phsrastr  = StringUtils::hms(phaseRa,2);
        string phsdecstr = DecAngle(phaseDec,"radians").dms(2);

        switch ( mode ) {

            case DriveHandle::SOURCE_MODE_RA_DEC :
            {
            //
            //  Set the new mjd of ephemeris.
            //  Get apparent ra and dec pointing centers for this 
            //  antenna at mjd from ephemeris.
            //
            ephem.setMJD( mjdTriplet.mjd[j] );
            const double antennaRa  = ephem.getRa();
            const double antennaDec = ephem.getDec();
            string rastr = StringUtils::hms(antennaRa,2);
            string decstr = DecAngle(antennaDec,"radians").dms(2);
            
            CARMA_CPTRACE( Trace::TRACE4, 
            " - DE->setAntennaRaDec("
            << carmaAntNo << "," 
            << setprecision(10) 
            << mjdTriplet.mjd[j]     << ","
            << rastr      << ","
            << decstr     << ","
            << phsrastr   << ","
            << phsdecstr  << ","
            << sourceName
            << ")"
            );

                
            // Note delay phase and tracking offsets will get set 
            // to zero inside this call for calculation of delay.
            // This is okay because we are passing in the total
            // ra/dec which include offsets.
            delayEngine_->setAntennaRaDec( carmaAntNo,
                           mjdTriplet.mjd[j],
                           antennaRa, antennaDec,
                           phaseRa, phaseDec,
                           true,
                           sourceName
                          );

            }
            break;

            case DriveHandle::SOURCE_MODE_AZ_EL:
         // for calculating delays, 
         // choose az and el between 0-2PI, 0-PI respectively
         // [param=true]
            {
            double azRadians = 
            ephem.getSource().getXCoordinate().radians(true);
            double elRadians = 
            ephem.getSource().getYCoordinate().radians(true);
            delayEngine_->setAntennaAzEl( carmaAntNo,
                           mjdTriplet.mjd[j],
                           azRadians, elRadians,
                           azRadians, elRadians
                           );

            }
            break;

         case DriveHandle::SOURCE_MODE_IDLE:
             // Do nothing for IDLE or unrecognized mode.
            break;
         default:
             // Complain if mode is unrecognized.
            ostringstream os;
            os << methodName 
               << ": Unrecognized drive mode: " 
               << mode <<". Ignoring it.";
            programLogErrorIfPossible( os.str() );
            break;
        } // switch

        } // for j (mjd)
    } // for i (DriveHandle)

    v = delayEngine_->computeDelays();

    } // DelayEngineLock scope
    
    CARMA_CPTRACE( Trace::TRACE4, " - calling broadcastDelayData" );
    broadcastDelayData( v );
    CARMA_CPTRACE( Trace::TRACE4, " - done with broadcastDelayData" );
    CARMA_CPTRACE( Trace::TRACE4, "Exiting" );
}

// Be careful here, errors in the config tables will throw exceptions
void 
SubarrayControlImpl::loadDelayTables()
{
    const string padData     = "delay/padDelay.tab";
    const string antData     = "delay/antDelay.tab";
    const string opticsData  = "delay/opticsDelay.tab";
    const string loCableData = "delay/extraLOcable.tab";
    const string columnName  = "delay";
    const string columnMM    = "mmRx";
    const string columnCM    = "cmRx";

    // pad delays
    const string padFile = Program::getConfFile( padData );
    Table padTable;
    padTable.open( padFile );
    vector<int> padNo = padTable.getIntColumn("padNo");
    vector<double> delay = padTable.getDoubleColumn( columnName );

    vector<int>::iterator pi = padNo.begin();
    vector<int>::iterator pEnd = padNo.end();
    vector<double>::iterator di = delay.begin();
    while ( pi != pEnd ) {
        padDelay_.insert( make_pair( *pi++, *di++ ) );
    }

    // antenna specific delays
    string delayFile = "";
    Table delayTable;
    delayFile        = Program::getConfFile(antData);
    try {
        delayTable.open(delayFile);
    } catch(...) {
        string m = carma::util::getStringForCaught();
        programLogError("Fatal error:" + m);
        cout <<"Rethrowing fatal exception: " << m << endl;
        throw;
    }   
    antDelay_        = delayTable.getDoubleColumn(columnName);
    int sz = antDelay_.size();
    if (sz != 23) {
        ostringstream o;
        o << "The configuration file " << delayFile 
          << " contains " << sz
          << " rows. It must contain exactly 23 rows, one for each antenna."; 
        cout << o.str() << endl;
        throw CARMA_ERROR(o);
    }
    
    // Optic delays
    delayFile        = Program::getConfFile(opticsData);
    try {
        delayTable.open(delayFile); 
    } catch(...) {
        string m = carma::util::getStringForCaught();
        programLogError("Fatal error:" + m);
        cout <<"Rethrowing fatal exception: " << m << endl;
        throw;
    }   
    opticsDelayMM_   = delayTable.getDoubleColumn(columnMM);
    sz = opticsDelayMM_.size();
    if (sz != 23) {
        ostringstream o;
        o << "The configuration file " << delayFile 
          << " contains " << sz << " rows for the mmRx column."
          << " It must contain exactly 23 rows, one for each antenna."; 
        cout << o.str() << endl;
        throw CARMA_ERROR(o);
    }
    opticsDelayCM_   = delayTable.getDoubleColumn(columnCM);
    sz = opticsDelayCM_.size();
    if (sz != 23) {
        ostringstream o;
        o << "The configuration file " << delayFile 
          << " contains " << sz << " rows for the cmRx column."
          << " It must contain exactly 23 rows, one for each antenna."; 
        cout << o.str() << endl;
        throw CARMA_ERROR(o);
    }
    
    // LO cable
    delayFile        = Program::getConfFile(loCableData);
    try {
        delayTable.open(delayFile);    
    } catch(...) {
        string m = carma::util::getStringForCaught();
        programLogError("Fatal error:" + m);
        cout <<"Rethrowing fatal exception: " << m << endl;
        throw;
    }   
    extraLOcableDelayMM_   = delayTable.getDoubleColumn(columnMM);
    sz = extraLOcableDelayMM_.size();
    if (sz != 23) {
        ostringstream o;
        o << "The configuration file " << delayFile 
          << " contains " << sz << " rows for the mmRx column."
          << " It must contain exactly 23 rows, one for each antenna."; 
        cout << o.str() << endl;
        throw CARMA_ERROR(o);
    }
    extraLOcableDelayCM_   = delayTable.getDoubleColumn(columnCM);
    sz = extraLOcableDelayCM_.size();
    if (sz != 23) {
        ostringstream o;
        o << "The configuration file " << delayFile 
          << " contains " << sz << " rows for the cmRx column."
          << " It must contain exactly 23 rows, one for each antenna."; 
        cout << o.str() << endl;
        throw CARMA_ERROR(o);
    }
    // Convert from meters to nsec
    const double lightSpeed_    = 2.99792458e8; // m/sec
    for (int i=0; i<23; i++) {
        opticsDelayMM_[i] = 1e9*opticsDelayMM_.at(i)/lightSpeed_;
        opticsDelayCM_[i] = 1e9*opticsDelayCM_.at(i)/lightSpeed_;
    }
}
