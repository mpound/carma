/**
 *
 * Carma Rx selector interface implementation.
 *
 * @author: Steve Scott
 *
 * $Id: RxSelectorHandle.cc,v 1.42 2011/10/27 22:55:30 scott Exp $
 *
 * $CarmaCopyright$
 *
 */
 

#include <cstdlib>

#include "carma/corba/corba.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/control/antennaHandleUtils.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::util;


namespace {

string
getStringForRxType( const RxControl::Type rxType )
{
    switch ( rxType ) {
        case RxControl::RX1CM: return "RX1CM";
        case RxControl::RX1MM: return "RX1MM";
        case RxControl::RX3MM: return "RX3MM";
        case RxControl::RXANY: return "RXANY";
    }
    
    return "< unknown >";
}


}  // namespace < anonymous >


RxSelectorHandle::RxSelectorHandle(
    const unsigned short           carmaAntNo,
    MonitorSystem&                 monitorSystem,
    ControlSubsystemBase::Antenna& antenna ) :
RxSelectorRemoteObjHandle( makeAntennaDoName( carmaAntNo, RXSELECTOR_NAME ),
                           &(antenna.antennaReachable( ).rxSelector( )),
                           &(getAntennaSubsystem( carmaAntNo, monitorSystem )),
                           &monitorSystem,
                           true,
                           false ),
carmaAntNo_(carmaAntNo),
nextSequenceNo_(-10),
consecutiveErrorCount_(0),
pendingIvCurveRxType_(RxControl::RX1MM),
pendingIvCurvePolType_(RxControl::SINGLE)
{
    // nothing to do here?
}


RxSelectorHandle::~RxSelectorHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


unsigned short RxSelectorHandle::getCarmaAntennaNo( ) const
{
    return carmaAntNo_ ;
}

void
RxSelectorHandle::handleNilRx( const RxControl::Type rxType ) const
{
    const string msg = doName() +
                       " returned NIL for the Rx for type " +
                       getStringForRxType( rxType );
    
    programLogErrorIfPossible( msg );
    
    throw CARMA_ERROR( msg );
}

void
RxSelectorHandle::handleNilIF( const RxControl::Type rxType ) const
{
    const string msg = doName() +
                       " returned NIL for the IF for RX " +
                       getStringForRxType( rxType );
    
    programLogErrorIfPossible( msg );
    
    throw CARMA_ERROR( msg );
}

void
RxSelectorHandle::handleNilFrontEnd( const RxControl::Type rxType ) const
{
    const string msg = doName() +
                       " returned NIL for the FrontEnd for RX " +
                       getStringForRxType( rxType );
    
    programLogErrorIfPossible( msg );
    
    throw CARMA_ERROR( msg );
}

void
RxSelectorHandle::setFrequency( const RxControl::Type   rxType, 
                                const double            yigFreq,
                                const double            loFreq,
                                const double            refFreq,
                                const int               harmonic,
                                const bool endWithAbsorberInBeam,
                                const bool              optimizeReceiver,
                                const bool              forceRelock,
                                ControlSubsystem* const controlSubsystem,
                                MonitorSystem* const    monsys, 
                                const int               preferredSequenceNo )
{
    if (!isObjReachable()) return;
    string remoteCallString;
    {
        ostringstream oss;            
        oss << getStringForRxType( rxType )
            << " Rx::setFrequency("
            << "yigFreq=" << yigFreq
            << " loFreq=" << loFreq
            << ")";                
        remoteCallString = oss.str( );
    }
    int seqNo = getAntennaCommon(carmaAntNo_, *monsys).
                    receivers().tuneSeqNum().getValue();
    if (seqNo == preferredSequenceNo) {
        nextSequenceNo_ = preferredSequenceNo + 10;
    }
    else {
        nextSequenceNo_ = preferredSequenceNo;
    }
            
    try {
        const double sendTime = Time::MJD( );
        RxControl_var rxVar = remoteObj( )->Rx( rxType );
         
        if ( CORBA::is_nil( rxVar ) ) handleNilRx( rxType );            
        rxVar->setFrequency( yigFreq, loFreq,
                endWithAbsorberInBeam, optimizeReceiver, forceRelock,
                nextSequenceNo_ );
        controlSubsystem->antenna(carmaAntNo_-1).refLoFreq().setValue(refFreq);   
        controlSubsystem->antenna(carmaAntNo_-1).harmonic().setValue(harmonic);   
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( CORBA::SystemException & e ) {
        processException( remoteCallString, e );
    }
}

void 
RxSelectorHandle::antennaIFpower( const antenna::common::RxControl::Type rxType,
                                  const double power)
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        ostringstream oss;
            
        oss << getStringForRxType( rxType )
            << " Rx::antennaIFpower("<< "power=" << power << ")";                
        remoteCallString = oss.str( );
            
        try {
            //const double sendTime = Time::MJD( );
            RxControl_var rxVar = remoteObj()->Rx(rxType);            
            if (CORBA::is_nil(rxVar)) handleNilRx(rxType);
            
            rxVar->setIFPower(power);
            
            //logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( CORBA::SystemException & e ) {
            processException( remoteCallString, e );
        }
    }
}   
  
void 
RxSelectorHandle::antennaIFpresetPower( 
    const antenna::common::RxControl::Type rxType )
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        ostringstream oss;
            
        oss << getStringForRxType( rxType )
            << " Rx::antennaIFpresetPower()";                
        remoteCallString = oss.str( );
            
        try {
            //const double sendTime = Time::MJD( );
            RxControl_var rxVar = remoteObj()->Rx(rxType);            
            if (CORBA::is_nil(rxVar)) handleNilRx(rxType);
            
            rxVar->setIFPresetPower();
            
            //logSentCommandIfNeeded(remoteCallString, sendTime);
        } catch ( CORBA::SystemException & e ) {
            processException( remoteCallString, e );
        }
    }
}   
   
void 
RxSelectorHandle::antennaIFatten( 
    const antenna::common::RxControl::Type rxType,
    const antenna::common::RxControl::IF_Type polarization,
    const double atten )
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        ostringstream oss;
            
        oss << getStringForRxType( rxType )
            << " Rx::antennaIFatten()";                
        remoteCallString = oss.str( );
            
        try {
            //const double sendTime = Time::MJD( );
            RxControl_var rxVar = remoteObj()->Rx(rxType);            
            if (CORBA::is_nil(rxVar)) handleNilRx(rxType);
            
            rxVar->setIFAtten(atten, polarization);
            
            //logSentCommandIfNeeded(remoteCallString, sendTime);
        } catch ( CORBA::SystemException & e ) {
            processException( remoteCallString, e );
        }
    }
}   
  
 
void
RxSelectorHandle::setRefAtten( const unsigned short atten ) {
    if ( isObjReachable( ) ) {
        // You have to use a rx even though the atten is a common module
        // We arbitrarily choose the 3mm rx
        const RxControl::Type rxType = RxControl::RX3MM;

        string remoteCallString;
        {
            ostringstream oss;            
            oss << getStringForRxType( rxType )
                << " Rx::setLoTerminatorAttenuation(" << atten << ")";                
            remoteCallString = oss.str( );
        }
            
        try {
            const double sendTime = Time::MJD( );
            RxControl_var rxVar = remoteObj( )->Rx( rxType );            
            if ( CORBA::is_nil(rxVar) ) handleNilRx( rxType );
            LOControl_var lo = rxVar->LO();
            lo->setLoTerminatorAttenuation(atten);
            logSentCommandIfNeeded( remoteCallString, sendTime );
        } catch ( CORBA::SystemException & e ) {
            processException( remoteCallString, e );
        }
    }
}

void 
RxSelectorHandle::doIVcurve( const carma::antenna::common::RxControl::Type rx,
                             antenna::common::RxControl::Pol_Type pol, 
                             const IVcurveArgs  ivCurveParams,
                             MonitorSystem * const   monsys, 
                             const int preferredSeqNo ) 
{ 
    if ( !isObjReachable( ) )
        return;

    string remoteCallString;
    {
        ostringstream oss;            
        oss << getStringForRxType( rx )
            << " Rx::doIVcurve("
            << " startVjInMv=" << ivCurveParams.startVjInMv
            << " stopVjInMv=" << ivCurveParams.stopVjInMv
            << " stepVjInMv=" << ivCurveParams.stepVjInMv
            << " deltaInMs=" << ivCurveParams.deltaInMs
            << " doTotalPower=" << ivCurveParams.doTotalPower 
            << " ).";
        remoteCallString = oss.str();
    }

    pendingIvCurveRxType_ = rx;
    pendingIvCurvePolType_ = pol;

    const int seqNo = getAntennaCommon( carmaAntNo_, *monsys ).
        receivers().tuneSeqNum().getValue();
    if (seqNo == preferredSeqNo) {
        nextSequenceNo_ = preferredSeqNo + 10;
    } else {
        nextSequenceNo_ = preferredSeqNo;
    }
            
    try {
        const double sendTime = Time::MJD( );
        RxControl_var rxVar = remoteObj()->Rx(rx);            
        if (CORBA::is_nil(rxVar)) handleNilRx(rx);
        FrontEndControl_var feVar = rxVar->FrontEnd( pol );            
        if (CORBA::is_nil(feVar)) handleNilFrontEnd(rx);           
        feVar->doIVcurve( 
            ivCurveParams.startVjInMv,
            ivCurveParams.stopVjInMv,
            ivCurveParams.stepVjInMv,
            ivCurveParams.deltaInMs,
            ivCurveParams.doTotalPower,
            nextSequenceNo_ ); 
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( CORBA::SystemException & e ) {
        processException( remoteCallString, e );
    }
}

carma::antenna::common::IVCurve * 
RxSelectorHandle::getIVcurve( ) 
{ 
    IVCurve * ivCurve = 0; 

    if ( !isObjReachable( ) )
        return ivCurve;
        
    const string remoteCallString( "Rx::getIVcurve()" );

    try {
        const double sendTime = Time::MJD( );
        RxControl_var rxVar = remoteObj( )->Rx( pendingIvCurveRxType_ );  
        if ( CORBA::is_nil(rxVar) ) handleNilRx( pendingIvCurveRxType_ );

        FrontEndControl_var feVar = rxVar->FrontEnd( pendingIvCurvePolType_ );
        if (CORBA::is_nil(feVar)) handleNilFrontEnd( pendingIvCurveRxType_ );

        ivCurve = feVar->getIVCurve( );

        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( CORBA::SystemException & e ) {
        processException( remoteCallString, e );
    }

    return ivCurve; 
}

bool
RxSelectorHandle::isActionComplete(
    const MonitorSystem & monsys,
    const int             consecutiveErrorLimit )
{
    const bool debug = true;
    const MonitorPointInt & seqNoMP =
        getAntennaCommon(carmaAntNo_, monsys).receivers().tuneSeqNum();

    string label = "Tune" + doName();
    return SubarrayControlImpl::isActionCompleteHelper(
        seqNoMP,
        consecutiveErrorLimit, consecutiveErrorCount_,
        nextSequenceNo_, carmaAntNo_,
        label, debug);
}
 
void 
RxSelectorHandle::vj( 
    antenna::common::RxControl::Type rxType, 
    antenna::common::RxControl::Pol_Type pol, 
    const float vj )
{
    if ( isObjReachable( ) ) {
        ostringstream oss;            
        oss << getStringForRxType( rxType )
            << " Rx::vj("<< "vj=" << vj << ")";                
            
        try {
            RxControl_var rxVar = remoteObj()->Rx(rxType);            
            if (CORBA::is_nil(rxVar)) handleNilRx(rxType);
            FrontEndControl_var feVar = rxVar->FrontEnd( pol );            
            if (CORBA::is_nil(feVar)) handleNilFrontEnd(rxType);           
            feVar->setSISVj(vj);
        } catch (CORBA::SystemException& e) {
            processException(oss.str(), e);
        }
    }
} 

 
void 
RxSelectorHandle::ij( 
    antenna::common::RxControl::Type rxType, 
    antenna::common::RxControl::Pol_Type pol, 
    const float ij )
{
    if ( isObjReachable( ) ) {
        ostringstream oss;            
        oss << getStringForRxType( rxType )
            << " Rx::ij("<< "ij=" <<ij << ")";                
            
        try {
            RxControl_var rxVar = remoteObj()->Rx(rxType);            
            if (CORBA::is_nil(rxVar)) handleNilRx(rxType);
            FrontEndControl_var feVar = rxVar->FrontEnd( pol );            
            if (CORBA::is_nil(feVar)) handleNilFrontEnd(rxType);           
            feVar->setSISIj(ij);
        } catch (CORBA::SystemException& e)  {
            processException(oss.str(), e);
        }
    }
} 

