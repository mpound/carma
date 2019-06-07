#include "carma/control/PipelineHandle.h"

#include "carma/control/SubarrayControlImpl.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Time.h"

using namespace ::std;
using namespace ::carma;
using namespace ::carma::control;
using namespace ::carma::monitor;
using namespace ::carma::pipeline;
using namespace ::carma::util;


PipelineHandle::PipelineHandle(
    const string&           DOname,
    ControlSubsystem&       controlSubsystem,
    MonitorSystem&          monitorSystem,
    MonitorPointBool&       reachableMP,
    MonitorSubsystem&       pipelineMonitorSystem,
    const int               firstAstroBandNo,
    const string&           msgPrefix) :
        PipelineControlRemoteObjHandle(
                            DOname,
                            &reachableMP,
                            &pipelineMonitorSystem,
                            &monitorSystem,
                            true,     // LogIfNotReachable
                            false ),  // logSentCommands

controlSubsystem_(controlSubsystem),
monitorSystem_(monitorSystem),
firstAstroBandNo_(firstAstroBandNo),
msgPrefix_(msgPrefix),
nextSequenceNo_(0),
consecutiveErrors_(0),
errLimit_(24)
{
}


PipelineHandle::~PipelineHandle( )
try {
} catch ( ... ) {
    // Just stifle any exception    
    return;
}

void
PipelineHandle::integrate(const double intTime, 
                          const CORBA::Long numInts,
                          const double gap,
                          const CORBA::Boolean science,
                          int preferredSeqNo )
{
    if ( !isObjReachable( ) ) return;

    string remoteCallString;
    {
        ostringstream oss;
        oss << msgPrefix_ << fixed << "PipelineHandle::integrate("
            << "intTime =" << setprecision(1) << intTime << ", "
            << "numInts =" << numInts << ", "
            << "gap=" << setprecision(1) << gap << ", "
            << "science=" << boolalpha << science << ", " 
            << "seqNo =" << preferredSeqNo << ")";
        remoteCallString = oss.str();
    }

    // Check that current sequence number in the monitor system isn't the 
    // same as the one just passed to us.  If it is, set the preferred to 
    // 10 greater than the current sequence number from the monitor system. 
    // This works but causes this logic to trigger again every 10 commands.
    // In addition, this logic is duplicated everywhere and can probably be
    // consolidated into something which works more generically and efficiently
    // at the SubarrayControlImpl level.
    monitorSystem_.readNewest();
    const MonitorPointInt& seqNoMp = getSeqNoMP();

    if (seqNoMp.isValid() && (seqNoMp.getValue() == preferredSeqNo)) {
        preferredSeqNo += 10;
    }    
    nextSequenceNo_ = preferredSeqNo;

    try {
        const double sendTime = Time::MJD( );
        remoteObj( )->startIntegration(intTime, numInts, 
            gap, science, preferredSeqNo);
        
        logSentCommandIfNeeded( remoteCallString, sendTime );
    }  catch ( const CORBA::Exception & ex )  {
        processException( remoteCallString, ex );
    }
}
        
bool 
PipelineHandle::isIntegrationComplete(const monitor::MonitorSystem & monsys,
                                      const int monErrLimit)
{
    // Non-zero limit triggers resetting - do this first in order to assure
    // that errLimit is initialized.
    if ( monErrLimit > 0 ) {
        errLimit_ = monErrLimit;
        consecutiveErrors_ = 0;
    }

    const MonitorPointInt& seqNumMp = getSeqNoMP();

    if ( seqNumMp.isValid() ) {
        consecutiveErrors_ = 0;
        return ( seqNumMp.getValue() == nextSequenceNo_ );
    }

    // Invalid MP - do the Invalid MP dance.
    ++consecutiveErrors_;
    
    if (consecutiveErrors_ >= errLimit_) {
        ostringstream o;
        o << "Wait encountered " << errLimit_ << " consecutive invalid "
          << msgPrefix_ << "pipeline monitor data frames.";
        throw CARMA_EXCEPTION(InvalidMonitorDataException, o.str().c_str());
    }

    return false;
}

void
PipelineHandle::resetTsys(SeqShort carmaAntNoSeq) 
{
    if ( !isObjReachable( ) ) return;

    string remoteCallString;
    {
        ostringstream oss;
        oss << msgPrefix_ << "PipelineHandle::resetTsys( "
            << "carmaAntNoSeq=" 
            << SubarrayControlImpl::getStringForCarmaAntNoSeq( carmaAntNoSeq )
            << " )";
        remoteCallString = oss.str();
    }
    
    try {
        const double sendTime = Time::MJD();

        remoteObj( )->resetTsys( carmaAntNoSeq );
        
        logSentCommand( remoteCallString, sendTime );
    }  catch ( const CORBA::Exception & ex )  {
        processException( remoteCallString, ex );
    }
}

void
PipelineHandle::decimate(const bool value, const unsigned short astroBandNo) 
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        ostringstream oss;            
        oss << msgPrefix_ << "PipelineControl::decimate(" 
            << boolalpha << value 
            << ", astroBandNo=" << astroBandNo << ")";            
        remoteCallString = oss.str( );
        
        try {
            const double sendTime = Time::MJD( );           
            remoteObj( )->decimate(value, astroBandNo);            
            logSentCommandIfNeeded(remoteCallString, sendTime);
        }  catch ( const CORBA::Exception & ex )  {
            processException( remoteCallString, ex );
        }
    }
    // Put requested value into control monitor system independent of
    // whether the command was sent to the pipeline
    if (astroBandNo == 0) {
        vector<CBP*>::const_iterator b;
        for (b=CBPvector_.begin(); b != CBPvector_.end(); b++) {
            (*b)->decimation().setValue(value);
        }
    }
    else {
        CBPvector_.at(astroBandNo-firstAstroBandNo_)->decimation().
            setValue(value);
    }
}

void
PipelineHandle::keepEndChannels(const bool value, 
                                const unsigned short astroBandNo)
{
    if ( isObjReachable( ) ) {
        string remoteCallString;
        {
            ostringstream oss;            
            oss << msgPrefix_ << "PipelineControl::keepEndChannels(" 
                << boolalpha << value 
                << ", astroBandNo=" << astroBandNo << ")";                        
            remoteCallString = oss.str( );
        }
        
        try {
            const double sendTime = Time::MJD( );            
            remoteObj( )->keepEndChannels(value, astroBandNo);            
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }  catch ( const CORBA::Exception & ex )  {
            processException( remoteCallString, ex );
        }
    }
    // Put requested value into control monitor system independent of
    // whether the command was sent to the pipeline
    if (astroBandNo == 0) {
        vector<CBP*>::const_iterator b;
        for (b=CBPvector_.begin(); b != CBPvector_.end(); b++) {
            (*b)->keepEndChans().setValue(value);
        }
    }
    else {
        CBPvector_.at(astroBandNo-firstAstroBandNo_)->keepEndChans().
            setValue(value);
    }
}

void
PipelineHandle::setDecimationMode(const DecimationMode decimationMode,
                                  const unsigned short astroBandNo) 
{
    switch (decimationMode) {
        case DECIMATION_ON:
            decimate(true, astroBandNo);
            // Decimation automatically throws away end channels
            keepEndChannels(false, astroBandNo);
            break;

        case DECIMATION_OFF_KEEP_END_CHANNELS:
            decimate(false, astroBandNo);
            keepEndChannels(true, astroBandNo);
            break;

        case DECIMATION_OFF_DROP_END_CHANNELS:
            decimate(false, astroBandNo);
            keepEndChannels(false, astroBandNo) ;
            break;
    }
}

// ===========================================================================
// ************************ Specific Pipeline Handles ************************
SLPipelineHandle::SLPipelineHandle(
            ControlSubsystem&                controlSubsystem,
            MonitorSystem&                   monitorSystem,
            ControlSubsystemBase::Reachable& reachableContainer)
    : PipelineHandle(
            SPECTRAL_PIPELINE_NAME, 
            controlSubsystem,
            monitorSystem,
            reachableContainer.slPipeline(),
            monitorSystem.slPipeline(),
            1,
            "SL")
{
    const int nBands = 8;
    for (int b=0; b<nBands; b++) {
        CBPvector_.push_back(&(controlSubsystem_.spectralLineCorrelator().
            slcBand(b).controlBandPoints()));
    }
}            
// Virtual helpers
MonitorPointInt& SLPipelineHandle::getSeqNoMP()
{
    return monitorSystem_.slPipeline().
            integratorStageContainer().integratorStage().currentSeqNo();
}

WBPipelineHandle::WBPipelineHandle(
            ControlSubsystem&                controlSubsystem,
            MonitorSystem&                   monitorSystem,
            ControlSubsystemBase::Reachable& reachableContainer)
    : PipelineHandle(
            WIDEBAND_PIPELINE_NAME,
            controlSubsystem,
            monitorSystem,
            reachableContainer.wbPipeline(),
            monitorSystem.wbPipeline(),
            9,
            "WB")
{
    const int nBands = 16;
    for (int b=0; b<nBands; b++) {
        CBPvector_.push_back(&(controlSubsystem_.widebandCorrelator().
            wbcBand(b).controlBandPoints()));
    }
}            
// Virtual helpers
MonitorPointInt& WBPipelineHandle::getSeqNoMP()
{
    return monitorSystem_.wbPipeline().
            integratorStageContainer().integratorStage().currentSeqNo();
}
void 
WBPipelineHandle::decimate(const bool dec, const unsigned short astroBandNo)
{
    ostringstream oss;            
    oss << msgPrefix_ << "PipelineControl::decimate(" 
        << boolalpha << dec 
        << ", astroBandNo=" << astroBandNo 
        << ") command skipped; no decimation on WB pipeline";            
    programLogInfoIfPossible(oss.str( ));
}

C3gMax8PipelineHandle::C3gMax8PipelineHandle(
            ControlSubsystem&                controlSubsystem,
            MonitorSystem&                   monitorSystem,
            ControlSubsystemBase::Reachable& reachableContainer)
    : PipelineHandle(
            C3GMAX8_PIPELINE_NAME,
            controlSubsystem,
            monitorSystem,
            reachableContainer.c3gMax8Pipeline(),
            monitorSystem.c3gMax8Pipeline(),
            33,
            "C3gMax8")
{
    const int nBands = 8;
    for (int b=0; b<nBands; b++) {
        CBPvector_.push_back(&(controlSubsystem_.c3gMax8Correlator().
            c3gMax8Band(b).controlBandPoints()));
    }
}            
// Virtual helper
MonitorPointInt& C3gMax8PipelineHandle::getSeqNoMP()
{
    return monitorSystem_.c3gMax8Pipeline().
            integratorStageContainer().integratorStage().currentSeqNo();
}

C3gMax23PipelineHandle::C3gMax23PipelineHandle(
            ControlSubsystem&                controlSubsystem,
            MonitorSystem&                   monitorSystem,
            ControlSubsystemBase::Reachable& reachableContainer)
    : PipelineHandle(
            C3GMAX23_PIPELINE_NAME,
            controlSubsystem,
            monitorSystem,
            reachableContainer.c3gMax23Pipeline(),
            monitorSystem.c3gMax23Pipeline(),
            25,
            "C3gMax23")
{
    const int nBands = 8;
    for (int b=0; b<nBands; b++) {
        CBPvector_.push_back(&(controlSubsystem_.c3gMax23Correlator().
            c3gMax23Band(b).controlBandPoints()));
    }
}            
// Virtual helper
MonitorPointInt& C3gMax23PipelineHandle::getSeqNoMP()
{
    return monitorSystem_.c3gMax23Pipeline().
            integratorStageContainer().integratorStage().currentSeqNo();
}
