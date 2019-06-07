/**
 *
 * Carma control downconverter interface implementation.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/control/DownconverterHandle.h"

#include <cstdlib>
#include <string>
#include <sstream>

#include "carma/corba/corba.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/util/Logger.h"

using namespace ::std;

using namespace carma;
using namespace carma::util;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::downconverter;
using namespace log4cpp;


namespace {


string
makeDoName( const bool spectral )
{
    if ( spectral )
        return SLDCCONTROL_NAME;
    else
        return WBDCCONTROL_NAME;
}


MonitorSubsystem &
getSubsystem( const bool            spectral,
              const MonitorSystem & carma )
{
    if ( spectral )
        return carma.sldc();
    else
        return carma.wbdc();
}


MonitorPointBool &
getSystemReachableMp( const bool                        spectral,
                      ControlSubsystemBase::Reachable & reachable )
{
    if ( spectral )
        return reachable.sldcSystem();
    else
        return reachable.wbdcSystem();
}


MonitorPointBool &
getDcReachableMp( const bool                        spectral,
                  ControlSubsystemBase::Reachable & reachable )
{
    if ( spectral )
        return reachable.sldcControl();
    else
        return reachable.wbdcControl();
}


typedef downconverter::System_var RemoteObjVarType;


}  // namespace < anonymous >

/*
namespace carma {
    namespace control {
        // Block downconverter block indicator
        enum blockTypeEnum {
            UPPER_BLOCK,
            LOWER_BLOCK
        } blockType;

    }
}
*/


// public

DownconverterHandle::DownconverterHandle(
    const bool                        spectral,
    MonitorSystem &                   carmaMonitor,
    ControlSubsystemBase::Reachable & reachable ) :
DownconverterSystemRemoteObjHandle(
    makeDoName( spectral ),
    &(getSystemReachableMp( spectral, reachable )),
    &(getSubsystem( spectral, carmaMonitor )),
    &carmaMonitor,
    true,
    false ),
isSpectral_( spectral ),
controlReachableMp_( getDcReachableMp( spectral, reachable ) ),
quadModReachableMp_( reachable.quadMod() ),
noiseSourceReachableMp_( reachable.noiseSource() ),
blockDCReachableMp_( reachable.blockDownconverter() ),
cachedControl_(),
cachedQuadMod_(),
cachedNoiseSource_(),
cachedBlockDC_()
{
    controlReachableMp_.setValue( true );
    quadModReachableMp_.setValue( true );
    noiseSourceReachableMp_.setValue( true );
    blockDCReachableMp_.setValue( true );
}


DownconverterHandle::~DownconverterHandle( )
try {
} catch ( ... ) {
    // just stifle any exception
    
    return;
}


bool
DownconverterHandle::isSpectral( ) const
{
    return isSpectral_;
}


bool
DownconverterHandle::resolveObjRef( )
{
    // Flush/invalidate all the old cached subobjects
    {
        cachedControl_     = ControlType::_var_type();
        cachedQuadMod_     = QuadModType::_var_type();
        cachedNoiseSource_ = NoiseSourceType::_var_type();
        cachedBlockDC_     = BlockDCType::_var_type();
        
        controlReachableMp_.setValue( false );
        quadModReachableMp_.setValue( false );
        noiseSourceReachableMp_.setValue( false );
        blockDCReachableMp_.setValue( false );
    }
    
    // Try to resolve the main DO
    const bool validatedObjRef =
        DownconverterSystemRemoteObjHandle::resolveObjRef();
    
    if ( validatedObjRef ) {
        // Recache the subobjects
        
        RemoteObjVarType remoteObjVar = remoteObj();
    
        {
            const string callString = "DownconverterHandle::cacheControl";
        
            try {
                cachedControl_ = remoteObjVar->GlobalDownconverter();
        
                if ( CORBA::is_nil( cachedControl_ ) == false )
                    controlReachableMp_.setValue( true );
                else
                    programLogError( callString + " - Nil control ref" );
            } catch ( const CORBA::Exception & ex ) {
                processException( callString, ex );
            }
        }
        
        {
            const string callString = "DownconverterHandle::cacheQuadMod";
        
            try {
                cachedQuadMod_ = remoteObjVar->GlobalQuadMod();
                
                if ( CORBA::is_nil( cachedQuadMod_ ) == false )
                    quadModReachableMp_.setValue( true );
            } catch ( const CORBA::Exception & ex ) {
                processException( callString, ex );
            }
        }
        
        {
            const string callString = "DownconverterHandle::cacheNoiseSource";
        
            try {
                cachedNoiseSource_ = remoteObjVar->GlobalNoiseSource();
                
                if ( CORBA::is_nil( cachedNoiseSource_ ) == false )
                    noiseSourceReachableMp_.setValue( true );
            } catch ( const CORBA::Exception & ex ) {
                processException( callString, ex );
            }
        }

        {
            const string callString = "DownconverterHandle::cacheBlockDC";
        
            try {
                cachedBlockDC_ = remoteObjVar->GlobalBlockDownconverter();
                
                if ( CORBA::is_nil( cachedBlockDC_ ) == false )
                    blockDCReachableMp_.setValue( true );
            } catch ( const CORBA::Exception & ex ) {
                processException( callString, ex );
            }
        }
    }
    
    return validatedObjRef;
}


bool
DownconverterHandle::isControlReachable( const bool logIfNotReachable )
{
    if ( isObjReachable( logIfNotReachable ) != true )
        return false;
        
    const bool result = (CORBA::is_nil( cachedControl_ ) == false);
        
    if ( result != true ) {
        const string msg =
            doName() +
            ":Control is not reachable because the sub DO ref is nil";

        CARMA_CPTRACE( Trace::TRACE5, msg );

        if ( logIfNotReachable )
            programLogErrorIfPossible( msg );
    }
        
    return result;
}


bool
DownconverterHandle::isQuadModReachable( const bool logIfNotReachable )
{
    if ( isObjReachable( logIfNotReachable ) != true )
        return false;

    const bool result = (CORBA::is_nil( cachedQuadMod_ ) == false);
        
    if ( result != true ) {
        const string msg =
            doName() +
            ":QuadMod is not reachable because the sub DO ref is nil";

        CARMA_CPTRACE( Trace::TRACE5, msg );

        if ( logIfNotReachable )
            programLogErrorIfPossible( msg );
    }

    return result;
}


bool
DownconverterHandle::isNoiseSourceReachable( const bool logIfNotReachable )
{
    if ( isObjReachable( logIfNotReachable ) != true )
        return false;

    const bool result = (CORBA::is_nil( cachedNoiseSource_ ) == false);
        
    if ( result != true ) {
        const string msg =
            doName() +
            ":NoiseSource is not reachable because the sub DO ref is nil";

        CARMA_CPTRACE( Trace::TRACE5, msg );

        if ( logIfNotReachable )
            programLogErrorIfPossible( msg );
    }

    return result;
}

bool
DownconverterHandle::
isBlockDownconverterReachable( const bool logIfNotReachable )
{
    if ( isObjReachable( logIfNotReachable ) != true )
        return false;

    const bool result = (CORBA::is_nil( cachedBlockDC_ ) == false);
        
    if ( result != true ) {
        const string msg =
            doName() +
            ":Block Downconverter is not reachable because the sub DO ref is nil";

        CARMA_CPTRACE( Trace::TRACE5, msg );

        if ( logIfNotReachable )
            programLogErrorIfPossible( msg );
    }

    return result;
}

bool
DownconverterHandle::isBlockDownconverterReachable( void )
{
    return isBlockDownconverterReachable( getDefaultLogIfNotReachable() );
}


bool
DownconverterHandle::isControlReachable( )
{
    return isControlReachable( getDefaultLogIfNotReachable() );
}


bool
DownconverterHandle::isQuadModReachable( )
{
    return isQuadModReachable( getDefaultLogIfNotReachable() );
}


bool
DownconverterHandle::isNoiseSourceReachable( )
{
    return isNoiseSourceReachable( getDefaultLogIfNotReachable() );
}


void
DownconverterHandle::noiseSource( const bool state )
{
    string ndcText;
    {
        ostringstream oss;
        
        oss << "DownconverterHandle::noiseSource(state="
            << boolalpha << state << ")";
            
        ndcText = oss.str();
    }
    
    const ScopedLogNdc ndc( ndcText );
    
    if ( isNoiseSourceReachable() != true )
        return;
        
    if ( isQuadModReachable() != true )
        return;
    
    {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << "enableNoiseSource(state=" << boolalpha << state << ")";
                
            remoteCallString = oss.str();
        }

        try {
            const double sendTime = Time::MJD();
            
            cachedNoiseSource_->enableNoiseSource(state);
            
            logSentCommandIfNeeded( remoteCallString,
                                    sendTime,
                                    "NoiseSource" );
        }  catch (const CORBA::Exception& ex)  {
            processException (remoteCallString, ex);
        }  catch (...) {
            programLogError( "Couldn't find noise source" );
        }
    }
    
    {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << "enableQuadMod(state=" << boolalpha << state << ")";
                
            remoteCallString = oss.str();
        }

        try {
            const double sendTime = Time::MJD();
            
            cachedQuadMod_->enableQuadMod(state);
            
            logSentCommandIfNeeded( remoteCallString, sendTime, "QuadMod" );
        }  catch (const CORBA::Exception& ex)  {
            processException (remoteCallString, ex);
        } catch (...) {
            programLogError( "Trouble with quadmod" );
        }     
    }
}

void DownconverterHandle::noisePreset( )
{
    const ScopedLogNdc ndc( "DownconverterHandle::noisePreset()" );
    
    if ( isNoiseSourceReachable() != true ) return;

    const string remoteCallString( "setNoiseOutputToPreset()" );

    try {
        const double sendTime = Time::MJD();
        cachedNoiseSource_->setNoiseOutputToPreset();
        logSentCommandIfNeeded(remoteCallString, sendTime, "NoisePreset" );
    }  catch (const CORBA::Exception& ex)  {
        processException (remoteCallString, ex);
    }  catch (...) {
        programLogError( "Couldn't find noise source" );
    }
}

void DownconverterHandle::noiseAtten(const short atten)
{
    string ndcText;
    ostringstream oss;
    oss << "DownconverterHandle::noiseAtten(atten="
        << atten << ")";
    ndcText = oss.str();    
    const ScopedLogNdc ndc( ndcText );
    
    if ( isNoiseSourceReachable() != true ) return;
    string remoteCallString;
    oss.clear();            
    oss << "setNoiseAttenuatino(atten=" << atten << ")";                
    remoteCallString = oss.str();

    try {
        const double sendTime = Time::MJD();
        cachedNoiseSource_->setNoiseAttenuation(atten);
        logSentCommandIfNeeded(remoteCallString, sendTime, "NoiseAtten" );
    }  catch (const CORBA::Exception& ex)  {
        processException (remoteCallString, ex);
    }  catch (...) {
        programLogError( "Couldn't find noise source" );
    }
}

void DownconverterHandle::quadmodAtten(const short atten)
{
    string ndcText;
    ostringstream oss;
    oss << "DownconverterHandle::quadmodAtten(atten="
        << atten << ")";
    ndcText = oss.str();    
    const ScopedLogNdc ndc( ndcText );
    
    if ( isQuadModReachable() != true ) return;
    string remoteCallString;
    oss.clear();            
    oss << "setPoutAttenuation(atten=" << atten << ")";                
    remoteCallString = oss.str();

    try {
        const double sendTime = Time::MJD();
        cachedQuadMod_->setPoutAtten(atten);
        logSentCommandIfNeeded(remoteCallString, sendTime, "QuadModAtten" );
    }  catch (const CORBA::Exception& ex)  {
        processException (remoteCallString, ex);
    }  catch (...) {
        programLogError( "Couldn't find quadmod" );
    }
}

void
DownconverterHandle::psysPreset(vector<short> inputNo, short bandNo)
{
    const ScopedLogNdc ndc( "DownconverterHandle::psysPreset" );
    
    // The downconverters can take up to 2 seconds to complete this command
    // so we put in a sleep to make sure that it completes before returning

    string remoteCallString ;
    try {
        // If all inputs and all bands then we use the quick call
        if ((inputNo[0] == 0) && (bandNo == 0)) {

            if ( isControlReachable() != true ) return;
            const double sendTime = Time::MJD();
            cachedControl_->setPsysPreset();
            logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
            // This is to allow time for the downconverters to complete
            usleep(3*1000000);       
            return;
        }
        
        // Set of antennas and/or bands...
        RemoteObjVarType remoteObjVar = remoteObj();
        
        if ( CORBA::is_nil( remoteObjVar ) == true) return;
        
        for (unsigned int i=0; i < inputNo.size(); i++) {
            const double sendTime = Time::MJD();
            ostringstream o;
            o << "setPsysPreset(input=" << inputNo[i] 
              << ", band=" << bandNo << ")";
            remoteCallString = o.str();
            remoteObjVar->setPsysPreset(inputNo[i], bandNo);
            logSentCommandIfNeeded(remoteCallString, sendTime);
            //Category& l = Program::getLogger();
            //l << Priority::INFO << remoteCallString ;
        }

    } catch (const CORBA::Exception& ex)  {
        processException(remoteCallString, ex);
    } catch ( ... ) {
        logException(remoteCallString, "Trouble - Unknown exception caught" );
    } 
    

}


void
DownconverterHandle::ifoutPreset( )
{
    const ScopedLogNdc ndc( "DownconverterHandle::ifoutPreset" );

    if ( isControlReachable() != true )
        return;

    const string remoteCallString = "setIfOutPreset()";

    try {
        const double sendTime = Time::MJD();
        
        cachedControl_->setIfOutPreset();
        
        logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
    }  catch (const CORBA::Exception& ex)  {
        processException( remoteCallString, ex );
    } catch (...) {
        programLogError( "Caught unknown exception." );
    }
}


void
DownconverterHandle::rfPower( const bool state )
{
    const ScopedLogNdc ndc( "DownconverterHandle::rfPower" );

    if ( isControlReachable() != true )
        return;

    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "enableRfInputAmp(state=" << boolalpha << state << ")";
        
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();
        
        cachedControl_->enableRfInputAmp(state);
        
        logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
    }  catch (const CORBA::Exception& ex)  {
        processException( remoteCallString, ex );
    } catch (...) {
        programLogError( "Caught unknown exception." );
    }
}


void
DownconverterHandle::ifoutLevel( const double level )
{
    const ScopedLogNdc ndc( "DownconverterHandle::ifoutLevel" );

    if ( isControlReachable() != true )
        return;

    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "setIfOut(level=" << level << ")";
        
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();
        
        cachedControl_->setIfOut(level);
        
        logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
    }  catch (const CORBA::Exception& ex)  {
        processException( remoteCallString, ex );
    } catch (...) {
        programLogError( "Caught unknown exception." );
    }
}


void
DownconverterHandle::psysLevel( const double level )
{
    const ScopedLogNdc ndc( "DownconverterHandle::psysLevel" );

    if ( isControlReachable() != true )
        return;

    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "setPsys(level=" << level << ")";
        
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();

        cachedControl_->setPsys(level);

        logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
    } catch ( const CORBA::Exception & ex ) {
        processException( remoteCallString, ex );
    } catch (...) {
        programLogError( "Caught unknown exception." );
    }
}


void 
DownconverterHandle::psysLevel( const double level,
                                const short  inputNo,
                                const short  bandNo )
{
    string ndcText;
    {
        ostringstream oss;
        
        oss.setf( ios::fixed );

        oss << "DownconverterHandle::psysLevel("
            << "level(dB)=" << level
            << ", inputNo=" << inputNo
            << ", bandNo="  << bandNo << ")";

        ndcText = oss.str();
    }
    
    const ScopedLogNdc ndc( ndcText );

    if ( isObjReachable() != true )
        return;
        
    DownconverterControl_var dc;
    string getDcRemoteCallString;
    {
        {
            ostringstream oss;
            
            oss << "Downconverter(inputNo=" << inputNo
                << ", bandNo=" << bandNo << ")";
    
            getDcRemoteCallString = oss.str();
        }

        RemoteObjVarType remoteObjVar = remoteObj();

        try {
            const double sendTime = Time::MJD();
            
            dc = remoteObjVar->Downconverter(inputNo, bandNo);
            
            logSentCommandIfNeeded( getDcRemoteCallString, sendTime );
        } catch ( const CORBA::Exception & ex ) {
            processException( getDcRemoteCallString, ex );
        } catch (...) {
            logException( getDcRemoteCallString,
                          "Trouble - Unknown exception caught" );
        }
    }
    
    {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss.setf( ios::fixed );
    
            oss << "setPsys(level(dB)=" << level << ")";
    
            remoteCallString = oss.str();
        }
        
        try {
            if ( CORBA::is_nil( dc ) )
                throw CARMA_ERROR( "dc is NIL" );
    
            const double sendTime = Time::MJD();
            
            dc->setPsys(level);
            
            logSentCommandIfNeeded( remoteCallString,
                                    sendTime,
                                    getDcRemoteCallString );
        }  catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        } catch (...) {
            logException( remoteCallString, "Trouble - Unknown exception caught" );
        }
    }
}


namespace {

typedef SpectralDownconverterControl SlControlType;

}  // namespace < anonymous >


void
DownconverterHandle::selectSlSideband( 
    const SlControlType::SidebandType sb, 
    const unsigned short              bandNo ) 
{
    string ndcText;
    {
        ostringstream oss;
        
        oss << "DownconverterHandle::selectSlSideband("
            << "sb=" << sb << ", "
            << "bandNo=" << bandNo << ")"; 
        
        ndcText = oss.str();
    }

    const ScopedLogNdc ndc( ndcText );

    if ( isControlReachable() != true )
        return;
        
    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "selectSideband(sb=" << sb << ")";
        
        remoteCallString = oss.str();
    }

    try {
        SlControlType::_var_type slControl =
            SlControlType::_narrow( cachedControl_ );
           
        if ( CORBA::is_nil( slControl ) == false ) {
            const double sendTime = Time::MJD();

            slControl->selectSideband( sb );

            logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
        }
    } catch ( const CORBA::Exception & ex )  {
        processException( remoteCallString, ex );
    } catch ( ... ) {
        logException( remoteCallString, "Trouble - Unknown exception caught" );
    }    
}


void
DownconverterHandle::setSlLOFrequency( const float          freq,
                                       const unsigned short bandNo ) 
{
    string ndcText;
    {
        ostringstream oss;
        
        oss << "DownconverterHandle::setSlLOFrequency("
            << "freq=" << freq << ", "
            << "bandNo=" << bandNo << ")";
            
        ndcText = oss.str();
    }
                                  
    const ScopedLogNdc ndc( ndcText );

    if ( isControlReachable() != true )
        return;
        
    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "setLOFrequency(freq=" << freq << ")";
        
        remoteCallString = oss.str();
    }

    try {
        SlControlType::_var_type slControl =
            SlControlType::_narrow( cachedControl_ );
           
        if ( CORBA::is_nil( slControl ) == false ) {
            const double sendTime = Time::MJD();

            slControl->setLOFrequency( freq );

            logSentCommandIfNeeded( remoteCallString, sendTime, "Control" );
        }
    } catch ( const CORBA::Exception & ex )  {
        processException( remoteCallString, ex );
    } catch ( ... ) {
        logException( remoteCallString, "Trouble - Unknown exception caught" );
    }
}


void
DownconverterHandle::setSlSidebandFrequency( 
    const SlControlType::SidebandType sb, 
    const double                      freq,
    const unsigned short              bandNo ) 
{
    string ndcText;
    {
        ostringstream oss;
        
        oss << "DownconverterHandle::setSlSidebandFrequency("
            << "sb=" << sb << ", "
            << "freq=" << freq << ", "
            << "bandNo=" << bandNo << ")";
            
        ndcText = oss.str();
    }
    
    const ScopedLogNdc ndc( ndcText );
    
    if ( isObjReachable() != true )
        return;
        
    string remoteCallString;
    {
        ostringstream oss;
        
        oss << "selectSidebandFrequency("
            << "sb=" << sb << ", "
            << "freq=" << freq << ", "
            << "bandNo=" << bandNo << ")";
        
        remoteCallString = oss.str();
    }

    try {
        RemoteObjVarType remoteObjVar = remoteObj();
        
        if ( CORBA::is_nil( remoteObjVar ) == false ) {
            const double sendTime = Time::MJD();
            
            remoteObjVar->selectSidebandFrequency( sb, freq, bandNo );
        
            logSentCommandIfNeeded( remoteCallString, sendTime );
        }
    } catch ( const CORBA::Exception & ex )  {
        processException( remoteCallString, ex );
    } catch ( ... ) {
        logException( remoteCallString, "Trouble - Unknown exception caught" );
    }    
}

void
DownconverterHandle::selectFilter(
    const downconverter::DownconverterControl::FilterType filter,
    const unsigned short                                  bandNo )
{
    string ndcText;
    {
        ostringstream oss;            

        oss << "DownconverterHandle::selectFilter("
            << "filterEnum=" << filter << ", "
            << "bandNo=" << bandNo << ")";
            
        ndcText = oss.str();
    }
    
    const ScopedLogNdc ndc( ndcText );

    if ( isObjReachable() != true )
        return;
        
    string remoteCallString;
    {
        ostringstream oss;            

        oss << "selectFilter("
            << "filterEnum=" << filter << ", "
            << "bandNo=" << bandNo << ")";
            
        remoteCallString = oss.str();
    }
    
    try {
        RemoteObjVarType remoteObjVar = remoteObj();
        
        if ( CORBA::is_nil( remoteObjVar ) == false ) {
            const double sendTime = Time::MJD();

            remoteObjVar->selectFilter(filter, bandNo);

            logSentCommandIfNeeded( remoteCallString, sendTime );
        }
    } catch ( const CORBA::Exception & ex ) {
        processException(remoteCallString, ex);
    } catch ( ... ) {
        logException(remoteCallString, "Trouble - Unknown exception caught" );
    }    
}

void
DownconverterHandle::selectFilter(
    const downconverter::DownconverterControl::FilterType filter,
    const unsigned short                                  inputNo,
    const unsigned short                                  bandNo )
{
    string ndcText;
    {
        ostringstream oss;            

        oss << "DownconverterHandle::selectFilter("
            << "filterEnum=" << filter << ", "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << ")";
            
        ndcText = oss.str();
    }
    
    const ScopedLogNdc ndc( ndcText );

    if ( isObjReachable() != true )
        return;
        
    string remoteCallString;
    {
        ostringstream oss;            

        oss << "selectFilter("
            << "filterEnum=" << filter << ", "
            << "inputNo=" << inputNo << ", "
            << "bandNo=" << bandNo << ")";
            
        remoteCallString = oss.str();
    }
    
    try {
        RemoteObjVarType remoteObjVar = remoteObj();
        
        if ( CORBA::is_nil( remoteObjVar ) == false ) {
            const double sendTime = Time::MJD();

            remoteObjVar->SpectralDownconverter(inputNo, bandNo)->selectOutputFilter(filter);

            logSentCommandIfNeeded( remoteCallString, sendTime );
        }
    } catch ( const CORBA::Exception & ex ) {
        processException(remoteCallString, ex);
    } catch ( ... ) {
        logException(remoteCallString, "Trouble - Unknown exception caught" );
    }    
}


void
DownconverterHandle::setBlockAndPolarization( 
        const downconverter::BlockDownconverterControl::Block block,
        const downconverter::BlockDownconverterControl::Polarization polarization,
        const unsigned short bandNo)
{
    const string blkStr = 
        ( block == BlockDownconverterControl::UPPER ) ? 
                 "upper" : "lower";
    const string polStr = 
        ( polarization == BlockDownconverterControl::POLARIZATION_1 ) ? 
                 "POLARIZATION_1" : "POLARIZATION_2";

    string ndcText;
    {
        ostringstream oss;            

        oss << "DownconverterHandle::setBlockAndPolarization("
            << "block=" << blkStr << ", "
            << "pol="   << polStr<< ", "
            << "bandNo=" << bandNo << ")";
            
        ndcText = oss.str();
    }

    const ScopedLogNdc ndc( ndcText );

    if ( isBlockDownconverterReachable() != true )
        return;

    string remoteCallString;
    {
        ostringstream oss;            
        oss << "setBlockAndPolarization("
            << "block=" << blkStr << ", "
            << "pol="   << polStr<< ", "
            << "bandNo=" << bandNo << ")";
            
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();
        cachedBlockDC_->setBlockAndPolarization( block, polarization, bandNo );
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException(remoteCallString, ex);
    } catch ( ... ) {
        logException(remoteCallString, "Trouble - Unknown exception caught" );
    }    
        
}

void DownconverterHandle::setBlockAndPolarization( 
        const downconverter::BlockDownconverterControl::Block block,
        const downconverter::BlockDownconverterControl::Polarization polarization,
        const unsigned short inputNo,
        const unsigned short bandNo)
{
    const string blkStr = 
        ( block == BlockDownconverterControl::UPPER ) ? 
                 "upper" : "lower";
    const string polStr = 
        ( polarization == BlockDownconverterControl::POLARIZATION_1 ) ? 
                 "POLARIZATION_1" : "POLARIZATION_2";

    string ndcText;
    {
        ostringstream oss;            

        oss << "DownconverterHandle::setBlockAndPolarization("
            << "block="   << blkStr  << ", "
            << "pol="     << polStr  << ", "
            << "inputNo=" << inputNo << ", "
	    << "bandNo="  << bandNo  << ")";
            
        ndcText = oss.str();
    }

    const ScopedLogNdc ndc( ndcText );

    if ( isBlockDownconverterReachable() != true )
        return;

    string remoteCallString;
    {
        ostringstream oss;            
        oss << "setBlockAndPolarization("
            << "block="   << blkStr  << ", "
            << "pol="     << polStr  << ", "
            << "inputNo=" << inputNo << ", "
	    << "bandNo="  << bandNo  << ")";
            
        remoteCallString = oss.str();
    }

    try {
        const double sendTime = Time::MJD();
	remoteObj()->BlockDownconverter(inputNo)->setBlockAndPolarization( block, polarization, bandNo );
        logSentCommandIfNeeded( remoteCallString, sendTime );
    } catch ( const CORBA::Exception & ex ) {
        processException(remoteCallString, ex);
    } catch ( ... ) {
        logException(remoteCallString, "Trouble - Unknown exception caught" );
    }    
        
}
