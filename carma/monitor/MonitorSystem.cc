/**
 *
 * Implementation for the monitor system class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorSystem.cc,v 1.118 2014/06/20 15:55:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorSystem.h"

#include <string>
#include <iomanip>
#include <iostream>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/monitor/SubsystemFrameHeader.h"
#include "carma/monitor/SystemThresholdFrame.h"
#include "carma/monitor/SystemFrameBuffer.h"

#include "carma/monitor/AstroSubsystem.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/C3gMax8PipelineSubsystemExt.h"
#include "carma/monitor/C3gMax23PipelineSubsystemExt.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/monitor/CentralIfSubsystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/monitor/FaultSubsystem.h"
#include "carma/monitor/ImrSubsystem.h"
#include "carma/monitor/AlarmSubsystem.h"
#include "carma/monitor/LineLengthSubsystem.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/LoRefSubsystem.h"
#include "carma/monitor/MasterClockSubsystem.h"
#include "carma/monitor/OpacityMonitorSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/ProjectDatabaseManagerSubsystem.h"
#include "carma/monitor/PhaseMonitorSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/monitor/SystemStatus.h"
#include "carma/monitor/SzaSubsystem.h"
#include "carma/monitor/TestSubsystemExt.h"
#include "carma/monitor/VlbiSubsystem.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/WbdcSubsystem.h"
#include "carma/monitor/WbPipelineSubsystemExt.h"
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/monitor/WbDataflowSubsystem.h"
#include "carma/monitor/SlDataflowSubsystem.h"
#include "carma/monitor/C3gDataflowSubsystem.h"
#include "carma/monitor/WbRemapperSubsystem.h"
#include "carma/monitor/SlRemapperSubsystem.h"
#include "carma/monitor/C3gRemapperSubsystem.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {

const int kFinalCMSQueueDepth = 30;

const float fudgeFactor = 1.08;

// These functions calculate the
// maximum values for the # of CARMA subsystems,
// maximum value of total number of monitor points and
// maximum value of total number bof samples across 
// all subsystems. This information is required
// at the time constructing the CARMA MonitorSystem
// object for sizing the required storage.

// Numbers based on default values from Steve Scott's estimates in NumberMPs.pdf
// Email sent to ccwg on 01/28/04
// Numbers changed to accomodate cumulative maximum # of monitor points
// and maximum # of samples in mpml files.
long getMaxCarmaSubsystems( ) {
        return 
        AstroSubsystem::COUNT + 
        C3gMax8PipelineSubsystem::COUNT + 
        C3gMax23PipelineSubsystem::COUNT + 
        CentralIfSubsystem::COUNT +
        ControlSubsystemBase::COUNT +
        DataflowSubsystem::COUNT +
        DelayEngineSubsystem::COUNT +
        FaultSubsystem::COUNT +
        ImrSubsystem::COUNT +
        LineLengthSubsystem::COUNT +
        LoberotatorSubsystem::COUNT +
        LoRefSubsystem::COUNT +
        MasterClockSubsystem::COUNT +
        OpacityMonitorSubsystem::COUNT +
        PhaseMonitorSubsystem::COUNT +
        ProjectDatabaseManagerSubsystem::COUNT +
        SignalPathSubsystem::COUNT +
        SldcSubsystem::COUNT +
        SlPipelineSubsystem::COUNT +
        SystemStatusSubsystem::COUNT +
        TestSubsystem::COUNT +
        WbdcSubsystem::COUNT +
        WbPipelineSubsystem::COUNT +
        WeatherSubsystem::COUNT +
        BimaSubsystem::COUNT +
        OvroSubsystem::COUNT +
        SzaSubsystem::COUNT +
        CarmaSlcBandSubsystem::COUNT +
        Carma3GBandSubsystem::COUNT +
        WbcBandSubsystem::COUNT +
        VlbiSubsystem::COUNT +
        AlarmSubsystem::COUNT +
        WbDataflowSubsystem::COUNT + 
        SlDataflowSubsystem::COUNT +
        C3gDataflowSubsystem::COUNT +
        WbRemapperSubsystem::COUNT + 
        SlRemapperSubsystem::COUNT +
        C3gRemapperSubsystem::COUNT 
        ; 
}

long  getMaxCarmaMonitorPoints( ) {
    return static_cast< long >( fudgeFactor * 
     (  AstroSubsystem::COUNT * AstroSubsystem::MAX_MONITOR_POINTS +
        C3gMax8PipelineSubsystem::COUNT * C3gMax8PipelineSubsystem::MAX_MONITOR_POINTS + 
        C3gMax23PipelineSubsystem::COUNT * C3gMax23PipelineSubsystem::MAX_MONITOR_POINTS +
        CentralIfSubsystem::COUNT * CentralIfSubsystem::MAX_MONITOR_POINTS +
        ControlSubsystemBase::COUNT * ControlSubsystemBase::MAX_MONITOR_POINTS +
        DataflowSubsystem::COUNT * DataflowSubsystem::MAX_MONITOR_POINTS +
        DelayEngineSubsystem::COUNT * DelayEngineSubsystem::MAX_MONITOR_POINTS +
        FaultSubsystem::COUNT * FaultSubsystem::MAX_MONITOR_POINTS +
        ImrSubsystem::COUNT * ImrSubsystem::MAX_MONITOR_POINTS +
        AlarmSubsystem::COUNT * AlarmSubsystem::MAX_MONITOR_POINTS +
        LineLengthSubsystem::COUNT * LineLengthSubsystem::MAX_MONITOR_POINTS +
        LoberotatorSubsystem::COUNT * LoberotatorSubsystem::MAX_MONITOR_POINTS +
        LoRefSubsystem::COUNT * LoRefSubsystem::MAX_MONITOR_POINTS +
        MasterClockSubsystem::COUNT * MasterClockSubsystem::MAX_MONITOR_POINTS +
        OpacityMonitorSubsystem::COUNT * OpacityMonitorSubsystem::MAX_MONITOR_POINTS +
        PhaseMonitorSubsystem::COUNT * PhaseMonitorSubsystem::MAX_MONITOR_POINTS +
        ProjectDatabaseManagerSubsystem::COUNT * ProjectDatabaseManagerSubsystem::MAX_MONITOR_POINTS +
        SignalPathSubsystem::COUNT * SignalPathSubsystem::MAX_MONITOR_POINTS +
        SldcSubsystem::COUNT * SldcSubsystem::MAX_MONITOR_POINTS +
        SlPipelineSubsystem::COUNT * SlPipelineSubsystem::MAX_MONITOR_POINTS +
        SystemStatusSubsystem::COUNT * SystemStatusSubsystem::MAX_MONITOR_POINTS +
        TestSubsystem::COUNT * TestSubsystem::MAX_MONITOR_POINTS +
        WbdcSubsystem::COUNT * WbdcSubsystem::MAX_MONITOR_POINTS +
        WbPipelineSubsystem::COUNT * WbPipelineSubsystem::MAX_MONITOR_POINTS +
        WeatherSubsystem::COUNT * WeatherSubsystem::MAX_MONITOR_POINTS +
        BimaSubsystem::COUNT * BimaSubsystem::MAX_MONITOR_POINTS +
        OvroSubsystem::COUNT * OvroSubsystem::MAX_MONITOR_POINTS +
        SzaSubsystem::COUNT * SzaSubsystem::MAX_MONITOR_POINTS +
        CarmaSlcBandSubsystem::COUNT * CarmaSlcBandSubsystem::MAX_MONITOR_POINTS +
        Carma3GBandSubsystem::COUNT * Carma3GBandSubsystem::MAX_MONITOR_POINTS +
        VlbiSubsystem::COUNT*VlbiSubsystem::MAX_MONITOR_POINTS +
        WbcBandSubsystem::COUNT * WbcBandSubsystem::MAX_MONITOR_POINTS  +
        WbDataflowSubsystem::COUNT + WbDataflowSubsystem::MAX_MONITOR_POINTS +
        SlDataflowSubsystem::COUNT + SlDataflowSubsystem::MAX_MONITOR_POINTS +
        C3gDataflowSubsystem::COUNT + C3gDataflowSubsystem::MAX_MONITOR_POINTS +
        WbRemapperSubsystem::COUNT + WbRemapperSubsystem::MAX_MONITOR_POINTS +
        SlRemapperSubsystem::COUNT + SlRemapperSubsystem::MAX_MONITOR_POINTS +
        C3gRemapperSubsystem::COUNT + C3gRemapperSubsystem::MAX_MONITOR_POINTS
     )
    );
}

long  getMaxCarmaSamples( ) {
    return static_cast< long >( fudgeFactor * 
     (  AstroSubsystem::COUNT * AstroSubsystem::MAX_SAMPLES + 
        C3gMax8PipelineSubsystem::COUNT * C3gMax8PipelineSubsystem::MAX_SAMPLES + 
        C3gMax23PipelineSubsystem::COUNT * C3gMax23PipelineSubsystem::MAX_SAMPLES +
        CentralIfSubsystem::COUNT * CentralIfSubsystem::MAX_SAMPLES +
        ControlSubsystemBase::COUNT * ControlSubsystemBase::MAX_SAMPLES +
        DataflowSubsystem::COUNT * DataflowSubsystem::MAX_SAMPLES +
        DelayEngineSubsystem::COUNT * DelayEngineSubsystem::MAX_SAMPLES +
        FaultSubsystem::COUNT * FaultSubsystem::MAX_SAMPLES +
        ImrSubsystem::COUNT * ImrSubsystem::MAX_SAMPLES +
        AlarmSubsystem::COUNT * AlarmSubsystem::MAX_SAMPLES +
        LineLengthSubsystem::COUNT * LineLengthSubsystem::MAX_SAMPLES +
        LoberotatorSubsystem::COUNT * LoberotatorSubsystem::MAX_SAMPLES +
        LoRefSubsystem::COUNT * LoRefSubsystem::MAX_SAMPLES +
        MasterClockSubsystem::COUNT * MasterClockSubsystem::MAX_SAMPLES +
        OpacityMonitorSubsystem::COUNT * OpacityMonitorSubsystem::MAX_SAMPLES +
        PhaseMonitorSubsystem::COUNT * PhaseMonitorSubsystem::MAX_SAMPLES +
        ProjectDatabaseManagerSubsystem::COUNT * ProjectDatabaseManagerSubsystem::MAX_SAMPLES +
        SignalPathSubsystem::COUNT * SignalPathSubsystem::MAX_SAMPLES +
        SldcSubsystem::COUNT * SldcSubsystem::MAX_SAMPLES +
        SlPipelineSubsystem::COUNT * SlPipelineSubsystem::MAX_SAMPLES +
        SystemStatusSubsystem::COUNT * SystemStatusSubsystem::MAX_SAMPLES +
        TestSubsystem::COUNT * TestSubsystem::MAX_SAMPLES +
        WbdcSubsystem::COUNT * WbdcSubsystem::MAX_SAMPLES +
        WbPipelineSubsystem::COUNT * WbPipelineSubsystem::MAX_SAMPLES +
        WeatherSubsystem::COUNT * WeatherSubsystem::MAX_SAMPLES +
        BimaSubsystem::COUNT * BimaSubsystem::MAX_SAMPLES +
        OvroSubsystem::COUNT * OvroSubsystem::MAX_SAMPLES +
        SzaSubsystem::COUNT * SzaSubsystem::MAX_SAMPLES +
        CarmaSlcBandSubsystem::COUNT * CarmaSlcBandSubsystem::MAX_SAMPLES +
        Carma3GBandSubsystem::COUNT * Carma3GBandSubsystem::MAX_SAMPLES +
        VlbiSubsystem::COUNT*VlbiSubsystem::MAX_SAMPLES +
        WbcBandSubsystem::COUNT * WbcBandSubsystem::MAX_SAMPLES +
        WbDataflowSubsystem::COUNT + WbDataflowSubsystem::MAX_SAMPLES +
        SlDataflowSubsystem::COUNT + SlDataflowSubsystem::MAX_SAMPLES +
        C3gDataflowSubsystem::COUNT + C3gDataflowSubsystem::MAX_SAMPLES +
        WbRemapperSubsystem::COUNT + WbRemapperSubsystem::MAX_SAMPLES +
        SlRemapperSubsystem::COUNT + SlRemapperSubsystem::MAX_SAMPLES +
        C3gRemapperSubsystem::COUNT + C3gRemapperSubsystem::MAX_SAMPLES 
       ) 
      );
}


} // namespace <unnamed>



CarmaMonitorSystem::CarmaMonitorSystem( ) :
    MonitorSystem( "Carma", 
                   getMaxCarmaSubsystems( ), 
                   getMaxCarmaMonitorPoints( ),
                   getMaxCarmaSamples( ) )
{

}

CarmaMonitorSystem::CarmaMonitorSystem( const string name,
                                        const int queueDepth ) :
    MonitorSystem( name, 
                   getMaxCarmaSubsystems( ), 
                   getMaxCarmaMonitorPoints( ),
                   getMaxCarmaSamples( ),
                   queueDepth )
{

}

CarmaMonitorSystem::CarmaMonitorSystem(const RawCarmaMonitorSystem& raw)
        :  MonitorSystem("Carma", 
                            raw.systemFrameBuffer().getMaxNumSubsystemFrames(),
                            raw.systemFrameBuffer().getMaxTotalMonitorPoints(), 
                            raw.systemFrameBuffer().getMaxTotalSamples()) 
{
    synchronize (raw);
}

RawCarmaMonitorSystem::RawCarmaMonitorSystem( ) :
    CarmaMonitorSystem( "RawCarma", 
                        SubsystemFrameBuffer::kDefaultQueueDepth )
{

}


FinalCarmaMonitorSystem::FinalCarmaMonitorSystem( ) :
    CarmaMonitorSystem( "FinalCarma", 
                        kFinalCMSQueueDepth ) 
{

}


MonitorSystem::MonitorSystem(const string& systemName, long numSubsystems, 
                     long totalNumMonitorPoints, long totalNumSamples,
                     int queueDepth):
    MonitorSystemContainer(systemName), 

        systemFrameBuffer_(SystemFrameBuffer::getSystemFrameBuffer(systemName, 
                                   numSubsystems,
                                   totalNumMonitorPoints,
                                   totalNumSamples,
                                   queueDepth)),

        astro_(         *new AstroSubsystem(&systemFrameBuffer_)),
        c3gMax8Pipeline_( *new C3gMax8PipelineSubsystem(&systemFrameBuffer_)),
        c3gMax23Pipeline_(*new C3gMax23PipelineSubsystem(&systemFrameBuffer_)),
        centralIf_(     *new CentralIfSubsystem(&systemFrameBuffer_)),
        control_(       *new ControlSubsystem(&systemFrameBuffer_)),
        dataflow_(      *new DataflowSubsystem(&systemFrameBuffer_)),
        delayEngine_(   *new DelayEngineSubsystem(&systemFrameBuffer_)),
        imr_(           *new ImrSubsystem(&systemFrameBuffer_)),
        alarm_(         *new AlarmSubsystem(&systemFrameBuffer_)),
        lineLength_(    *new LineLengthSubsystem(&systemFrameBuffer_)),
        loberotator_(   *new LoberotatorSubsystem(&systemFrameBuffer_)),
        loRef_(         *new LoRefSubsystem(&systemFrameBuffer_)),
        masterclock_(   *new MasterClockSubsystem(&systemFrameBuffer_)),
        opacityMonitor_(*new OpacityMonitorSubsystem(&systemFrameBuffer_)),
        phaseMonitor_(  *new PhaseMonitorSubsystem(&systemFrameBuffer_)),
        projectDatabaseManager_(
                        *new ProjectDatabaseManagerSubsystem(&systemFrameBuffer_)),
        signalPath_(    *new SignalPathSubsystem(&systemFrameBuffer_)),
        sldc_(          *new SldcSubsystem(&systemFrameBuffer_)),
        slPipeline_(    *new SlPipelineSubsystem(&systemFrameBuffer_)),
        systemStatus_(  *new SystemStatusSubsystem(&systemFrameBuffer_)),
        test_(          *new TestSubsystem(&systemFrameBuffer_)),
        vlbi_(          *new VlbiSubsystem(&systemFrameBuffer_)),
        wbdc_(          *new WbdcSubsystem(&systemFrameBuffer_)),
        wbPipeline_(    *new WbPipelineSubsystem(&systemFrameBuffer_)),
        weather_(       *new WeatherSubsystem(&systemFrameBuffer_)),
        fault_(         *new FaultSubsystem( &systemFrameBuffer_ ) )
{

    CARMA_CPTRACE(Trace::TRACE6, "Start of MonitorSystem constructor");    
        
    bimas_.reserve( BimaSubsystem::COUNT );
    for ( int i = 0; i < BimaSubsystem::COUNT; ++i ) {
        BimaSubsystem * const bimaSubsystem =
            new BimaSubsystem(i+1, &systemFrameBuffer_);

        bimas_.push_back( bimaSubsystem );
        
        add( *bimaSubsystem );
    }

    ovros_.reserve( OvroSubsystem::COUNT );
    for ( int i = 0; i < OvroSubsystem::COUNT; ++i ) {
        OvroSubsystem * const ovroSubsystem =
            new OvroSubsystem(i+1, &systemFrameBuffer_);

        ovros_.push_back( ovroSubsystem );
        
        add( *ovroSubsystem );
    }

    szas_.reserve( SzaSubsystem::COUNT );
    for ( int i = 0; i < SzaSubsystem::COUNT; ++i ) {
        SzaSubsystem * const szaSubsystem =
            new SzaSubsystem(i+1, &systemFrameBuffer_);

        szas_.push_back( szaSubsystem );
        
        add( *szaSubsystem );
    }

    carmaSlcBands_.reserve( CarmaSlcBandSubsystem::COUNT );
    for ( int i = 0; i < CarmaSlcBandSubsystem::COUNT; ++i ) {
        CarmaSlcBandSubsystem * const carmaSlcBandSubsystem =
            new CarmaSlcBandSubsystem(i+1, &systemFrameBuffer_);

        carmaSlcBands_.push_back( carmaSlcBandSubsystem );        
        add( *carmaSlcBandSubsystem );
    }

    carma3gBands_.reserve( Carma3GBandSubsystem::COUNT );
    for ( int i = 0; i < Carma3GBandSubsystem::COUNT; ++i ) {
        Carma3GBandSubsystem * const carma3gSubsystem =
            new Carma3GBandSubsystem(i+1, &systemFrameBuffer_);

        carma3gBands_.push_back( carma3gSubsystem );        
        add( *carma3gSubsystem );
    }

    wbcBands_.reserve( WbcBandSubsystem::COUNT );
    for ( int i = 0; i < WbcBandSubsystem::COUNT; ++i ) {
        WbcBandSubsystem * const wbcBandSubsystem =
            new WbcBandSubsystem(i+1, &systemFrameBuffer_);
            
        wbcBands_.push_back( wbcBandSubsystem );
        add( *wbcBandSubsystem );
    }
   
    add(astro_);
    add(c3gMax8Pipeline_);
    add(c3gMax23Pipeline_);
    add(centralIf_);
    add(control_);
    add(dataflow_);
    add(delayEngine_);
    add(imr_);
    add(alarm_);
    add(lineLength_);
    add(loberotator_);
    add(loRef_);
    add(masterclock_);
    add(opacityMonitor_);
    add(phaseMonitor_);
    add(projectDatabaseManager_);
    add(signalPath_);
    add(sldc_);
    add(slPipeline_);
    add(systemStatus_);
    add(test_);
    add(vlbi_);
    add(wbdc_);
    add(wbPipeline_);
    add(weather_);
    add(fault_);

    wbDataflow_.reserve(WbDataflowSubsystem::COUNT);
    for ( int i = 0 ; i < WbDataflowSubsystem::COUNT; ++i ) {
        WbDataflowSubsystem * const w =
            new WbDataflowSubsystem(i+1,&systemFrameBuffer_);
        wbDataflow_.push_back( w );
        add( *w );
    }

    slDataflow_.reserve(SlDataflowSubsystem::COUNT);
    for ( int i = 0 ; i < SlDataflowSubsystem::COUNT; ++i ) {
        SlDataflowSubsystem * const w =
            new SlDataflowSubsystem(i+1,&systemFrameBuffer_);
        slDataflow_.push_back( w );
        add( *w );
    }

    c3gDataflow_.reserve(C3gDataflowSubsystem::COUNT);
    for ( int i = 0 ; i < C3gDataflowSubsystem::COUNT; ++i ) {
        C3gDataflowSubsystem * const w =
            new C3gDataflowSubsystem(i+1,&systemFrameBuffer_);
        c3gDataflow_.push_back( w );
        add( *w );
    }

    wbRemapper_.reserve(WbRemapperSubsystem::COUNT);
    for ( int i = 0 ; i < WbRemapperSubsystem::COUNT; ++i ) {
        WbRemapperSubsystem * const w =
            new WbRemapperSubsystem(i+1,&systemFrameBuffer_);
        wbRemapper_.push_back( w );
        add( *w );
    }

    slRemapper_.reserve(SlRemapperSubsystem::COUNT);
    for ( int i = 0 ; i < SlRemapperSubsystem::COUNT; ++i ) {
        SlRemapperSubsystem * const w =
            new SlRemapperSubsystem(i+1,&systemFrameBuffer_);
        slRemapper_.push_back( w );
        add( *w );
    }

    c3gRemapper_.reserve(C3gRemapperSubsystem::COUNT);
    for ( int i = 0 ; i < C3gRemapperSubsystem::COUNT; ++i ) {
        C3gRemapperSubsystem * const w =
            new C3gRemapperSubsystem(i+1,&systemFrameBuffer_);
        c3gRemapper_.push_back( w );
        add( *w );
    }
    
    dbms::TagIDAuthority::getAuthority().logNotableOnTheFlyCreationsThusFarAndInTheFuture();

    CARMA_CPTRACE(Trace::TRACE6, "End of MonitorSystem constructor");    
}


MonitorSystem::~MonitorSystem()
try {
    if (debug_) cout << "MonitorSystem d'tor" << endl;
    
    for ( size_t i = 0; i < bimas_.size(); ++i )
        delete bimas_.at( i );

    for ( size_t i = 0; i < ovros_.size(); ++i )
        delete ovros_.at( i );
    
    for ( size_t i = 0; i < szas_.size(); ++i )
        delete szas_.at( i );
    
    /*
    for ( size_t i = 0; i < slcBands_.size(); ++i )
        delete slcBands_.at( i );
        */

    for ( size_t i = 0; i < carmaSlcBands_.size(); ++i )
        delete carmaSlcBands_.at( i );
    
    for ( size_t i = 0; i < wbcBands_.size(); ++i )
        delete wbcBands_.at( i );
    
    delete &astro_;
    delete &c3gMax8Pipeline_;
    delete &c3gMax23Pipeline_;
    delete &centralIf_;
    delete &control_;
    delete &delayEngine_;
    delete &imr_;
    delete &alarm_;
    delete &lineLength_;
    delete &loRef_;
    delete &loberotator_;
    delete &masterclock_;
    delete &opacityMonitor_;
    delete &phaseMonitor_;
    delete &projectDatabaseManager_;
    delete &signalPath_;
    delete &sldc_;
    delete &slPipeline_;
    delete &systemStatus_;
    delete &test_;
    delete &wbdc_;
    delete &wbPipeline_;
    delete &weather_;
    delete &fault_;

    for ( size_t i = 0; i < wbDataflow_.size(); ++i )
        delete wbDataflow_.at( i );

    for ( size_t i = 0; i < slDataflow_.size(); ++i )
        delete slDataflow_.at( i );

    delete &systemFrameBuffer_;

} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in MonitorSystem::~MonitorSystem" );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    
    return;
}


AntennaCommon &
MonitorSystem::antennaCommon( const int index ) const
{
    if ( index < 0 ) {
        ostringstream oss;
        
        oss << "MonitorSystem::antennaCommon index (" << index
            << ") is negative";
        
        throw CARMA_ERROR( oss.str() );
    }
    
    {
        const int ovroIndex = index;
        
        if ( ovroIndex < OvroSubsystem::COUNT )
            return ovro( ovroIndex ).antennaCommon();
    }
    
    {
        const int bimaIndex = index - OvroSubsystem::COUNT;
    
        if ( bimaIndex < BimaSubsystem::COUNT )
            return bima( bimaIndex ).antennaCommon();
    }
    
    {
        const int szaIndex =
            index - (OvroSubsystem::COUNT + BimaSubsystem::COUNT);
    
        if ( szaIndex < SzaSubsystem::COUNT )
            return sza( szaIndex ).antennaCommon();
    }
    
    {
        ostringstream oss;
        
        oss << "MonitorSystem::antennaCommon index (" << index
            << ") is out of range";
        
        throw CARMA_ERROR( oss.str() );
    }
}


int 
MonitorSystem::getWbcBandCount() const 
{
    return WbcBandSubsystem::COUNT ;
}

int 
MonitorSystem::getCarmaSlcBandCount() const 
{
    return CarmaSlcBandSubsystem::COUNT ;
}

int 
MonitorSystem::getCarma3gBandCount() const 
{
    return Carma3GBandSubsystem::COUNT ;
}

string MonitorSystem::addTransportTimesAsString (ostringstream o) const
{
    o.setf(ios::fixed);
    int indent = 2;
    string separator = "  ";
    o << setw(indent) << "";
    o << "Collator" << separator << "Collator"  
      << endl
      << "  write " << separator << "  write "
      << endl
      << "  delay " << separator << "  time  "
      << endl
      << endl;
    
    string separator1 = "      ";
    double frameTime = ::carma::util::Time::MJD(getFrameCount()+1);
    o << setw(indent) << "";
    o << static_cast<int>(1000*this->getCollatorWriteDelay())
      << separator1
      << static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*
                              (this->getCollatorWriteTime() - frameTime))
      << endl
      << endl;

    return o.str();
}



string MonitorSystem::transportStatisticsToString (bool canonical) const
{
    ostringstream o;
    o.setf(ios::fixed);
    int indent = 2;
    o << setw(indent) << "";
    if (canonical) {
        o << getCanonicalName();
    }
    else {
        o << getName();
    }
    o << ":";

    double frameTime = ::carma::util::Time::MJD(getFrameCount()+1);
    int collatorWriteTime = 0;
    int rawReadTime = 0;
    int finalWriteTime = 0;
    if (this->isCurrent())  {
        collatorWriteTime = (this->getCollatorWriteTime() == 0)  ?  0 :
            static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*
                              (this->getCollatorWriteTime() - frameTime));

        rawReadTime = (this->getRawReadTime() == 0)  ?  0 :
            static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*
                              (this->getRawReadTime() - frameTime));

        finalWriteTime = (this->getFinalWriteTime() == 0)  ?  0 :
            static_cast<int>(1000*::carma::util::Time::SECONDS_PER_DAY*
                              (this->getFinalWriteTime() - frameTime));
    }

    string separator = "  ";
    o << separator
      << "CollatorWriteDelay="  
      << static_cast<int>(1000*this->getCollatorWriteDelay())
      << separator
      << "CollatorWriteTime="  
      << collatorWriteTime
      << endl
      << separator
      << separator << separator << separator << separator
      << "RawReadTime="  
      << rawReadTime
      << separator
      << "FinalWriteTime="  
      << finalWriteTime
      << endl;

    // extra line for readability
    o << endl;
 
    o << MonitorSubsystem::transportHeaderToString();
    for (int i=0; i<getNumChildren(); i++) {
        o << getChild(i).componentRef().transportStatisticsToString (canonical); 
    }
    return o.str();
}



bool MonitorSystem::isCurrent () const
{
    return systemFrameBuffer_.isCurrentFrame();
}


MonitorSubsystem &
MonitorSystem::getChildSubsystem( const int index ) const
{
    const MonitorContainer::Child child = getChild( index );
    
    MonitorSubsystem * const childAsSubsystem = child.subsystemPtr();
        
    if ( childAsSubsystem == 0 ) {
        ostringstream oss;
        
        if ( child.isNull() )
            oss << "getChild(" << index << ")  return NULL child";
        else {
            oss << "Dynamic cast to MonitorSubsystem for \"" 
                << child.componentRef().getCanonicalName() << "\" failed";
        }
        
        throw CARMA_ERROR( oss.str() );
    }
    
    return *childAsSubsystem;
}


SubsystemFrame &
MonitorSystem::getChildSubsystemFrame( const int index ) const {
    return getChildSubsystem( index ).monitorPointSet( ).getSubsystemFrame( );
}

long MonitorSystem::getNumValidSubsystems () const
{
    long numValidSubsystems = 0;
    const long frameCount = getFrameCount();
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        if ( getChildSubsystem( i ).isCurrent( frameCount ) )
            ++numValidSubsystems;
    }

    return numValidSubsystems;
}

void MonitorSystem::resetQueue () 
{
    systemFrameBuffer_.setNoneAvailable();
}

unsigned int
MonitorSystem::read()
{
    return systemFrameBuffer_.read();
}


bool
MonitorSystem::readNewest()
{
    return systemFrameBuffer_.readNewest();
}

bool
MonitorSystem::readNewestConditionalCopy()
{
    return systemFrameBuffer_.readNewestConditionalCopy();
}

bool
MonitorSystem::readNewestIfStale()
{
    return systemFrameBuffer_.readNewestIfStale();
}

void
MonitorSystem::write()
{
    systemFrameBuffer_.write();
}


int
MonitorSystem::getFrameCount() const
{
    return systemFrameBuffer_.getFrameCount();
}


double MonitorSystem::getCollatorWriteDelay() const
{
    return systemFrameBuffer_.getCollatorWriteDelay();
}


double MonitorSystem::getCollatorWriteTime() const
{
    return systemFrameBuffer_.getCollatorWriteTime();
}


void
MonitorSystem::setRawReadTime( const double mjdTimestamp ) 
{
    systemFrameBuffer_.setRawReadTime( mjdTimestamp );

    // Now set monitor point associated with this value
    try {
        MonitorPointInt & rawReadTime =
            dynamic_cast< MonitorPointInt & >( getMonitorPoint(
                "SystemStatus.MonitorSystemStats.rawReadTime", false ) );

        const double frameMjd = carma::util::Time::MJD( getFrameCount() + 1 );
        rawReadTime.setValue( static_cast< int >( 
            ( mjdTimestamp - frameMjd ) * Time::MILLISECONDS_PER_DAY ) );
        
    } catch (...) {
        // Stifle
    } 
}


void
MonitorSystem::setRawReadTime( )
{
    setRawReadTime( carma::util::Time::MJD() );
}


double MonitorSystem::getRawReadTime() const
{
    return systemFrameBuffer_.getRawReadTime();
}


void
MonitorSystem::setFinalWriteTime( const double mjdTimestamp ) 
{
    systemFrameBuffer_.setFinalWriteTime( mjdTimestamp );

    // Now set monitor point associated with this value
    try {
        MonitorPointInt & finalWriteTime =
            dynamic_cast< MonitorPointInt & >( getMonitorPoint(
                "SystemStatus.MonitorSystemStats.finalWriteTime", false ) );

        const double frameMjd = carma::util::Time::MJD( getFrameCount() + 1 );
        finalWriteTime.setValue( static_cast< int >( 
            ( mjdTimestamp - frameMjd ) * Time::MILLISECONDS_PER_DAY ) );
    } catch (...) {
        // Stifle
    } 
}


void
MonitorSystem::setFinalWriteTime( ) 
{
    setFinalWriteTime( carma::util::Time::MJD() );
}


double MonitorSystem::getFinalWriteTime() const
{
    return systemFrameBuffer_.getFinalWriteTime();
}


void MonitorSystem::resetTimes() 
{
    return systemFrameBuffer_.clearAllTimes();
}


long    
MonitorSystem::getTotalNumSubsystems() const
{
    return systemFrameBuffer_.getMaxNumSubsystemFrames();
}


long    
MonitorSystem::getTotalNumMonitorPoints() const
{
    return systemFrameBuffer_.getMaxTotalMonitorPoints();
}


long    
MonitorSystem::getTotalNumMonitorSamples() const
{
    return systemFrameBuffer_.getMaxTotalSamples();
}


size_t    
MonitorSystem::getTotalMonitorSystemSize() const
{
    return systemFrameBuffer_.getSystemTotalSizeInBytes();
}


long
MonitorSystem::getMaxNumMonitorPoints() const
{
    long cumulativeMaxMonitorPoints  = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeMaxMonitorPoints +=
            getChildSubsystem( i ).maxMonitorPoints();
    }

    return cumulativeMaxMonitorPoints;
}


long    
MonitorSystem::getMaxNumMonitorSamples() const
{
    long cumulativeMaxMonitorSamples = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeMaxMonitorSamples +=
            getChildSubsystem( i ).maxSamples();
    }
    
    return cumulativeMaxMonitorSamples;
}


size_t    
MonitorSystem::getMaxMonitorSystemSize() const
{
    size_t cumulativeSubsystemSize   = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        MonitorSubsystem & subsystem = getChildSubsystem( i );
        
        cumulativeSubsystemSize +=
            SubsystemFrame::sizeFrame( subsystem.maxMonitorPoints(),
                                       subsystem.maxSamples() );
   }

   return cumulativeSubsystemSize;
}


long 
MonitorSystem::getActualNumSubsystems() const
{
    return getNumChildren();
}


long    
MonitorSystem::getActualNumMonitorPoints() const
{
    long cumulativeActualMonitorPoints  = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeActualMonitorPoints +=
            getChildSubsystemFrame( i ).getNumMonitorPoints();
    }

    return cumulativeActualMonitorPoints;
}


long    
MonitorSystem::getActualNumMonitorPoints(const MonitorValueType type) const
{
    long cumulativeActualMonitorPoints  = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeActualMonitorPoints +=
            getChildSubsystemFrame( i ).getNumMonitorPoints(type);
    }

    return cumulativeActualMonitorPoints;
}


long    
MonitorSystem::getActualNumMultiSampleMonitorPoints() const
{
    long cumulativeActualMonitorPoints  = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        SubsystemFrame & ssFrame = getChildSubsystemFrame( i );
            
        cumulativeActualMonitorPoints +=
            (ssFrame.getNumMonitorPoints() - ssFrame.getNumSingleSamplePoints());
    }

    return cumulativeActualMonitorPoints;
}


long
MonitorSystem::getActualNumMonitorSamples() const
{
    long cumulativeActualMonitorSamples = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeActualMonitorSamples +=
            getChildSubsystemFrame( i ).getNumActualSamples();
    }
    
    return cumulativeActualMonitorSamples;
}


long
MonitorSystem::getActualNumMonitorSamples (const MonitorValueType type) const
{
    long cumulativeActualMonitorSamples = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeActualMonitorSamples +=
            getChildSubsystemFrame( i ).getNumMonitorSamples(type);
    }
    
    return cumulativeActualMonitorSamples;
}


size_t    
MonitorSystem::getActualMonitorSystemSize() const
{
    size_t cumulativeSystemSize = SystemFrame::getSystemHeaderSizeInBytes( );
        
    const int numSubsystems = getNumChildren();

    cumulativeSystemSize = 
         numSubsystems * sizeof (SubsystemHeader)
         + getActualNumMonitorPoints()*(sizeof (MonitorHeader) +sizeof (int))
         + getActualNumMonitorSamples()*(sizeof (MonitorSampleValue));

    return cumulativeSystemSize;
}


long 
MonitorSystem::getAllocatedNumSubsystems() const
{
    return systemFrameBuffer_.getNumSubsystemFrames();
}


long 
MonitorSystem::getAllocatedNumMonitorPoints() const
{
    long cumulativeNumMonitorPoints  = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeNumMonitorPoints +=
            getChildSubsystemFrame( i ).getNumMonitorPoints();
    }

    return cumulativeNumMonitorPoints;
}


long    
MonitorSystem::getAllocatedNumMonitorSamples() const
{
    long cumulativeNumMonitorSamples = 0;
    const int numSubsystems = getNumChildren();

    for (int i=0; i < numSubsystems; i++) {
        cumulativeNumMonitorSamples +=
            getChildSubsystemFrame( i ).getNumMonitorSamples();
    }
    
    return cumulativeNumMonitorSamples;
}


size_t    
MonitorSystem::getAllocatedMonitorSystemSize() const
{
    size_t cumulativeSubsystemSize   = 0;
    const int numSubsystems = getNumChildren();

    cumulativeSubsystemSize = 
             numSubsystems*sizeof (SubsystemHeader)
             + getAllocatedNumMonitorPoints()*(
                                     sizeof (MonitorHeader) +sizeof (int))
             + getAllocatedNumMonitorSamples()*(sizeof (MonitorSampleValue));

   return cumulativeSubsystemSize;
}


long 
MonitorSystem::getCountedNumMonitorPoints() const
{
    long cumulativeNumMonitorPoints  = 0;
    MonitorPointIterator   itr (*this);

    while (++itr)  {
        cumulativeNumMonitorPoints++;
    }

    return cumulativeNumMonitorPoints;
}


long    
MonitorSystem::getCountedNumMonitorSamples() const
{
    long cumulativeNumMonitorSamples = 0;
    MonitorPointIterator   itr (*this);

    while (++itr)  {
        long numSamples = itr.getMonitorPoint().getNumSamples();;
        if (numSamples > 1) numSamples++; // for average sample
        cumulativeNumMonitorSamples += numSamples;
    }

    return cumulativeNumMonitorSamples;
}


size_t    
MonitorSystem::getCountedMonitorSystemSize() const
{
    size_t cumulativeSubsystemSize   = 0;
    long cumulativeNumMonitorPoints  = 0;
    long cumulativeNumMonitorSamples = 0;
    const int numSubsystems = getNumChildren();
    MonitorPointIterator   itr (*this);

    while (++itr)  {
        long numSamples = itr.getMonitorPoint().getNumSamples();;
        if (numSamples > 1) numSamples++; // for average sample
        cumulativeNumMonitorSamples += numSamples;
        cumulativeNumMonitorPoints++;
    }

    cumulativeSubsystemSize = 
         numSubsystems*sizeof (SubsystemHeader)
         + cumulativeNumMonitorPoints*(sizeof (MonitorHeader) +sizeof (int))
         + cumulativeNumMonitorSamples*(sizeof (MonitorSampleValue));

    return cumulativeSubsystemSize;
}


SystemFrameBuffer&    
MonitorSystem::getSystemFrameBuffer() const
{
    return systemFrameBuffer_;
}

void
CarmaMonitorSystem::synchronize( const CarmaMonitorSystem & src )
{
    systemFrameBuffer().synchronize( src.systemFrameBuffer() );
}

