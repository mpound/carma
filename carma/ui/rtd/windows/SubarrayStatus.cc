/*
 * @file
 * 
 * Manages status information for subarray status window.
 *
 * @author Amar Amarnath 
 * $Id: SubarrayStatus.cc,v 1.22 2008/07/28 20:14:55 abeard Exp $
 *
 * $CarmaCopyright$
 */

#include <sstream>
#include <vector>

#include "carma/monitor/Subarray.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/windows/SubarrayStatus.h"

using namespace ::std;
using namespace carma;
using namespace carma::ui;
using namespace carma::monitor;  
using namespace carma::ui::rtd;


//const int SubarrayStatus::STATUS_STRING_SIZE = 32;

SubarrayStatus::SubarrayStatus 
            (::carma::monitor::ControlSubsystem& control, int subarrayIndex)
         : control_(control),
           controller_(control.subarray(subarrayIndex)),
           subarrayNumber_(controller_.number().getValue()),
           numAntennas_(controller_.numberOfAntennas().getValue()),
           numTotalAntennas_(control.antennaCount()),
           subarray_(controller_.number().getValue()),
           actuallyReachable_( 0 ),
           potentiallyReachable_( 0 ),
           trackMJD_(static_cast<double>(0.0))
{
    updateValues();
}


SubarrayStatus::~SubarrayStatus ()
{
}

const int& SubarrayStatus::number() const
{
    return subarrayNumber_ ;
}


const int& SubarrayStatus::numAntennas() const
{
    return numAntennas_ ;
}

const string& SubarrayStatus::antennaRatio () const
{
    return antennaRatio_;
}

const string& SubarrayStatus::reachabilityRatio () const
{
    return reachabilityRatio_;
}

const string& SubarrayStatus::mode () const
{
    return trackMode_;
}

const double&  SubarrayStatus::trackMJD () const 
{
    return trackMJD_ ;
}


const string& SubarrayStatus::trackSource () const
{
    return trackSource_;
}

void
SubarrayStatus::updateValues()
{
    const AntGroup antGroup =
        control_.getSubarrayAntennaGroup( subarrayNumber_ );
    
    updateNumberOfAntennas();
    updateNumReachableObjects( antGroup );
    updateTrackValues();
    trackMode_         = makeTrackModeString( antGroup );
    antennaRatio_      = makeAntennaRatio();
    reachabilityRatio_ = makeReachabilityRatioString();
    trackSource_       = makeTrackSource ();
}


static const string    columnNames[] = {  " Value "  } ;

int SubarrayStatus::numColumns()
{
    return sizeof(columnNames)/sizeof(columnNames[0]);
}

const string& SubarrayStatus::columnHeading (int columnIndex)
{
    int index = columnIndex;

    if (index < 0) index = 0;
    if (index >= SubarrayStatus::numColumns()) 
        index = SubarrayStatus::numColumns() - 1;

    return columnNames[index];
}

// protected

void SubarrayStatus::updateNumberOfAntennas ()
{
    numAntennas_ = controller_.numberOfAntennas().getValue();
}


string SubarrayStatus::makeAntennaRatio () const
{
    ostringstream os;

    os << numAntennas_ << "/" << numTotalAntennas_;
    return os.str();
}


void
SubarrayStatus::evalReachableMP(
    carma::monitor::MonitorPointBool & mp,
    int &                              actual,
    int &                              potential ) {
    if ( mp.getValue( ) )
        ++actual;
        
    ++potential;                                 
}


void
SubarrayStatus::updateNumReachableObjects( const AntGroup & antGroup )
{
    int actual = 0;
    int potential = 0;

    // count number of reachable objects
    
    {
        AntGroup::const_iterator i = antGroup.begin( );
        const AntGroup::const_iterator iEnd = antGroup.end( );
        
        for ( ; i != iEnd; ++i ) {
            ControlSubsystemBase::Antenna * ant = *i;
            
            if ( ant == 0 )
                continue;
                
            ControlSubsystemBase::AntennaReachable & antReachable = 
                ant->antennaReachable( );
            
            evalReachableMP( antReachable.calibrator( ), actual, potential );
            evalReachableMP( antReachable.cryo( ),       actual, potential );
            evalReachableMP( antReachable.drive( ),      actual, potential );
            evalReachableMP( antReachable.focus( ),      actual, potential );
            evalReachableMP( antReachable.opticalTel( ), actual, potential );
            evalReachableMP( antReachable.rxSelector( ), actual, potential );
        }
    }

    ControlSubsystemBase::Reachable & reachable = controller_.reachable( );

    if (subarray_.hasCorrelator())  {
        // this code should check for the type of correlator and
        // examine reachability of appropriate downconverter and
        // correlator bands as well.

        evalReachableMP( reachable.sldcSystem(),  actual, potential );
        evalReachableMP( reachable.sldcControl(), actual, potential );
        evalReachableMP( reachable.wbdcSystem(),  actual, potential );
        evalReachableMP( reachable.wbdcControl(), actual, potential );
        evalReachableMP( reachable.noiseSource(), actual, potential );
        evalReachableMP( reachable.quadMod(),     actual, potential );
        evalReachableMP( reachable.loRef(),       actual, potential );
        evalReachableMP( reachable.loberotator(), actual, potential );
        evalReachableMP( reachable.linelength(),  actual, potential );
        evalReachableMP( reachable.clock(),       actual, potential );
        evalReachableMP( reachable.slPipeline(),  actual, potential );
        evalReachableMP( reachable.wbPipeline(),  actual, potential );
    }
    
    actuallyReachable_ = actual;
    potentiallyReachable_ = potential;
}


string
SubarrayStatus::makeTrackModeString( const AntGroup & antGroup ) const
{
    bool haveACommonMode = false;
    int commonMode = 0;
    
    {
        AntGroup::const_iterator i = antGroup.begin( );
        const AntGroup::const_iterator iEnd = antGroup.end( );
        
        for ( ; i != iEnd; ++i ) {
            ControlSubsystemBase::Antenna * ant = *i;
            
            if ( ant == 0 )
                continue;
            
            const int antMode = ant->trackMode().getValue();
            
            if ( haveACommonMode == false ) {
                commonMode = antMode;
                haveACommonMode = true;
            } else if ( antMode != commonMode )
                return "MIXED";
        }
    }

    if ( haveACommonMode == false )
        return "NONE";
        
    switch ( commonMode ) {
        case ControlSubsystemBase::TrackModeMonitorPointEnum::IDLE:
            return "IDLE";

        case ControlSubsystemBase::TrackModeMonitorPointEnum::SOURCE:
            return "RA/DEC";

        case ControlSubsystemBase::TrackModeMonitorPointEnum::AZEL:
            return "AZ/EL";
    }

    return "BadVal";
}


string
SubarrayStatus::makeReachabilityRatioString () const
{
    ostringstream os;
    os << actuallyReachable_ << "/" << potentiallyReachable_ ;

    return os.str();
}


void
SubarrayStatus::updateTrackMJD ()
{
    trackMJD_ = controller_.commands().track().timestamp().getValue();
}


string
SubarrayStatus::makeTrackSource () const
{
    const string noSource ("NO SOURCE");

    string trackSource(controller_.source().getValue());
    if (trackSource.length() == 0)  trackSource = noSource;

    return trackSource;
}


void
SubarrayStatus::updateTrackValues ()
{
    updateTrackMJD();
}

