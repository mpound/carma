/*
 * @file
 * 
 * Gets data from the carma control subsystem and displays.
 *
 * @author Marc Pound
 * $Id: CompositeSubarrayDisplay.cc,v 1.15 2014/04/14 22:00:48 scott Exp $
 *
 * $CarmaCopyright$
 */

#include <iosfwd>
#include <sstream>
#include <vector>

#include "carma/util/Program.h"

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/windows/CompositeSubarrayDisplay.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;



SubarrayStatus * const kNullSubarrayStatusPtr = 0;

CompositeSubarrayDisplay::CompositeSubarrayDisplay( 
	                          const string & subtitle, 
                                  const bool     visibleTimePanel ) :
MonitorDisplay( subtitle, visibleTimePanel ),
maxShownCarmaAntNo_(23),
subarrayStatus_( ControlSubsystem::subarrayCount(), kNullSubarrayStatusPtr ),
subarraysAntNames_(),
antNameLists_(),
corrNameLists_(),
azelPlotManager_( cms(), 23, 5 )

{ 
    ControlSubsystem & control = cms().control();
    const int numSubarrays = ControlSubsystem::subarrayCount();
    
    subarraysAntNames_.resize( numSubarrays );
    antNameLists_.resize( numSubarrays );
    corrNameLists_.resize( numSubarrays );
    
    for ( int saIndex = 0; saIndex < numSubarrays; ++saIndex ) {
        subarrayStatus_.at( saIndex ) = new SubarrayStatus( control, saIndex );
        subarrayStatus_.at( saIndex )->updateValues();

        subarraysAntNames_.at( saIndex ).resize( maxShownCarmaAntNo_ );
        for ( int antIndex = 0; antIndex < maxShownCarmaAntNo_; ++antIndex ) {
            subarraysAntNames_.at( saIndex ).at( antIndex ).color =
                WHITE_CELL_COLOR;
        }
	// make these different to ensure a proper update the
	// first time through
	antNameLists_.at( saIndex ).current   = "FOO";
	antNameLists_.at( saIndex ).previous  = "BAR";
	corrNameLists_.at( saIndex ).current  = "FOO";
	corrNameLists_.at( saIndex ).previous = "BAR";
    }

    updateAllSubarrays();
}


CompositeSubarrayDisplay::~CompositeSubarrayDisplay( )
try {
    vector< SubarrayStatus * >::iterator i = subarrayStatus_.begin();
    const vector< SubarrayStatus * >::iterator iEnd = subarrayStatus_.end();
    
    for ( ; i != iEnd; ++i ) {
        SubarrayStatus * const deadSSWalking = *i;

        if ( deadSSWalking != 0 ) {
            *i = 0;
            
            delete deadSSWalking;
        }
    }
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

CellColor
CompositeSubarrayDisplay::calcAntNameColor(
    const ControlSubsystem::Antenna * const ant,
    const CellColor                         defaultColor ) const
{
    bool someInvalid = false;
    bool someUnreachable = false;

    if ( ant != 0 ) {
        const ControlSubsystemBase::AntennaReachable & antReachable =
            ant->antennaReachable();
            
        if ( antReachable.calibrator().isValid() == false )
            someInvalid = true;
        if ( antReachable.calibrator().getValue() == false )
            someUnreachable = true;

        if ( antReachable.cryo().isValid() == false )
            someInvalid = true;
        if ( antReachable.cryo().getValue() == false )
            someUnreachable = true;

        if ( antReachable.drive().isValid() == false )
            someInvalid = true;
        if ( antReachable.drive().getValue() == false )
            someUnreachable = true;

        if ( antReachable.focus().isValid() == false )
            someInvalid = true;
        if ( antReachable.focus().getValue() == false )
            someUnreachable = true;

        if ( antReachable.opticalTel().isValid() == false )
            someInvalid = true;
        if ( antReachable.opticalTel().getValue() == false )
            someUnreachable = true;

        if ( antReachable.rxSelector().isValid() == false )
            someInvalid = true;
        if ( antReachable.rxSelector().getValue() == false )
            someUnreachable = true;
    }

    if ( someUnreachable )
        return RED_CELL_COLOR;
        
    if ( someInvalid )
        return YELLOW_CELL_COLOR;

    return defaultColor;
}


void
CompositeSubarrayDisplay::updateAntNameInfo( )
{
    const int kMaintSaNo = ControlSubsystem::getMaintenanceSubarrayNo();
    
    const ControlSubsystem & controlSubsys = cms().control();

    typedef map< int, const ControlSubsystem::Antenna * > SaCarmaAntNoMap;

    const int saAntNamesCount = subarraysAntNames_.size();

    for ( int saNo = 1; saNo <= saAntNamesCount; ++saNo ) {
        int count = 0;
        // Construct the map by carma antenna numbers for this subarray
        SaCarmaAntNoMap saCarmaAntNoMap;
        ostringstream antNameListCreator;

        {
            const set< ControlSubsystem::Antenna * > saAnts =
                controlSubsys.getSubarrayAntennaGroup( saNo );

            set< ControlSubsystem::Antenna * >::const_iterator i =
                saAnts.begin();
                
            const set< ControlSubsystem::Antenna * >::const_iterator iEnd =
                saAnts.end();
            
            for ( ; i != iEnd; ++i ) {
                const ControlSubsystem::Antenna * const ant = *i;
                
                if ( ant == 0 )
                    continue;
                    
                const int carmaAntNo = ant->carmaAntennaNumber().getValue();
                
                if ( (carmaAntNo < 1) || (carmaAntNo > maxShownCarmaAntNo_) )
                    continue;
                    
                saCarmaAntNoMap.insert( make_pair( carmaAntNo, ant ) );
                count++;
            }

        }

        unsigned int saIdx = saNo - 1;
        // Update the vector of antenna names for this subarray
        {
            vector< AntNameInfo > & saAntNames = subarraysAntNames_.at(saIdx);
            
            vector< AntNameInfo >::iterator i = saAntNames.begin();
            const vector< AntNameInfo >::iterator iEnd = saAntNames.end();

            SaCarmaAntNoMap::const_iterator j = saCarmaAntNoMap.begin();
            const SaCarmaAntNoMap::const_iterator jEnd = saCarmaAntNoMap.end();

            CellColor defaultColor = STRIPE_DARK_CELL_COLOR;
            
            for ( ; i != iEnd; ++i ) {
                string longName;
                string shortName;
                int carmaAntNo = 1;
                CellColor color = defaultColor;
                
                if ( j != jEnd ) {
                    const ControlSubsystem::Antenna * const ant = j->second;

                    ostringstream oss;
                    
                    oss << "C" << j->first;
                    shortName = oss.str();
                    carmaAntNo = j->first;
                    
                    if ( ant != 0 ) {
                        const string typedAntName = ant->name().getValue();
                
                        if ( typedAntName.empty() == false )
                            oss << " (" << typedAntName << ")";
                    }
                    
                    longName = oss.str();
                    
                    if ( saNo == kMaintSaNo ) {
                        // special case the maintenance subarray
                        color = defaultColor;
                    } else
                        color = calcAntNameColor( ant, defaultColor );
                
                    ++j;
                }
                
                i->shortName  = shortName;
                i->longName   = longName;
                i->carmaAntNo = carmaAntNo;
                i->color      = color;

                antNameListCreator << shortName << " " ;
                
                if ( defaultColor == STRIPE_DARK_CELL_COLOR )
                    defaultColor = STRIPE_LIGHT_CELL_COLOR;
                else
                    defaultColor = STRIPE_DARK_CELL_COLOR;
            }

        }

        // now update the strings indicating possibe membership change
        antNameLists_.at(saIdx).previous = antNameLists_.at(saIdx).current;
        antNameLists_.at(saIdx).current  = antNameListCreator.str();

    } // subarray number loop
}


void
CompositeSubarrayDisplay::updateCorrNameInfo( )
{
    const SignalPathSubsystem & signalPathSubsys = cms().signalPath();
    const int saCorrNamesCount = corrNameLists_.size();

    for(int saNo = 0; saNo < saCorrNamesCount; saNo++){
        ostringstream corrNameListCreator;
	corrNameListCreator << signalPathSubsys.mapping().subarray( saNo ).CORRELATOR_DESIGNATION_MP().getValue();

	corrNameLists_.at(saNo).previous = corrNameLists_.at(saNo).current;
	corrNameLists_.at(saNo).current = corrNameListCreator.str();
    }
}


void
CompositeSubarrayDisplay::updateAllSubarrays( )
{
    cms().readNewest();

    vector< SubarrayStatus * >::const_iterator i =
        subarrayStatus_.begin();
    
    const vector< SubarrayStatus * >::const_iterator iEnd =
        subarrayStatus_.end();
    
    for ( ; i != iEnd; ++i ) {
        SubarrayStatus * const saStatus = *i;
        
        if ( saStatus != 0 )
            saStatus->updateValues();
    }
    
    updateAntNameInfo();
    updateCorrNameInfo();
}


void
CompositeSubarrayDisplay::internalUpdate( )
{
    updateAllSubarrays();
    azelPlotManager_.update();
}

RtAzelPlotPtr
CompositeSubarrayDisplay::getAzelPlot( int saNo )
{
    return azelPlotManager_.getAzelPlot( saNo );
}

SubarrayStatus *
CompositeSubarrayDisplay::subarrayStatus( const int saIndex )
{
    return subarrayStatus_.at( saIndex );
}


int
CompositeSubarrayDisplay::getMaxSaAntNames( ) const
{
    return maxShownCarmaAntNo_;
}


const CompositeSubarrayDisplay::AntNameInfo &
CompositeSubarrayDisplay::saAntNameInfoRef( const int saIndex,
                                   const int antIndex ) const
{
    return subarraysAntNames_.at( saIndex ).at( antIndex );
}

const CompositeSubarrayDisplay::AntNameList &
CompositeSubarrayDisplay::saAntNameListRef( const int saIndex ) const
{
    return antNameLists_.at( saIndex );
}

const CompositeSubarrayDisplay::CorrNameList &
CompositeSubarrayDisplay::saCorrNameListRef( const int saIndex ) const
{
    return corrNameLists_.at( saIndex );
}

bool
CompositeSubarrayDisplay::membershipHasChanged ( const int saIndex ) const
{
    const AntNameList& a = saAntNameListRef( saIndex );
    return ( a.current != a.previous );
}

bool
CompositeSubarrayDisplay::anyMembershipHasChanged ( void ) const
{
    static int countDown = 0;
    unsigned int count = antNameLists_.size() ;
    for ( unsigned int i = 0; i< count ; i++ ) {
        if ( membershipHasChanged( i ) ) countDown = 4;
    }
    if (countDown   <= 0) return false;
    if (countDown-- == 1) return true;
    return false;
}

bool
CompositeSubarrayDisplay::anyCorrMembershipHasChanged ( void ) const
{
    static int countDown = 0;
    unsigned int count = corrNameLists_.size();
    for ( unsigned int i = 0; i < count; i++ ){
        if(corrMembershipHasChanged( i ) ) countDown = 4;
    }
    if (countDown   <= 0) return false;
    if (countDown-- == 1) return true;
    return false;
}

bool
CompositeSubarrayDisplay::corrMembershipHasChanged ( const int saIndex ) const
{
    const CorrNameList & c = saCorrNameListRef( saIndex );
    return ( c.current != c.previous );
}


