/*
 * @file
 *
 * Gets data from the carma control subsystem and displays.
 *
 * @author Amar Amarnath
 * $Id: rtdcontrolsubarrays.cc,v 1.33 2014/06/04 17:09:50 mpound Exp $
 *
 * $CarmaCopyright$
 */

#include <iosfwd>
#include <sstream>
#include <vector>

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/windows/SubarrayStatus.h"
#include "carma/ui/rtd/windows/controlAnts.h"
#include "carma/ui/rtd/windows/controlReachable.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;


namespace {


SubarrayStatus * const kNullSubarrayStatusPtr = 0;


class SubarrayDisplay : public MonitorDisplay {
    public:
        struct AntNameInfo {
            string    text;
            CellColor color;
        };

        SubarrayDisplay( const string & subtitle,
                         bool           visibleTimePanel );

        ~SubarrayDisplay( );

        SubarrayStatus * subarrayStatus( int saIndex );

        int getMaxSaAntNames( ) const;

        const AntNameInfo & saAntNameInfoRef( int saIndex, int antIndex ) const;

    protected:
        virtual void internalUpdate( );

        void updateAllSubarrays( );

        void updateAntNameInfo( );

        CellColor calcAntNameColor(
            const ControlSubsystem::Antenna * ant,
            CellColor                         defaultColor ) const;

    private:
        const int maxShownCarmaAntNo_;

        vector< SubarrayStatus * >      subarrayStatus_;
        vector< vector< AntNameInfo > > subarraysAntNames_;
};


SubarrayDisplay::SubarrayDisplay( const string & subtitle,
                                  const bool     visibleTimePanel ) :
MonitorDisplay( subtitle, visibleTimePanel ),
maxShownCarmaAntNo_( 23 ),
subarrayStatus_( ControlSubsystem::subarrayCount(), kNullSubarrayStatusPtr ),
subarraysAntNames_()
{
    ControlSubsystem & control = cms().control();

    const int numSubarrays = ControlSubsystem::subarrayCount();

    subarraysAntNames_.resize( numSubarrays );

    for ( int saIndex = 0; saIndex < numSubarrays; ++saIndex ) {
        subarrayStatus_.at( saIndex ) = new SubarrayStatus( control, saIndex );
        subarrayStatus_.at( saIndex )->updateValues();

        subarraysAntNames_.at( saIndex ).resize( maxShownCarmaAntNo_ );
        for ( int antIndex = 0; antIndex < maxShownCarmaAntNo_; ++antIndex ) {
            subarraysAntNames_.at( saIndex ).at( antIndex ).color =
                WHITE_CELL_COLOR;
        }
    }

    updateAllSubarrays();
}


SubarrayDisplay::~SubarrayDisplay( )
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
SubarrayDisplay::calcAntNameColor(
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


//@TODO The assumption of subarray<->correlator is no longer valid!
MonitorCorrelatorDesignation
getSaCorrDesignation( const int saNo )
{
    switch ( saNo ) {
        case 1:  monitor::corrTypeToCorrDes(CORR_SPECTRAL);
        case 2:  monitor::corrTypeToCorrDes(CORR_WIDEBAND);
    }

    return monitor::corrTypeToCorrDes(CORR_NONE);
}


void
SubarrayDisplay::updateAntNameInfo( )
{
    const int kMaintSaNo = ControlSubsystem::getMaintenanceSubarrayNo();

    const ControlSubsystem & controlSubsys = cms().control();

    typedef map< int, const ControlSubsystem::Antenna * > SaCarmaAntNoMap;

    const int saAntNamesCount = subarraysAntNames_.size();

    for ( int saNo = 1; saNo <= saAntNamesCount; ++saNo ) {
      const MonitorCorrelatorDesignation saCorrDesig = getSaCorrDesignation( saNo );

        // Construct the map by carma antenna numbers for this subarray
        SaCarmaAntNoMap saCarmaAntNoMap;
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
            }
        }

        // Update the vector of antenna names for this subarray
        {
            vector< AntNameInfo > & saAntNames =
                subarraysAntNames_.at( saNo - 1 );

            vector< AntNameInfo >::iterator i = saAntNames.begin();
            const vector< AntNameInfo >::iterator iEnd = saAntNames.end();

            SaCarmaAntNoMap::const_iterator j = saCarmaAntNoMap.begin();
            const SaCarmaAntNoMap::const_iterator jEnd = saCarmaAntNoMap.end();

            CellColor defaultColor = STRIPE_DARK_CELL_COLOR;

            for ( ; i != iEnd; ++i ) {
                string text;
                CellColor color = defaultColor;

                if ( j != jEnd ) {
                    const ControlSubsystem::Antenna * const ant = j->second;

                    ostringstream oss;

                    const MonitorCorrelatorDesignation MONITOR_CORR_NONE 
                        = monitor::corrTypeToCorrDes(CORR_NONE);
                    if ( saCorrDesig != MONITOR_CORR_NONE ) {
                        const CorrDesignation & corrDesigMp =
                          ant->CORRELATOR_DESIGNATION_MP();

                        if ( (corrDesigMp.isValid() == false) ||
                             (corrDesigMp.getValue() != saCorrDesig) )
                            oss << "* ";
                    }

                    oss << "C" << j->first;

                    if ( ant != 0 ) {
                        const string typedAntName = ant->name().getValue();

                        if ( typedAntName.empty() == false )
                            oss << " (" << typedAntName << ")";
                    }

                    text = oss.str();

                    if ( saNo == kMaintSaNo ) {
                        // special case the maintenance subarray
                        color = defaultColor;
                    } else
                        color = calcAntNameColor( ant, defaultColor );

                    ++j;
                }

                i->text = text;
                i->color = color;

                if ( defaultColor == STRIPE_DARK_CELL_COLOR )
                    defaultColor = STRIPE_LIGHT_CELL_COLOR;
                else
                    defaultColor = STRIPE_DARK_CELL_COLOR;
            }
        }
    }
}


void
SubarrayDisplay::updateAllSubarrays( )
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
}


void
SubarrayDisplay::internalUpdate( )
{
    updateAllSubarrays();
}


SubarrayStatus *
SubarrayDisplay::subarrayStatus( const int saIndex )
{
    return subarrayStatus_.at( saIndex );
}


int
SubarrayDisplay::getMaxSaAntNames( ) const
{
    return maxShownCarmaAntNo_;
}


const SubarrayDisplay::AntNameInfo &
SubarrayDisplay::saAntNameInfoRef( const int saIndex,
                                   const int antIndex ) const
{
    return subarraysAntNames_.at( saIndex ).at( antIndex );
}


void
addSubarrayStatusFolder( SubarrayDisplay & display )
{
    const int saCount = ControlSubsystem::subarrayCount();
    const int antNameRows = display.getMaxSaAntNames();

    RtTablePtr table( new RtTable( "Subarrays" ) );

    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const string saName =
            ControlSubsystem::getSubarrayAlphanumericName( saNo );

        table->addCol( RtColumn::makeColumn( saName ) );
    }

    table->addRow( RtRow::makeRow( "Sa #" ) );
    table->addRow( RtRow::makeRow( "Running" ) );
    table->addRow( RtRow::makeRow( "Initialized" ) );
    table->addRow( RtRow::makeRow( "Reachability Ratio" ) );
    table->addRow( RtRow::makeRow( "SA Mode" ) );
    table->addRow( RtRow::makeRow( "Track Time" ) );
    table->addRow( RtRow::makeRow( "Track Source" ) );

    for ( int i = 0; i < antNameRows; ++i ) {
        if ( i == 0 )
            table->addRow( RtRow::makeRow( "Antennas" ) );
        else
            table->addRow( RtRow::makeRow( string() ) );
    }

    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const ControlSubsystem::Subarray & sa =
            display.cms().control().subarray( saNo - 1 );

        SubarrayStatus * const saStatus = display.subarrayStatus( saNo - 1 );

        {
            CellPtr saStatusNo = CellPtr(new CellInt( "20.19.1", saStatus->number() ));
            saStatusNo->setValidity( true );
            table->add( saStatusNo );
        }

        {
            MonitorCellPtr running = MonitorCell::makeCell( sa.controllerRunning() );

            MonitorCellBool * const runningBool =
                dynamic_cast< MonitorCellBool * >( running.get() );

            if ( runningBool == 0 )
                programLogErrorIfPossible( "Running cell was not a bool" );
            else {
                runningBool->overrideStateAppearances(
                    "true",  GREEN_CELL_COLOR,
                    "FALSE", RED_CELL_COLOR );
            }

            table->add( running );
        }

        {
            MonitorCellPtr initialized = MonitorCell::makeCell( sa.controllerInitialized() );

            MonitorCellBool * const initializedBool =
                dynamic_cast< MonitorCellBool * >( initialized.get() );

            if ( initializedBool == 0 )
                programLogErrorIfPossible( "Initialized cell was not a bool" );
            else {
                initializedBool->overrideStateAppearances(
                    "true",  GREEN_CELL_COLOR,
                    "FALSE", RED_CELL_COLOR );
            }

            table->add( initialized );
        }

        {
            CellPtr saStatusReachabilityRatio = CellPtr(
                new CellString( "20.14.6", saStatus->reachabilityRatio() ));

            saStatusReachabilityRatio->setValidity( true );
            table->add( saStatusReachabilityRatio );
        }

        {
            CellPtr saStatusMode = CellPtr(
                new CellString( "20.14.6", saStatus->mode() ));

            saStatusMode->setValidity( true );
            table->add( saStatusMode );
        }

        {
            CellPtr saStatusTrackMjd = CellPtr(
                new CellDateUT( "20.0.20", 2,  saStatus->trackMJD() ));

            saStatusTrackMjd->setValidity( true );
            table->add( saStatusTrackMjd );
        }

        {
            CellPtr saStatusTrackSource = CellPtr(
                new CellString( "20.10.10", saStatus->trackSource() ));

            saStatusTrackSource->setValidity( true );
            table->add( saStatusTrackSource );
        }

        for ( int i = 0; i < antNameRows; ++i ) {
            const SubarrayDisplay::AntNameInfo & antNameInfo =
                display.saAntNameInfoRef( (saNo - 1), i );

            CellPtr antName = CellPtr(
                new CellString( "13.0.13",
                                antNameInfo.text,
                                antNameInfo.color ));

            antName->setLayout( EOL_CENTERED_LAYOUT );
            antName->setValidity( true );

            if ( i > 0 )
                antName->setBorderTopEnabled( false );

            if ( (i + 1) < antNameRows )
                antName->setBorderBottomEnabled( false );

            table->add( antName );
        }
    }

    const int numRows = table->getNumRows();

    RtSpringPtr spring(new RtSpring( 4, 1.0 ));

    RtFolderPtr folder(new RtFolder( "Subarrays" ));

    folder->add( table );
    folder->add( spring );

    table->setMinRows( numRows );
    table->setPrefRows( numRows );

    display.add( folder );
}


void
runDisplay( const string & helpFilePath )
{
    // Create a display
    SubarrayDisplay display( "Control Status", true );

    display.setSpecificHelpFromTextFile( "Control Status Help", helpFilePath );

    addSubarrayStatusFolder( display );
    addControlAntsFolder( display );
    addControlReachableFolder( display );

    while ( display.serveData() ) {
        // none
    }
}


} // namespace < anonymous >


int
Program::main( )
{
    {
        const string baseLogname = getLogname();

        setInstanceLogname( baseLogname + ".controlsubarrays" );
    }

    runDisplay( getConfFile( "rtd/help/controlsubarrays.html" ) );

    return EXIT_SUCCESS;
}
