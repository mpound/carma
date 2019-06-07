#include "carma/ui/rtd/windows/controlAnts.h"

#include <map>
#include <set>
#include <sstream>

#include <boost/shared_ptr.hpp>

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorSingleTableFolder.h"
#include "carma/ui/rtd/common/MonitorTableVisitor.h"
#include "carma/ui/rtd/common/reachableCellUtils.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;


namespace {


const CellColor kSci1SaAndSpectralCorrColour = BLUE_CELL_COLOR;
const CellColor kSci2SaAndWidebandCorrColour = ORANGE_CELL_COLOR;
const CellColor kEng1SaColour = CYAN_CELL_COLOR;
const CellColor kEng2SaColour = PURPLE_CELL_COLOR;
const CellColor kMaintSaAndNoneCorrColour = LIGHT_GRAY_CELL_COLOR;


int
calcSaNameCellWidth()
{
    const ScopedLogNdc ndc( "calcSaNameCellWidth" );

    ::size_t maxWidth = 3;

    maxWidth = ::std::max( ControlSubsystem::getSubarrayName( 1 ).size(),
                           maxWidth );

    maxWidth = ::std::max( ControlSubsystem::getSubarrayName( 2 ).size(),
                           maxWidth );

    maxWidth = ::std::max( ControlSubsystem::getSubarrayName( 3 ).size(),
                           maxWidth );

    maxWidth = ::std::max( ControlSubsystem::getSubarrayName( 4 ).size(),
                           maxWidth );

    maxWidth = ::std::max( ControlSubsystem::getSubarrayName( 5 ).size(),
                           maxWidth );

    return maxWidth;
}


void
postprocessSaNameCell( MonitorCell & cell )
{
    const ScopedLogNdc ndc( "postprocessSaNameCell" );

    cell.setLayout( EOL_CENTERED_LAYOUT );

    cell.setGoodColorTextOverride( ControlSubsystem::getSubarrayName( 1 ),
                                   kSci1SaAndSpectralCorrColour );

    cell.setGoodColorTextOverride( ControlSubsystem::getSubarrayName( 2 ),
                                   kSci2SaAndWidebandCorrColour );

    cell.setGoodColorTextOverride( ControlSubsystem::getSubarrayName( 3 ),
                                   kEng1SaColour );

    cell.setGoodColorTextOverride( ControlSubsystem::getSubarrayName( 4 ),
                                   kEng2SaColour );

    cell.setGoodColorTextOverride( ControlSubsystem::getSubarrayName( 5 ),
                                   kMaintSaAndNoneCorrColour );
}


void
postprocessCorrDesigCell( const MonitorPoint & mp,
                          MonitorCell &        cell )
{
    const ScopedLogNdc ndc( "postprocessCorrDesigCell" );

    const CorrDesignation * const corrDesigMp =
      dynamic_cast< const CorrDesignation * >( &mp );

    if ( corrDesigMp == 0 ) {
        const string msg =
            "MonitorPoint " + mp.getCanonicalName() + " was not a CorrDesignation";

        programLogErrorIfPossible( msg );
    } else {
        cell.setLayout( EOL_CENTERED_LAYOUT );

        CorrelatorSet corrSet;

        corrSet.initialize(CORR_SPECTRAL);
        cell.setGoodColorTextOverride(
        corrSet.corrTypeString(),                     
            kSci1SaAndSpectralCorrColour );

        corrSet.initialize(CORR_WIDEBAND);
        cell.setGoodColorTextOverride(
            corrSet.corrTypeString(),                     
            kSci2SaAndWidebandCorrColour );

        corrSet.initialize(CORR_NONE);
        cell.setGoodColorTextOverride(
            corrSet.corrTypeString(),
            kMaintSaAndNoneCorrColour );
    }
}


class Visitor : public MonitorTableVisitor {
    public:
        explicit Visitor( );

        ~Visitor( );

        void addToReachableMpSet( const MonitorPointBool & mpBool );

        void addToSaNameMpSet( const MonitorPointString & mpString );

        void addToCorrDesigMpSet( const CorrDesignation & corrDesigMp );

        void addToOtherMpSet( const MonitorPoint & mp,
                              int                  width );

        void addRowLabelOverride( const MonitorPoint & mp,
                                  const string &       override );

    protected:
        bool acceptMonitorPoint( const MonitorPoint & mp ) const;

        int calcCellWidth( const MonitorPoint & mp ) const;

        void postprocessMonitorCell( const MonitorPoint & mp,
                                     MonitorCell &        cell ) const;

        string generateRowLabel( const MonitorPoint & mp,
                                 bool                 showUnits ) const;

    private:
        bool isReachableMp( const MonitorPoint & mp ) const;

        bool isSaNameMp( const MonitorPoint & mp ) const;

        bool isCorrDesigMp( const MonitorPoint & mp ) const;

        bool isOtherMp( const MonitorPoint & mp ) const;

        set< string >         reachableMps_;
        set< string >         saNameMps_;
        set< string >         corrDesigMps_;
        map< string, int >    otherMps_;
        map< string, string > rowLabelOverrides_;
};

typedef boost::shared_ptr<Visitor> VisitorPtr;

Visitor::Visitor( ) :
reachableMps_(),
saNameMps_(),
corrDesigMps_(),
otherMps_(),
rowLabelOverrides_()
{
}


Visitor::~Visitor( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
Visitor::addToReachableMpSet( const MonitorPointBool & mpBool )
{
    reachableMps_.insert( mpBool.getCanonicalName() );
}


void
Visitor::addToSaNameMpSet( const MonitorPointString & mpString )
{
    saNameMps_.insert( mpString.getCanonicalName() );
}


void
Visitor::addToCorrDesigMpSet( const CorrDesignation & corrDesigMp )
{
    corrDesigMps_.insert( corrDesigMp.getCanonicalName() );
}


void
Visitor::addToOtherMpSet( const MonitorPoint & mp,
                          const int            width )
{
    otherMps_.insert( make_pair( mp.getCanonicalName(), width ) );
}


void
Visitor::addRowLabelOverride( const MonitorPoint & mp,
                              const string &       override )
{
    const string shortName = mp.getShortName();

    const map< string, string >::const_iterator i =
        rowLabelOverrides_.find( shortName );

    if ( i == rowLabelOverrides_.end() )
        rowLabelOverrides_.insert( make_pair( shortName, override ) );
    else if ( i->second != override )
        programLogErrorIfPossible( "Conflicting row label overrides" );
}


bool
Visitor::isReachableMp( const MonitorPoint & mp ) const
{
    const MonitorPointBool * const mpBool =
        dynamic_cast< const MonitorPointBool * >( &mp );

    if ( mpBool == 0 )
        return false;

    return (reachableMps_.find( mp.getCanonicalName() ) != reachableMps_.end());
}


bool
Visitor::isSaNameMp( const MonitorPoint & mp ) const
{
    const MonitorPointString * const mpString =
        dynamic_cast< const MonitorPointString * >( &mp );

    if ( mpString == 0 )
        return false;

    return (saNameMps_.find( mp.getCanonicalName() ) != saNameMps_.end());
}


bool
Visitor::isCorrDesigMp( const MonitorPoint & mp ) const
{
    const CorrDesignation * const corrDesigMp =
        dynamic_cast< const CorrDesignation * >( &mp );

    if ( corrDesigMp == 0 )
        return false;

    return (corrDesigMps_.find( mp.getCanonicalName() ) != corrDesigMps_.end());
}


bool
Visitor::isOtherMp( const MonitorPoint & mp ) const
{
    return (otherMps_.find( mp.getCanonicalName() ) != otherMps_.end());
}


bool
Visitor::acceptMonitorPoint( const MonitorPoint & mp ) const
{
    return (isReachableMp( mp ) ||
            isSaNameMp( mp ) ||
            isCorrDesigMp( mp ) ||
            isOtherMp( mp ));
}


int
Visitor::calcCellWidth( const MonitorPoint & mp ) const
{
    if ( isReachableMp( mp ) )
        return calcReachableCellWidth();

    if ( isSaNameMp( mp ) )
        return calcSaNameCellWidth();

    {
        const map< string, int >::const_iterator i =
            otherMps_.find( mp.getCanonicalName() );

        if ( i != otherMps_.end() )
            return i->second;
    }

    return MonitorTableVisitor::calcCellWidth( mp );
}


void
Visitor::postprocessMonitorCell( const MonitorPoint & mp,
                                 MonitorCell &        cell ) const
{
    if ( isReachableMp( mp ) ) {
        MonitorCellBool * const cellBool =
            dynamic_cast< MonitorCellBool * >( &cell );

        if ( cellBool != 0 )
            postprocessReachableCell( *cellBool );
    }

    if ( isSaNameMp( mp ) )
        postprocessSaNameCell( cell );

    if ( isCorrDesigMp( mp ) )
        postprocessCorrDesigCell( mp, cell );
}


string
Visitor::generateRowLabel( const MonitorPoint & mp,
                           const bool           showUnits ) const
{
    {
        const map< string, string >::const_iterator i =
            rowLabelOverrides_.find( mp.getShortName() );

        if ( i != rowLabelOverrides_.end() )
            return i->second;
    }

    return MonitorTableVisitor::generateRowLabel( mp, showUnits );
}


}  // namespace < anonymous >


void
carma::ui::rtd::addControlAntsFolder( MonitorDisplay & display )
{
    CarmaMonitorSystem & monitorSystem = display.cms();

    monitorSystem.readNewest();

    const ControlSubsystem & controlSubsys = monitorSystem.control();

    vector< MonitorContainer * > colMcs;
    colMcs.reserve( 15 );

    {
        vector< MonitorColumnInfo > columns;

        VisitorPtr visitor(new Visitor());

        const int antCount = controlSubsys.antennaCount();

        for ( int i = 0; i < antCount; ++i ) {
            ControlSubsystemBase::Antenna & ant = controlSubsys.antenna(i);

            const int carmaAntNo = ant.carmaAntennaNumber().getValue();

            if ( (carmaAntNo < 1) || (carmaAntNo > 23) ) continue;

            string colName;
            {
                ostringstream oss;

                oss << "C" << carmaAntNo
                    << " (" << ant.name().getValue() << ")";

                colName = oss.str();
            }

            auto_ptr< MonitorContainer >
                colMc( new MonitorContainer( colName ) );

            colMc->add( ant.subarrayName() );
            visitor->addToSaNameMpSet( ant.subarrayName() );
            visitor->addRowLabelOverride( ant.subarrayName(), "SA" );

            colMc->add( ant.CORRELATOR_DESIGNATION_MP() );
            visitor->addToCorrDesigMpSet( ant.CORRELATOR_DESIGNATION_MP() );
            visitor->addRowLabelOverride( ant.CORRELATOR_DESIGNATION_MP(), "Corr" );

            colMc->add( ant.correlatorInputNumber() );
            visitor->addToOtherMpSet( ant.correlatorInputNumber(), 2 );
            visitor->addRowLabelOverride( ant.correlatorInputNumber(), "Input" );

            {
                const ControlSubsystemBase::AntennaReachable & antReachable =
                    ant.antennaReachable();

                colMc->add( antReachable.antenna() );
                colMc->add( antReachable.calibrator() );
                colMc->add( antReachable.cryo() );
                colMc->add( antReachable.drive() );
                colMc->add( antReachable.focus() );
                colMc->add( antReachable.opticalTel() );
                colMc->add( antReachable.rxSelector() );

                visitor->addToReachableMpSet( antReachable.antenna() );
                visitor->addToReachableMpSet( antReachable.calibrator() );
                visitor->addToReachableMpSet( antReachable.cryo() );
                visitor->addToReachableMpSet( antReachable.drive() );
                visitor->addToReachableMpSet( antReachable.focus() );
                visitor->addToReachableMpSet( antReachable.opticalTel() );
                visitor->addToReachableMpSet( antReachable.rxSelector() );
            }

            colMcs.push_back( colMc.release() );

            columns.push_back(
                MonitorColumnInfo( colName, *(colMcs.back()) ) );
        }

        MonitorSingleTableFolderPtr folder(new MonitorSingleTableFolder(
                "Antennas",
                columns,
                AUTO_SIZE_EACH_COLUMN,
                0,           // hierarchy depth, 0 = all the way down
                0,           // sample index, 0 = average
                false,       // no units in row labels
                true,        // switch rows and olumns
                visitor ));

        // Add the folders to the display
        display.add( folder );
    }
}
