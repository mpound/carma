#include "carma/ui/rtd/windows/controlReachable.h"

#include "carma/monitor/MonitorContainer.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/reachableCellUtils.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::ui;
using namespace carma::ui::rtd;


namespace {


class NotApplicableCell : public CellString {
    public:
        explicit NotApplicableCell( );

    private:
        static const Format    kFormat_;
        static const string    kValueString_;
        static const CellColor kColor_;
};


const Format    NotApplicableCell::kFormat_( "3.0.3" );
const string    NotApplicableCell::kValueString_ = "N/A";
const CellColor NotApplicableCell::kColor_ = LIGHT_GRAY_CELL_COLOR;


NotApplicableCell::NotApplicableCell( ) :
CellString( kFormat_, kValueString_, kColor_ )
{
    setValidity( true );
}


CellPtr
makeReachableCell( MonitorPointBool & mpBool,
                   const bool         applicable )
{
    if ( applicable == false )
        return CellPtr(new NotApplicableCell);

    const size_t width = calcReachableCellWidth();

    MonitorCellPtr monitorCell = MonitorCell::makeCell( width, mpBool );

    MonitorCellBool * const monitorCellBool =
        dynamic_cast< MonitorCellBool * >( monitorCell.get() );

    if ( monitorCellBool == 0 )
        programLogErrorIfPossible( "Monitor cell was not a MonitorCellBool" );
    else
        postprocessReachableCell( *monitorCellBool );

    return monitorCell;
}


}  // namespace < anonymous >


void
carma::ui::rtd::addControlReachableFolder( MonitorDisplay & display )
{
    const int saCount = ControlSubsystem::subarrayCount();

    RtTablePtr table(new RtTable("Subarrays"));

    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const string saName =
            ControlSubsystem::getSubarrayAlphanumericName( saNo );

        table->addCol( RtColumn::makeCol( saName ) );
    }

    table->addRow( RtRow::makeRow( "Alarm" ) );
    table->addRow( RtRow::makeRow( "LO Ref" ) );
    table->addRow( RtRow::makeRow( "Loberotator" ) );
    table->addRow( RtRow::makeRow( "Line Length" ) );
    table->addRow( RtRow::makeRow( "Master Clock" ) );
    table->addRow( RtRow::makeRow( "Fault System" ) );
    table->addRow( RtRow::makeRow( "PDB Manager" ) );
    table->addRow( RtRow::makeRow( "Signal Path" ) );
    table->addRow( RtRow::makeRow( "CorrData Remapper" ) );
    table->addRow( RtRow::makeRow( "DC System" ) );
    table->addRow( RtRow::makeRow( "DC Control" ) );
    table->addRow( RtRow::makeRow( "Noise Source" ) );
    table->addRow( RtRow::makeRow( "Quad Mod" ) );
    table->addRow( RtRow::makeRow( "Pipeline" ) );

    for ( int bandNo = 1; bandNo <= 16; ++bandNo ) {
        ostringstream rowLabel;

        rowLabel << "Correlator Band " << bandNo;

        table->addRow( RtRow::makeRow( rowLabel.str() ) );
    }

    for ( int bandNo = 1; bandNo <= 8; ++bandNo ) {
        ostringstream rowLabel;

        rowLabel << "VLBI Band " << bandNo;

        table->addRow( RtRow::makeRow( rowLabel.str() ) );
    }

    table->addRow( new RtRow( "C3G Max23 control"));
    table->addRow( new RtRow( "C3G Max8 control"));


    for ( int saNo = 1; saNo <= saCount; ++saNo ) {
        const ControlSubsystemBase::Reachable & saReachable =
                display.cms().control().subarray( saNo - 1 ).reachable();
        
        //@FIXME MWP -- correlators no longer tied to subarrays.
        //This RTD folder should become dynamic based on
        //correlator ownership.  Since nbands is large now, 
        //consider making a separate "correlator reachability" 
        //folder?
        const bool spectral = (saNo == 1);
        const bool wideband = (saNo == 2);

        {
            CellPtr alarm = makeReachableCell( saReachable.alarm(), true );
            table->add( alarm );
        }

        {
            CellPtr loRef = makeReachableCell( saReachable.loRef(), ((saNo >= 1) && (saNo <= 3)) );
            table->add( loRef );
        }

        {
            CellPtr loberotator = makeReachableCell( saReachable.loberotator(), true );
            table->add( loberotator );
        }

        {
            CellPtr linelength = makeReachableCell( saReachable.linelength(), ((saNo == 1) || (saNo == 2)) );
            table->add( linelength );
        }

        {
            CellPtr clock = makeReachableCell( saReachable.clock(), ((saNo == 1) || (saNo == 2)) );
            table->add( clock );
        }

        {
            CellPtr fault = makeReachableCell( saReachable.fault(), (saNo == 1) );
            table->add( fault );
        }

        {
            CellPtr pdb = makeReachableCell( saReachable.projectDatabaseManager(), true );
            table->add( pdb );
        }

        {
            MonitorPointBool & signalPathMp = saReachable.signalPath();
            CellPtr signalPath = makeReachableCell( signalPathMp , true );
            table->add( signalPath );
        }

        {
            MonitorPointBool & corrDataRemapperMp = saReachable.corrDataRemapper();
            CellPtr corrDataRemapper = makeReachableCell( corrDataRemapperMp , true );
            table->add( corrDataRemapper );
        }

        if ( spectral || wideband ) {
            {
                MonitorPointBool & dcSystemMp =
                    (spectral ? saReachable.sldcSystem() :
                                saReachable.wbdcSystem());

                CellPtr dcSystem = makeReachableCell( dcSystemMp, true );
                table->add( dcSystem );
            }

            {
                MonitorPointBool & dcControlMp =
                    (spectral ? saReachable.sldcControl() :
                                saReachable.wbdcControl());

                CellPtr dcControl = makeReachableCell( dcControlMp, true );
                table->add( dcControl );
            }

            {
                CellPtr noiseSource = makeReachableCell( saReachable.noiseSource(), true );
                table->add( noiseSource );
            }

            {
                CellPtr quadMod = makeReachableCell( saReachable.quadMod(), true );
                table->add( quadMod );
            }

            {
                MonitorPointBool & pipelineMp =
                    (spectral ? saReachable.slPipeline() :
                                saReachable.wbPipeline());

                CellPtr pipeline = makeReachableCell( pipelineMp, true );
                table->add( pipeline );
            }

            for ( int bandNo = 1; bandNo <= 16; ++bandNo ) {
                if ( spectral ) {
                    if ( bandNo <= 8 ) {
                        CellPtr band = makeReachableCell( saReachable.slcorBand( bandNo - 1), true );
                        table->add( band );
                    } else {
                        CellPtr band(new NotApplicableCell);
                        table->add( band );
                   }
                } else {
                    CellPtr band = makeReachableCell(
                            saReachable.wbcorBand( bandNo - 1),
                            true );
                    table->add( band );
                }
            }



            for ( int bandNo = 1; bandNo <= 8; ++bandNo ) {
                if ( spectral ) {
                        CellPtr vlbiband = makeReachableCell(
                                saReachable.vlbiBand( bandNo - 1),
                                true );
                        table->add( vlbiband );
                } else {
                    CellPtr vlbiband(new NotApplicableCell);
                    table->add( vlbiband );
               }
            }
        } else { // not spectral or wideband
            {
                CellPtr dcSystem(new NotApplicableCell);
                table->add( dcSystem );
            }

            {
                CellPtr dcControl(new NotApplicableCell);
                table->add( dcControl );
            }

            {
                CellPtr noiseSource(new NotApplicableCell);
                table->add( noiseSource );
            }

            {
                CellPtr quadMod(new NotApplicableCell);
                table->add( quadMod );
            }

            {
                CellPtr pipeline(new NotApplicableCell);
                table->add( pipeline );
            }

            for ( int bandNo = 1; bandNo <= 16; ++bandNo ) {
                CellPtr band(new NotApplicableCell);
                table->add( band );
            }

            for (int bandNo = 1; bandNo <= 8; ++bandNo ) {
                CellPtr vlbi(new NotApplicableCell);
                table->add( vlbi );
            }
        }
            if (spectral) {
                Cell * const band = makeReachableCell(
                            saReachable.carma3gMax23(),
                            true );
        
                    rtObjsToDelete.push_back( band );
                    table->add( band );
            } else {
                    NotApplicableCell * const na = new NotApplicableCell;
                    rtObjsToDelete.push_back( na );
                    table->add( na );
            }

            if ( wideband ) {
                Cell * const band = makeReachableCell(
                            saReachable.carma3gMax8(),
                            true );
        
                    rtObjsToDelete.push_back( band );
                    table->add( band );
            } else {
            
                    NotApplicableCell * const na = new NotApplicableCell;
                    rtObjsToDelete.push_back( na );
                    table->add( na );
            }
    }

    const int numRows = table->getNumRows();

    RtSpringPtr spring(new RtSpring( 4, 1.0 ));

    RtFolderPtr folder(new RtFolder( "Reachability" ));

    folder->add( table );
    folder->add( spring );

    table->setMinRows( numRows );
    table->setPrefRows( numRows );

    display.add( folder );
}
