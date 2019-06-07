#include "carma/util/Program.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/Time.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorTable.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/SignalPathMapping.h"
#include "carma/monitor/SignalPathSubsystem.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <vector>

using namespace carma;
using namespace carma::monitor;
using namespace carma::ui::rtd;
using namespace carma::util;
using namespace std;

class CurrentFrameCell : public Cell {
public:

    CurrentFrameCell( );

    ~CurrentFrameCell( );

    void update( );

};

CurrentFrameCell::CurrentFrameCell( ) :
    Cell( "11.0.11" ) { }

CurrentFrameCell::~CurrentFrameCell( ) { }

void
CurrentFrameCell::update( )
{
    setValidity( true );
    fmtOss_.str( string() );
    fmtOss_ << setw(format_.len) << Time::computeCurrentFrame( );
}


class AntRangeCell : public Cell {
public:

    AntRangeCell( const SignalPathMapping & spm,
                  const int astroBandNo );

    ~AntRangeCell( );

    void update( );

private:

    const SignalPathMapping & spm_;
    const int astroBandNo_;

}; // class AntRangeCell

AntRangeCell::AntRangeCell( const SignalPathMapping & spm,
                            const int astroBandNo ) :
    Cell( "15.0.15" ),
    spm_( spm ),
    astroBandNo_( astroBandNo )
{
    setPlottable( false );
}

AntRangeCell::~AntRangeCell( ) { }

void
AntRangeCell::update( ) 
{
    if ( !spm_.changed() )
        return;

    // Get mapped antenna pol pairs and sort them into ants by pol
    typedef map< PolType, set<int> > AntSetByPol;
    AntSetByPol antsByPol;

    const vector<AntPolPair> antpols = spm_.getMappedAntPolPairs(astroBandNo_);
    BOOST_FOREACH( const AntPolPair & app, antpols ) {
        antsByPol[ app.second ].insert( app.first );
    }
        
    string text;
    BOOST_FOREACH( const AntSetByPol::value_type & val, antsByPol ) {
        text += formatAsRanges( val.second );
        text += PolarizationMonitorPointEnum::valueToString( val.first );
        text += " ";
    }

    setValidity( true );
    fmtOss_.str( string() );

    // const string fmtants = formatAsRanges( ants, "C", "L" );
    fmtOss_ << setw(format_.len) << text; // << fmtants;
    Cell::updateColor();
}


RtTablePtr
makeSummaryTable( SignalPathSubsystem::Mapping & spm,
                  const SignalPathMapping & spmm )
{
    const int nAstrobands = SignalPathSubsystem::Mapping::astrobandCount(); 

    RtTablePtr table(new RtTable( "Signal Path Mapping Summary" ));

    // Add columns 
    MonitorPointIterator mpi( spm.astroband( 0 ), 1 );
    ++mpi; 
    while ( mpi.hasMonitorPoint() ) {
        table->addCol( RtColumn::makeColumn( mpi.getMonitorPoint().getShortName() ) );
        ++mpi;
    }
    table->addCol( RtColumn::makeColumn( "Ants" ) );
    
    // Add rows
    for ( int r = 0; r < nAstrobands; ++r ) {
        ostringstream rowLabel;
        rowLabel << "Astroband " << r + 1;
        table->addRow( RtRow::makeRow( rowLabel.str() ) );
    }

    // Now that geometry has been set, add cells.
    table->setReverseOrder();
    for ( int ab = 0; ab < nAstrobands; ++ab ) {
        
        MonitorPointIterator mpi( spm.astroband( ab ), 1 );
        ++mpi; 

        while ( mpi.hasMonitorPoint() ) {
            table->addCell( MonitorCell::makeCell( mpi.getMonitorPoint( )));
            ++mpi;
        }

        table->addCell(CellPtr(new AntRangeCell( spmm, ab + 1 )));
    }

    return table;
}


int Program::main() {
    
    MonitorDisplay display( "Signal Path Mapping Summary" );
    display.setSpecificHelp( "Signal Path Mapping Summary", "" );

    SignalPathSubsystem::Mapping & spm = display.cms().signalPath().mapping();
    SignalPathMapping spms( display.cms(), monitor::corrTypeToCorrDes(CORR_ALL) ); 

    RtFolderPtr folder(new RtFolder( "SPM Folder" ));
    RtAreaPtr area(new RtArea( "SPM Area" ));
    RtVBoxPtr box(new RtVBox( "SPM Box" ));
    RtLabelPtr header(new RtLabel( "SPM Summary" ));

    area->addItem( "Current Frame", CellPtr(new CurrentFrameCell( )));
    area->addItem( spm.lastModified().getShortName(),
                  MonitorCell::makeCell( spm.lastModified() ) );
    area->addItem( spm.hardwareConfValid().getShortName(),
                  MonitorCell::makeCell( spm.hardwareConfValid() ) );

    RtTablePtr abtable = makeSummaryTable( spm, spms );

    box->add( area );
    folder->add( box );
    folder->add( abtable );
    display.add( folder );

    while ( display.serveData() )
        spms.update();

    return 0;
}
