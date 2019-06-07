#include "carma/util/Program.h"
#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/obsRecord2/CorrDataConsumer.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <vector>

using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;
using namespace std;

class SimpleListener : public CorrDataConsumer::Listener {
public:

    SimpleListener( bool verbose );

    ~SimpleListener();

    void processData( carma::correlator::lib::CorrelatorData * cd );

private:

    const bool verbose_;

};

SimpleListener::SimpleListener( const bool verbose ) : verbose_( verbose ) { }

SimpleListener::~SimpleListener() { }

void 
SimpleListener::processData( lib::CorrelatorData * cd ) {

    const double nowMjd = Time::MJD();
    const double hdrMjd = cd->getHeader().getMJD(); 
    const double latencyInMs = ( nowMjd - hdrMjd ) * Time::MILLISECONDS_PER_DAY;

    cout << "Corr data size " << cd->getSizeInBytes() 
         << ", event latency " << latencyInMs << " ms." << endl; 

    if ( !verbose_ ) return;
    
    const vector< CorrelatorBand > & bands = cd->getBands();
    cout << "Processing data for band(s) " << endl;
    cout << "Header MJD " << cd->getHeader().getMJD() << endl;
    cout << "Seq # (frame #) " << cd->getHeader().getSequenceNumber() << endl;
    BOOST_FOREACH( const CorrelatorBand & band, bands ) {
        cout << "Band " <<  band.getBandNumber() 
             << " (MJD= " << band.getMJD() << ") ";

        cout << "contains " << band.getNumberOfInputs() << " inputs ";
        const BaselineVector baselines = band.getBaselines();
        cout << "and " << baselines.size() << " baselines." << endl;
        BOOST_FOREACH( const CorrelatorBaseline & bl, baselines ) {
            cout << "Baseline " << bl.getInput1Number() << "-" << bl.getInput2Number() << " contains ";
            const SidebandVector sidebands = bl.getSidebands();
            cout << sidebands.size() << " sidebands: " << flush;
            BOOST_FOREACH( const CorrelatorSideband & sb, sidebands ) {
                cout << sb.getNumberOfChans() << " chans / " 
                    << sb.getSizeInBytes() << " bytes, stats size "
                    << sb.getStats().getSizeInBytes() << " sizeof(complex) " 
                    << sizeof( DataVector::value_type )
                    << " avg (" << sb.getStats().getAvg().real() 
                    << ", " << sb.getStats().getAvg().imag() << "), " 
                    << "int time " << sb.getStats().getIntegrationTime() << " ms." << endl;
            }
        }

    }
    cout <<  endl;
}

/**
 *  @description Connect to a correlator notification channel and print crap.
 *
 *  @usage Usage: Use it. 
 *
 *  @key c "" string Notification channel name e.g. carma.correlator.slcb1.
 *  @key verbose false bool Print out as much information as possible.
 *
 *  @logger TEST_FACILITY carma.test.correlator.obsrecord2.tCobraCorrConsumer
 */
int 
Program::main( ) {

    if ( !haveImrHostname() ) {
        cerr << "Must specify imr (e.g. imr=acc)." << endl;
        return 1;
    }

    SimpleListener listener( getBoolParameter( "verbose" ) );

    const string objectName = Program::getStringParameter("c");

    CorrDataConsumer consumer( getCorbaServer(), objectName, listener );

    consumer.getData();

    return 0;
}
