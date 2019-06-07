#include "carma/util/Program.h"
#include "carma/correlator/lib/CorrelatorData.h"

#include <boost/foreach.hpp>
#include <cassert>

using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::correlator::lib;

namespace {
    typedef vector< std::complex< float > > Spectra;

    void testSerialization( CorrelatorData & cd ) { 
        cout << "Testing serialization..." << endl;

        vector< char > bytes, reserializedCorrDataBytes;

        cout << "  serializing CorrelatorData object... " << flush;
        cd.serialIntoByteVec( bytes ); 

        cout << "contains " << bytes.size() << " bytes." << endl;

        CorrelatorData reserializedCorrData;

        cout << "  deserializing then reserializing CD object... " << flush;
        reserializedCorrData.deserial( bytes );
        reserializedCorrData.serialIntoByteVec( reserializedCorrDataBytes );

        cout << "contains " << reserializedCorrDataBytes.size() << " bytes." 
             << endl;

        assert( bytes.size() == reserializedCorrDataBytes.size() );

    }

}

//
// @version $Revision: 1.3 $
//
// @usage Usage: tSerialization
//
// @description
// Tests serialization of CorrelatorData object under different circumstances.
//
//  @key iters 100 int
//       Number of iterations per test
//
//  @key verbose false bool
//       Whether or not to produce verbose output
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorrelatorDataSerialization
//

int Program::main( )
{
    Spectra usbSpectra, lsbSpectra, asbSpectra;
    const Spectra::size_type channelCount = 81;
    
    for ( Spectra::size_type c = 0; c < channelCount; ++c ) {
        // Count up for lsb, down for usb and dc asb 
        usbSpectra.push_back( complex< float >( c, 0.0 ) );
        lsbSpectra.push_back( complex< float >( channelCount - c, 0.0 ) );
        asbSpectra.push_back( complex< float >( 1.0, 0.0 ) ); 
    }

    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    usb.setData( usbSpectra );
    lsb.setData( lsbSpectra );
    asb.setData( asbSpectra );

    // Mix it up a little for #2
    CorrelatorSideband usb2( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb2( CorrelatorSideband::LOWER_FLAVOR );
    CorrelatorSideband asb2( CorrelatorSideband::AUTO_FLAVOR );
    usb2.setData( lsbSpectra );
    lsb2.setData( usbSpectra );
    asb2.setData( asbSpectra );
    asb2.addIn( asb );
           
    CorrelatorBaseline crossBaseline1, crossBaseline2;
    crossBaseline1.setInput1Number( 1 );
    crossBaseline1.setInput2Number( 2 );
    crossBaseline1.addSideband( usb );
    crossBaseline1.addSideband( lsb );
    crossBaseline2.setInput1Number( 1 );
    crossBaseline2.setInput2Number( 2 );
    crossBaseline2.addSideband( usb2 );
    crossBaseline2.addSideband( lsb );

    try {
        cout << "Trying to add auto spectra to cross baseline...";
        crossBaseline2.addSideband( asb2 ); // Causes deserialization errors!
    } catch (...) {
        cout << " throws as expected." << endl;
    }

    crossBaseline2.addIn( crossBaseline1 );

    CorrelatorBaseline autoBaseline;
    autoBaseline.setInput1Number( 1 );
    autoBaseline.setInput2Number( 1 );
    autoBaseline.addSideband( asb2 );
    try { 
        cout << "Trying to add cross spectra sideband to auto baseline...";
        autoBaseline.addSideband( usb2 );
        assert( false );
    } catch (...) {
        cout << " throws as expected." << endl;
    }


    CorrelatorData data;
    for ( int b = 0; b < 8; ++b ) {
        CorrelatorBand band;

        band.setBandNumber( b + 1 );
        band.addBaseline( crossBaseline2 );
        band.addBaseline( autoBaseline );

        data.addBand( band );
    }

    CorrelatorData data2 = data;
    data.addIn( data2 );


    cout << "Constructed CorrelatorData object with: " 
        << data.getBands().size() << " bands (" << flush;
    const BandVector & bands = data.getBands();
    BOOST_FOREACH( const CorrelatorBand & band, bands ) 
        cout << band.getBandNumber() << " "; 
    cout << ")." << endl; 
     
     /* 
    cout << "Usb contains: " << flush;
    BOOST_FOREACH( complex< float > & c, nsUsb.getData() ) 
        cout << c << " ";
    cout << endl;
    
    cout << "Lsb contains: " << flush;
    BOOST_FOREACH( complex< float > & c, nsLsb.getData() ) 
        cout << c << " ";
    cout << endl;
    
    cout << "Asb contains: " << flush;
    BOOST_FOREACH( complex< float > & c, nsAsb.getData() ) 
        cout << c << " ";
    cout << endl;
    */

    testSerialization( data );

    return 0;

} 
    
