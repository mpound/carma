/**
 * $Id: tPipelineMonitorInput.cc,v 1.6 2014/06/04 17:09:25 mpound Exp $
 * 
 * @author Andy Beard
 * @version $Revision: 1.6 $
 * @description 
 * Read and output monitor data using the PipelineMonitorInput class.
 *
 * @usage 
 * $BUILD/carma/monitor/tPipelineMonitorInput (must run on an acc).
 *
 * @key corr "any" string One of \"sl\", \"wb\" or \"any\".
 * @key astroband 0 int Specific astroband to use or 0 for all.
 * @key antenna 0 int Specified antenna to use or 0 for all.
 * @key input 0 int Specified input to use or 0 for all.
 * @key pol "l" string One of \"l\" or \"r\".
 * @key frames 0 int Number of frames to run for (0 = forever).
 * 
 * @logger TEST_FACILITY carma.test.monitor.tPipelineMonitorInput
 */

#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/util/CorrelatorType.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/ExceptionUtils.h"

#include <boost/foreach.hpp>
#include <iostream>

using namespace carma::monitor;
using namespace carma::util;
using namespace std;

int
Program::main( )
try {

    const string corrString = getStringParameter( "corr" );
    const MonitorCorrelatorDesignation corr = 
        ( corrString == "any" ?  corrTypeToCorrDes(CORR_ALL) :  
        ( corrString == "sl"  ?  corrTypeToCorrDes(CORR_SPECTRAL) : 
        ( corrString == "wb"  ?  corrTypeToCorrDes(CORR_WIDEBAND): 
          corrTypeToCorrDes(CORR_NONE) ) ) );
    const int astroband = getIntParameter( "astroband" );
    const int antenna = getIntParameter( "antenna" );
    const int input = getIntParameter( "input" );
    const int maxFrames = getIntParameter( "frames" );
    const string polString = getStringParameter( "pol" );
    const PolType pol = ( polString == "r" ? PolMPE::R : PolMPE::L );

    if ( corr == corrTypeToCorrDes(CORR_NONE) ) {
        cerr << "Invalid corr designation." << endl;
        return 1;
    }

    if ( astroband < 0 || 
         astroband > SignalPathSubsystem::Mapping::astrobandCount() ) {
        cerr << "Invalid astroband." << endl;
    }

    if ( antenna < 0 || antenna > 23 ) {
        cerr << "Invalid antenna." << endl;
    }

    PipelineMonitorInput pmi( corr );

    int frames = 0;
    unsigned int fc = Time::computeClosestFrame();
    
    while ( frames++ < maxFrames || maxFrames == 0 ) {

        ++fc;

        cout << "Waiting for " << fc << "." << endl;
        const bool dataReceived = pmi.waitForInput( fc );

        if ( !dataReceived ) {
            cout << "Timed out or otherwise missed." << endl;
            continue;
        }
            
        ostringstream oss;
        oss << "Input received for fc=" << pmi.frameCount() << endl;

        vector< int > validAstroBandNums;
        if ( astroband == 0 ) {
            validAstroBandNums = pmi.getMappedAstroBandNumbers( );
        } else {
            validAstroBandNums.push_back( astroband );
        }

        BOOST_FOREACH( const int & abno, validAstroBandNums ) {

            if ( ! pmi.signalPathMapped( abno ) ) {
                oss << "Astroband " << abno << " not mapped." << endl;
                continue;
            }

            oss << "Astroband " << abno << " psys: " << endl;
            
            vector< AntPolPair > validAntPols;
            if ( antenna == 0 ) {
                validAntPols = pmi.getMappedAntPolPairs(abno);
            } else {
                validAntPols.push_back( AntPolPair( antenna, pol ) );
            }

            BOOST_FOREACH( const AntPolPair & pair, validAntPols ) {
                const string polString = PolMPE::valueToString( pair.second );
                if ( ! pmi.signalPathMapped( abno, pair ) ) {
                    oss << "AntPol "
                        << pair.first << polString << " not mapped." << endl;
                    continue;
                }
                oss << pair.first << polString << " "
                    << pmi.totalPower( abno, pair ).getValue() << " ";
            }

            oss << endl;

            int inputStart, inputEnd;
            if ( input == 0 ) {
                inputStart = 1; 
                inputEnd = 32;
            } else {
                inputStart = input;
                inputEnd = input + 1;
            }
            for ( int i = inputStart; i < inputEnd; ++i ) {
                const AntPolPair pair = pmi.getAntPolPair( abno, i );
                const string polString = PolMPE::valueToString( pair.second );
                if ( ! pmi.signalPathMapped( abno, pair ) ) {
                    oss << "Input " << i << " not mapped." << endl;
                    continue;
                }
                oss << "Input " << i << " maps to " << pair.first 
                    << polString << " ";
                
            }
            oss << endl;
            
        }

        cout << oss.str() << endl;

    } 

    return 0;

} catch (...) {
    cerr << getStringForCaught() << endl;
    return 1;
}
