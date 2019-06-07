// $Id: visBrickReader.cc,v 1.4 2012/08/14 22:03:17 eml Exp $

#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorHeader.h"
#include "carma/pipeline/VisBrickReader.h"
#include "carma/util/complexManip.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <sstream>
#include <cerrno>
#include <string>

using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::util;
using namespace std;

namespace {
    
    typedef vector< carma::correlator::lib::CorrelatorBand> CorrBands;
    typedef vector< carma::correlator::lib::CorrelatorBaseline> CorrBaselines;
    typedef vector< ::std::complex< float > > Spectra;
    
    typedef enum {
        UPPER,
        LOWER,
        BOTH,
        AUTO } SidebandType;

    struct BaselineArg {
        int input1;
        int input2;
        SidebandType sideband;
    };

    string
    printRecordDetails( const frameType            intNum,
                        const CorrelatorData &     data )
    {
        const int secsPrecision = 1;
        ostringstream oss;
        
        oss << "Integration #" << intNum << " (" 
            << Time::getTimeString( intNum, secsPrecision ) << ") contains";

        const CorrBands & bands = data.getBands( );

        oss << " " << bands.size( ) << " band" 
            << ( bands.size( ) > 1 ? "s" : "" ) << endl; 

        BOOST_FOREACH( const CorrelatorBand & band, bands ) { 
            oss << " " << band.getSummary() << endl;
        }

        return oss.str( );
    } // Print record details

    string
    printBandDetails( const CorrelatorBand & band ) 
    { 
        ostringstream oss;

        oss << band.getSummary( ) << endl;

        const CorrBaselines & baselines = band.getBaselines(); 

        if (static_cast<int>(baselines.size()) != band.getNumberOfBaselines()){

            oss << " Baseline vector size does not match band reported count!"
                << endl;
            return oss.str();
        }

        BOOST_FOREACH( const CorrelatorBaseline & baseline, baselines ) {
            oss << " " << baseline.getSummary( ) << endl;
        }

        return oss.str();
    } // printBandDetails

    string
    printBaselineDetails( const CorrelatorBaseline & baseline,
                          const SidebandType sbType )
    {
        ostringstream oss;

        oss << baseline.getSummary() << endl;

        if ( baseline.getNumberOfSidebands() == 0 ) {
            oss << " Contains no sidebands." << endl;
            return oss.str();
        }

        if ( sbType == BOTH ) {
            const SidebandVector & sidebands = baseline.getSidebands();

            BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {
                oss << " " << sideband.getSummary() << endl;
                oss << " " << sideband.getStats().getSummary() << endl;
                oss << " " << sideband.getSpectra();
            }
        } else if ( sbType == UPPER ) {
            const CorrelatorSideband & sideband = baseline.getUpperSideband();
            oss << " " << sideband.getSummary() << endl;
            oss << " " << sideband.getStats().getSummary() << endl;
            oss << " " << sideband.getSpectra();
        } else if ( sbType == LOWER ) {
            const CorrelatorSideband & sideband = baseline.getLowerSideband();
            oss << " " << sideband.getSummary() << endl;
            oss << " " << sideband.getStats().getSummary() << endl;
            oss << " " << sideband.getSpectra();
        } else if ( sbType == AUTO ) {
            const CorrelatorSideband & sideband = baseline.getAutoSideband();
            oss << " " << sideband.getSummary() << endl;
                oss << " " << sideband.getStats().getSummary() << endl;
            oss << " " << sideband.getSpectra();
        } 

        return oss.str();
    }

    string  
    shortSummary( const RecordsByFrameMap & recs )
    {
        const int secsPrec = 1;

        ostringstream oss;

        if ( recs.size( ) == 0 ) {
            oss << "Visbrick is empty." << endl;
            return oss.str( );
        }

        const string vbBeginTime = Time::getTimeString(
                recs.begin( )->first, secsPrec );
        const string vbBeginDate = Time::getDateString( 
                recs.begin( )->first, "%b %d %Y" );

        const string vbEndTime = Time::getTimeString(
                recs.rbegin( )->first, secsPrec );
        const string vbEndDate = Time::getDateString( 
                recs.rbegin( )->first, "%b %d %Y" );

        oss << endl;
        oss << "Visbrick contains " << recs.size( ) << " record(s) from "
            << vbBeginDate << " " << vbBeginTime << " to ";

        if ( vbBeginDate != vbEndDate ) {
            oss << vbEndDate << " ";
        }

        oss << vbEndTime << "." << endl;

        oss << "Note that integration frame number represents the beginning "
            << "of the integration." << endl;

        return oss.str( );
    } // shortSummary

    string  
    longSummary( const RecordsByFrameMap & recs )
    {
        const int secsPrec = 1;

        ostringstream oss;

        RecordsByFrameMap::const_iterator rBegin = recs.begin();
        RecordsByFrameMap::const_iterator rEnd = recs.end();
        for ( RecordsByFrameMap::const_iterator r = rBegin; r != rEnd; ++r ) {
            const CorrBands & bands = r->second->getBands();
            oss << "Integration # " << r->first << " ("
                << Time::getTimeString( r->first, secsPrec )  << ") "
                << "contains " << bands.size()
                << " band" << ( bands.size() > 1 ? "s" : "" ) << "." << endl;
        }

        return oss.str();

    } // longSummary
            
    string  
    collatedSummary( const RecordsByFrameMap & recs )
    {
        const int secsPrec = 1;
        ostringstream oss;

        if ( recs.size( ) == 0 ) {
            oss << "Visbrick is empty." << endl;
            return oss.str( );
        }
        
        // Form a map of frame vs. delta to next frame.
        typedef map< frameType, frameType > DeltaMap;
        DeltaMap deltaMap;
        
        RecordsByFrameMap::const_iterator i = recs.begin( );
        RecordsByFrameMap::const_iterator j = recs.begin( );
        const RecordsByFrameMap::const_iterator jEnd = recs.end( );
        ++j;
        for ( ; j != jEnd; ++i, ++j ) {
            deltaMap.insert( DeltaMap::value_type( i->first, 
                                                   j->first - i->first  ) );
        }
        
        // Loop over records, if contiguous, tally and repeat, else print.
        // Note that since there is no nominal integration time in a correlator
        // data object, we must determine contiguous records by looking ahead
        // two records and comparing the frame difference between them. 
        int contiguousRecords = 2;
        frameType delta = 0;
        i = recs.begin( );
        const RecordsByFrameMap::const_iterator iEnd = recs.end( );
        while ( i != iEnd ) { 

            DeltaMap::iterator d = deltaMap.find( i->first );

            contiguousRecords = 2;

            if ( d != deltaMap.end( ) ) {
                delta = d->second;
                
                for ( ++d; d != deltaMap.end( ) && d->second == delta ; ++d ) {
                    ++contiguousRecords; 
                }
            }
                
            oss << " Integration #" << i->first << " (" 
                << Time::getTimeString( i->first, secsPrec ) << ")";
            
            if ( contiguousRecords > 2 ) {
                // Catch iterator up to next non-contiguous record.
                for ( int c = 0; c < contiguousRecords - 1; ++c ) ++i;

                oss << " to #" << i->first << " - " << contiguousRecords 
                    << " consecutive " << ( delta / 2.0 ) << "s records."
                    << endl;
            } else {
                oss << " - 1 record." << endl;
            } 

            ++i; // Increment iterator to next non contiguous record 
        }

        oss << endl;

        return oss.str( );

    } // collatedSummary 

    frameType
    stringToFrameType( const string frameString ) 
    {
        errno = 0;

        long long answer = strtol( frameString.c_str( ), 0, 10 );

        int localErrno = errno;

        if ( localErrno != 0 ) {
            ostringstream err;

            err << "Unable to convert frameString '" << frameString << "' to "
                << "frameType. " << strerror( localErrno ) << ".";

            cerr << err.str( ) << endl;

            throw CARMA_ERROR( err.str( ) ); 
        }

        return static_cast< frameType >( answer );
    } // stringToFrameType

    bool 
    verifyVisBrickCorrectness( const RecordsByFrameMap & recs )
    {
        unsigned bytes = 0;
        bool correct = true;

        // Loop over all records, bands, baselines, sidebands and channels and 
        // verify that the data indeed matches the headers.
        const RecordsByFrameMap::const_iterator rBegin = recs.begin();
        const RecordsByFrameMap::const_iterator rEnd = recs.end();
        for ( RecordsByFrameMap::const_iterator r = rBegin; r != rEnd; ++r )
        {
            const frameType frame = r->first; 
            CorrelatorData * cdp = r->second;

            const CorrBands & bands = cdp->getBands();
            if ( cdp->getNumberOfBands() != static_cast<int>(bands.size( )) ) {
                cerr << "! frame " << frame << " band size mismatch." << endl;
                correct = false;
            }

            const CorrBands::const_iterator bBegin = bands.begin();
            const CorrBands::const_iterator bEnd = bands.end();
            for ( CorrBands::const_iterator b = bBegin; b != bEnd; ++b ) {

                // Check that the band number is what we think it is...
                const int bandNumber = b->getBandNumber();
                if ( bandNumber != cdp->getBand( bandNumber ).getBandNumber() ){
                    cerr << "! frame " << frame << " band numbers are "
                        << "seriously amiss!." << endl;
                    correct = false;
                }
                
                const CorrBaselines & baselines = b->getBaselines();
                if ( b->getNumberOfBaselines() != 
                     static_cast<int>(baselines.size()) ) {
                    cerr << "! frame " << frame << " baseline size mismatch." 
                        << endl;
                    correct = false;
                }

                const CorrBaselines::const_iterator blBegin = 
                    baselines.begin();
                const CorrBaselines::const_iterator blEnd = baselines.end();
                for ( CorrBaselines::const_iterator bl = blBegin; 
                      bl != blEnd; ++bl ) {

                    const SidebandVector & sidebands = bl->getSidebands();
                    if ( bl->getNumberOfSidebands() != 
                         static_cast<int>( sidebands.size() ) ) {
                        cerr << "! frame " << frame << " sideband size "
                            << "mismatch." << endl;
                        correct = false;
                    }

                    // Check that getUpper, getLower and getAutoSideband
                    // routines actually return what they say they do.
                    const int input1 = bl->getInput1Number();
                    const int input2 = bl->getInput2Number();
                    if ( input1 == input2 ) {
                        const CorrelatorSideband & autoSb = 
                            bl->getAutoSideband();
                        if ( !autoSb.isAuto() ) {
                            cerr << "! baseline " << input1 << "-" << input2
                                << " is an auto sideband but fails isAuto()!."
                                << endl;
                            correct = false;
                        } 
                    } else { 
                        try { 
                        const CorrelatorSideband & upperSb = 
                            bl->getUpperSideband();
                        const CorrelatorSideband & lowerSb = 
                            bl->getLowerSideband();
                        if ( !upperSb.isUSB() || !lowerSb.isLSB() ) {
                            cerr << "! baseline " << input1 << "-" << input2
                                << " is a cross correlation baseline but "
                                << "either USB or LSB fails isUSB() or isLSB()!"
                                << endl;
                            correct = false;
                        }
                        } catch (...) {
                            cerr << "! exception while trying to retrieve" 
                                << " sideband for " << input1 << "-" << input2 << endl;
                            continue;
                        }
                    }


                    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {

                        const Spectra & spectra = sideband.getData( );
                        if ( sideband.getNumberOfChans() != 
                             static_cast<int>( spectra.size() ) ) {
                            cerr << "! frame " << frame << " spectra size "
                                << "mismatch." << endl;
                            correct = false;
                        }

                        bytes += ( sideband.getNumberOfChans() * 
                                   sizeof( complex<float> ) ); 
                    }
                            
                } // Loop over baselines
            } // Loop over bands
        } // Loop over records
        return correct;
    }

    // Throws IllegalArgumentException if parsing fails.
    BaselineArg
    parseBaselineString( const string & baselineString )
    {
        BaselineArg answer;

        const string::size_type 
            delineatorPosition = baselineString.find_first_of( "-" );
        const string::size_type 
            input2LastPosition = baselineString.find_last_of( "1234567890" );

        if ( delineatorPosition == string::npos ||
             input2LastPosition == string::npos ||
             input2LastPosition < delineatorPosition ) {
            throw CARMA_EXCEPTION( IllegalArgumentException,
                "Baseline argument must be of form m-n[UL] where m and n "
                "are input numbers and U or L optionally designates Upper "
                "or Lower sideband." );
        }

        const string::difference_type input2StringLength = 
            input2LastPosition - delineatorPosition;

        const string::size_type
            flavorPosition = baselineString.find_first_of( "ULul" );

        const string input1String = baselineString.substr( 0,
                                                           delineatorPosition );
        const string input2String = 
            baselineString.substr( delineatorPosition + 1,
                                   input2StringLength );

        istringstream in1( input1String );
        in1 >> answer.input1;
        istringstream in2( input2String );
        in2 >> answer.input2;

        ostringstream parsed;
        parsed << answer.input1 << "-" << answer.input2;

        if ( flavorPosition == string::npos ) {
            if ( answer.input1 == answer.input2 ) {
                answer.sideband = AUTO;
            } else {
                answer.sideband = BOTH;
            }
        } else {

            const string flavorString = baselineString.substr( flavorPosition );

            if ( flavorString == "U" ) {
                answer.sideband = UPPER;
                parsed << "U";
            } else if ( flavorString == "u" ) {
                answer.sideband = UPPER;
                parsed << "u";
            } else if ( flavorString == "L" ) {
                answer.sideband = LOWER;
                parsed << "L";
            } else if ( flavorString == "l" ) {
                answer.sideband = LOWER;
                parsed << "l";
            }
        }

        if ( baselineString != parsed.str() ) {
            throw CARMA_EXCEPTION( IllegalArgumentException,
                "Baseline argument must be of form m-n[UL] where m and n "
                "are input numbers and U or L optionally designates Upper "
                "or Lower sideband." );
        }  

        return answer;
    }
        
} // namespace <unnamed>

/**
 * @author Andy Beard
 *
 * @version $Id: visBrickReader.cc,v 1.4 2012/08/14 22:03:17 eml Exp $
 *
 * @description
 * \nReads specified visbrick and outputs various summaries.  When no options
 *   prints a collated summary of records.
 *
 * @usage visBrickReader file=<visbrick file> [frame=integ# [band=<band #>] 
 *
 * @key file @mandatory string Visbrick filename. 
 * @key frame @noDefault string Print details about a specific integration.
 *         \n\tIntegrations are labeled by frame number.
 * @key band @noDefault int Print details about a band (requires frame).
 * @key baseline @noDefault string Print data about a baseline (requires band).
 *         \n\tString in m-n form with optional sideband delineator of U or 
 *         \n\tL for Upper or Lower sideband (default is both).  For example 
 *         \n\t'1-2U' specifies baseline 1-2 upper sideband.
 * @key verify true bool Verify visbrick correctness & exit with warning if not.
 * @key long false bool Print details on every record in summary.
 * 
 * @logger DEFAULT_FACILITY carma.pipeline.visBrickReader
 */
int Program::main( ) 
try {
    const string filename = getStringParameter( "file" );

    const bool continueOnFileErrors = true;
    CorrelatorVisBrickReader reader( filename, continueOnFileErrors );
    const RecordsByFrameMap recs = reader.getRecordsKeyedByFrame();

    if ( recs.empty() ) {
       cerr << "Visbrick is empty." << endl;
       return 1;
    }

    if ( getBoolParameter( "verify" ) ) {
        cout << "Verifying visbrick correctness..." << flush;
        const bool correct = verifyVisBrickCorrectness( recs );
        if ( correct ) {
            cout << " correctness verified." << endl;
            sleep( 2 );
        } else {
            cerr << "Visbrick is NOT correct." << endl;
            return 1;
        }
    }

    // Now parse and check everything else.
    const bool frameSpecified = parameterWasSpecified( "frame" );
    const bool bandSpecified = parameterWasSpecified( "band" );
    const bool baselineSpecified = parameterWasSpecified( "baseline" );
    const bool printLongSummary = getBoolParameter( "long" ); 

    int band = -1;
    BaselineArg baselineArg;
    frameType frame;

    if ( reader.fileErrorDetected() ) {
        cerr << endl << "File error detected while reading " << filename 
            << ". Please check the logs for more information. "
            << recs.size() << " records were extracted from the visbrick "
            << "prior to the file error - continuing with these records." 
            << endl;
     }

    if ( frameSpecified ) {
        // Frame is represented as a string to allow full range conversion to 
        // the unsigned frameType.
        const string frameString = getStringParameter( "frame" );
        frame = stringToFrameType( frameString ); 

        if ( bandSpecified ) {
            band = getIntParameter( "band" );
            
            if ( baselineSpecified ) {
                const string baselineString = getStringParameter( "baseline" );
                baselineArg = parseBaselineString( baselineString );
            }
        } else { // !bandSpecified
            if ( baselineSpecified ) {
                cerr << "You must specify a band with the baseline option." 
                    << endl;
                return 1;
            }
        } // if bandSpecified

    } else { // !frameSpecified
    
        if ( bandSpecified ) {
            cerr << "You must specify a frame with the band option." << endl;
            return 1;
        } 

        if ( baselineSpecified ) {
            cerr << "You must specify a frame (and band) with the baseline "
                << "option." << endl;
            return 1;
        }
    }
    
    ostringstream oss;
    
    oss << shortSummary( recs );

    if ( frameSpecified ) {

        RecordsByFrameMap::const_iterator i = recs.find( frame );
        if ( i == recs.end( ) ) {  
            cerr << "Integration #" << frame << " was not found." << endl;
            return 1;
        }

        oss << printRecordDetails( frame, *( i->second ) ) << endl; 
        
        oss << i->second->getHeader().getSummary() << endl;

        if ( bandSpecified ) {
        
            const CorrelatorBand & bandObj = i->second->getBand( band );

            if ( baselineSpecified ) {
                const CorrelatorBaseline & baseline = bandObj.getBaseline( 
                    baselineArg.input1,
                    baselineArg.input2 );
                
                oss << printBaselineDetails( baseline, baselineArg.sideband ) 
                    << endl;
            } else {
                oss << printBandDetails( bandObj ) << endl;
            }

        }

    } else {

        if ( printLongSummary ) 
            oss << longSummary ( recs ) << endl;  
        else 
            oss << collatedSummary( recs ) << endl;
    }
    
    cout << oss.str();

    return 0;
} catch (...) {
    string errMsg( getStringForCaught() ); 
    cerr << endl << errMsg << endl;
    return 1;
} // Program::main


