#include "carma/util/Program.h"

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl.h"
#include "carma/control/Subarray.h"
#include "carma/observertools/ItemValue.h"
#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/util/corbaSequenceUtils.h"
#include "carma/corba/Client.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace ::CORBA;
using namespace carma;
using namespace carma::control;
using namespace carma::util;
using namespace carma::observertools;


namespace {


SubarrayControl_var
retrieveSac( const int saNo )
{
    const string saName = Subarray::makeName( SUBARRAY_CONTROL_NAME, saNo );
    
    corba::Client & client = Program::getProgram().getCorbaClient();
    SubarrayControl_var result =
            client.resolveName< SubarrayControl >( saName );
            
    if ( CORBA::is_nil( result ) )
        throw runtime_error( "Could not retrieve " + saName );
    
    return result;
}


struct ThreadArgs {
    const int             threadNo;
    const int             saNo;
    const vector< short > antVec;
    const float           tmo;
    const bool            catalogTest;
    const bool            opticalPointSim;
    const bool            pdbTest;
    
    explicit ThreadArgs( int                     inThreadNo,
                         int                     inSaNo,
                         const vector< short > & inAntVec,
                         float                   inTmo,
                         bool                    inCatalogTest,
                         bool                    inOpticalPointSim,
                         bool                    pdbTest );
};


ThreadArgs::ThreadArgs( const int               inThreadNo,
                        const int               inSaNo,
                        const vector< short > & inAntVec,
                        const float             inTmo,
                        const bool              inCatalogTest,
                        const bool              inOpticalPointSim,
                        const bool              inpdbTest ) :
threadNo( inThreadNo ),
saNo( inSaNo ),
antVec( inAntVec ),
tmo( inTmo ),
catalogTest( inCatalogTest ),
opticalPointSim( inOpticalPointSim ),
pdbTest( inpdbTest)
{
}


const char * const kSourceNames[] =
    {
        "GDRA",
        "AGEM",
        "ATAU",
        "AUMA",
        "GUMA",
        "BTAU",
        "BUMA",
        "BAUR",
        "EUMA"
    };

const int kNumSourceNames =
    (sizeof( kSourceNames ) / sizeof( kSourceNames[ 0 ] ));
    

void
opticalPointSim( SubarrayControl_var & sac,
                 const carma::control::SeqShort &      antSeq,
                 const int             numCycles,
                 const float           tmo,
                 const int             sourceOffset )
{
    for ( int i = 0; i < numCycles; ++i ) {
        for ( int j = 0; j < 4; ++j ) {
            const int sourceNo = ((sourceOffset + i + j) % kNumSourceNames);
            
            sac->track( kSourceNames[ sourceNo ], antSeq, false, carma::control::ZERO, 0.0, false);
        }
        
        bool gotThemAll = false;
        
        for ( int attempt = 0; attempt < 3; ++attempt ) {
            try {
                AntennaReady_var antReady =
                    sac->wait( WAIT_ONSOURCE, antSeq, tmo, WAIT_SINGLE, 0 );
                    
                if ( antReady->notready.length() == 0 ) {
                    gotThemAll = true;

                    break;
                }
            } catch ( const InvalidMonitorDataException & ) {
                programLogInfoIfPossible( "InvalidMonitorDataException exception" );
                // That's fine we'll just keep trying
            } catch ( const TimeoutException & ) {
                programLogInfoIfPossible( "TimeoutException exception" );
            } catch ( ... ) {
                programLogInfoIfPossible( "< unknown > exception" );
            }
        }
    }
}


vector< string >
extractSourceNames( const string & whazUpResult )
{
    const vector< string > lines = StringUtils::tokenize( whazUpResult, "\n" );
    
    const size_t lineCount = lines.size();
    
    if ( lineCount < 2 )
        throw CARMA_ERROR( "Too few lines" );
        
    if ( lines[ 0 ] != " System Catalog" )
        throw CARMA_ERROR( "bad first line" );

    if ( lines[ 1 ] != "SOURCE     RA(2000)      DEC(2000)     UP?        ELEV      RISE LST     SET LST   DOPPLER     ELEVLIM = 10.00 degrees" )
        throw CARMA_ERROR( "bad column headers line" );

    vector< string > result;

    for ( size_t i = 2; i < lineCount; ++i ) {
        const vector< string > columns =
            StringUtils::tokenize( lines[ i ], " " );
            
        if ( columns.empty() == false )
            result.push_back( columns[ 0 ] );
    }
    
    return result;
}


void
stressCatalog( SubarrayControl_var & sac,
               const int             numCycles )
{
    const ScopedLogNdc ndc( "stressCatalog" );
    
    programLogInfoIfPossible( "Fetching whazUp list" );
    
    string whazUpResult;
    {
        String_var stringVar = sac->whazUp();
        
        const string whazUpResult = stringVar.in();
    }

    programLogInfoIfPossible( "Extracting source list" );
    
    const vector< string > sourceNames = extractSourceNames( whazUpResult );
    
    programLogInfoIfPossible( "Hammering isUp" );
    
    for ( int i = 0; i < numCycles; ++i ) {
        vector< string >::const_iterator j = sourceNames.begin();
        const vector< string >::const_iterator jEnd = sourceNames.end();
        
        for ( ; j != jEnd; ++j ) {
            if ( sac->isUp( j->c_str() ) != true )
                programLogError( "questionable isUp result" );
        }
    }

    programLogInfoIfPossible( "Done" );
}

const char * const kProjectCodes[] =
    {
        "c0001",
        "rpnt",
        "test",
        "fringe"
    };

const int kNumProjectCodes = 4;

void
stressPdb( SubarrayControl_var & sac,
           const int             numCycles )
{
    const ScopedLogNdc ndc( "stressPdb" );
    const char * p = "project";
    programLogInfoIfPossible( "Entering" );
    
    
    // try a bunch of overlapping project queries
    observertools::ItemValueSequence ivSeq;
    ivSeq.length(1);
    for ( int i = 0; i < numCycles; ++i ) {
	for ( int j = 0; j < kNumProjectCodes; j++ ) {
	    ivSeq[0].name  = p;
	    ivSeq[0].value = kProjectCodes[j];
            ProjectSequence * p = sac->queryProject( ivSeq );
	    if( p->length() != 0 ) {
		const string pid( (*p)[0].projectID );
		const string v(kProjectCodes[j]);
		if ( pid != v ) {
		    ostringstream os;
		    os << " Return project code ["
			<< pid 
			<< "] not same as query ["
			<< v
			<< "]"
			;
		    throw CARMA_ERROR( os.str() );
		}
	    } else {
		ostringstream os;
		os << " Project sequence for " << kProjectCodes[j]
		   << " has zero length ";
		programLogWarnIfPossible( os.str() );
	    }
        }
    }

    programLogInfoIfPossible( "Done" );
}


void
entryPoint( const ThreadArgs & args )
try {
    CARMA_CPTRACE( Trace::TRACE2, "Entered." );

    SubarrayControl_var sac = retrieveSac( args.saNo );
    
    const carma::control::SeqShort antSeq = convertVectorToSequence< 
        carma::control::SeqShort >( args.antVec );
    
    if ( args.catalogTest )
        stressCatalog( sac, 10 );
    
    if ( args.opticalPointSim )
        opticalPointSim( sac, antSeq, 100, args.tmo, args.threadNo );

    if ( args.pdbTest )
        stressPdb(sac, 10 );

} catch ( ... ) {
    logCaughtAsError( );
    
    throw;
}


void
testIt( const int   numThreads,
        const float tmo,
        const bool  catalogTest,
        const bool  opticalPointSim,
        const bool  pdbTest
	)
{
    CARMA_CPTRACE( Trace::TRACE2,
                   "Creating " << numThreads << " threads." );

    vector< ::pthread_t > threads;

    {
        const int antsPerThread = max( 1, (15 / numThreads) );
        
        for ( int i = 0; i < numThreads; ++i ) {
            string threadNdc;

            {
                ostringstream oss;

                oss << "thread #" << (i + 1);

                threadNdc = oss.str();
            }

            vector< short > antVec;
            
            for ( int j = 0; j < antsPerThread; ++j ) {
                const int ant = (i * antsPerThread) + j + 1;
                
                if ( ant <= 15 )
                    antVec.push_back( ant );
            }

            const ThreadArgs args( i,
                                   1,
                                   antVec,
                                   tmo,
                                   catalogTest,
                                   opticalPointSim,
		                   pdbTest );

            threads.push_back( StartPthreadWithCopy( entryPoint,
                                                     args,
                                                     threadNdc ) );
            
            // sleep( 1 );
        }
    }

    CARMA_CPTRACE( Trace::TRACE2,
                   "Joining to " << threads.size() << " threads." );

    {
        for ( ::size_t i = 0; i < threads.size(); ++i ) {
            void * threadResult = 0;

            ::pthread_join( threads[ i ], &threadResult );
        }
    }

    CARMA_CPTRACE( Trace::TRACE2, "All threads completed." );
}


}  // namespace < anonymous >


//
// @version $Revision: 1.20 $
//
// @usage use it
//
// @description
//   A test binary that stress tests the SubarrayControl DO.
//
// @key threads 5 int
//      Number of threads to use in the multithreaded testing. 0 or less means
//      no multithreaded testing.
//
// @key tmo 0.0 double
//      timeout value for sac waits, in seconds
//
// @key catalogTest true bool
//      Whether or not to test the catalog routines
//
// @key opticalPointSim false bool
//      Whether or not to do an optical pointing simulation
//
// @key pdbTest false bool
//      Whether or not to do a project database stress test
//
// @logger TEST_FACILITY carma.test.control.stressSac
//


int
Program::main( )
try {
    const int threadsParam = getIntParameter( "threads" );
    const double tmoParam = getDoubleParameter( "tmo" );
    const bool catalogTest = getBoolParameter( "catalogTest" );
    const bool opticalPointSim = getBoolParameter( "opticalPointSim" );
    const bool pdbTest = getBoolParameter( "pdbTest" );

    testIt( threadsParam, tmoParam, catalogTest, opticalPointSim, pdbTest );
    
    return EXIT_SUCCESS;
} catch ( BaseException & ex ) {
    cerr << " Caught Exception " << ex.getMessage() << endl;
    return EXIT_FAILURE;
}
