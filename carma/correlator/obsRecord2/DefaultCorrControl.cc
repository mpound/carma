// $Id: DefaultCorrControl.cc,v 1.191 2014/02/28 18:29:50 krauch Exp $

#include "carma/correlator/obsRecord2/DefaultCorrControl.h"

#include "carma/corba/Server.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/obsRecord2/BandStatus.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/correlator/obsRecord2/CorDataBase_skel.h"
#include "carma/correlator/obsRecord2/CorDataBase_skel_tie.h"
#include "carma/correlator/obsRecord2/obsRecordUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Orb.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLockManager.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/WorkResult.h"
#include "carma/util/WorkResultSetPostError.h"
#include "carma/util/WorkResultSetWaitError.h"
#include "carma/util/corbaSequenceUtils.h"

// COBRA Correlator Interface
#include <cobra/CorrelatorDelayCorrection.h>
#include <cobra/CorrelatorBandControlClient.h>
#include <cobra/CobraVersion.h>

#include <deque>
#include <iomanip>

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::corba;
using namespace carma::util;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;


namespace { // < anonymous >

    const string kNoiseSourceName = "noise";
    const string kRfSourceName = "rf";

    typedef ScopedLock< PthreadMutex > GuardLock;


    string
    getLatencyText( const struct ::timeval & tvBefore,
                    const struct ::timeval & tvAfter )
    {
        const long long micros =
            static_cast< long long >( tvAfter.tv_sec ) * 1000 * 1000 +
            static_cast< long long >( tvAfter.tv_usec ) -
            static_cast< long long >( tvBefore.tv_sec ) * 1000 * 1000 -
            static_cast< long long >( tvBefore.tv_usec );

        ostringstream oss;

        oss << (micros / 1.0e+6) << " seconds";

        return oss.str();
    }


    typedef CORBA::Double MjdDouble;
    typedef set< MjdDouble > SetMjdDouble;
    const int kMjdPrecision = 1;

    string
    formatMjd( const MjdDouble mjd )
    {
        if ( mjd == 0 )
            return "0";
        else
            return Time::getTimeString( mjd, kMjdPrecision );
    }


    string
    formatSetMjd( const SetMjdDouble & setMjd )
    {
        ostringstream oss;

        oss << setprecision( 15 );

        if ( setMjd.size() == 1 )
            oss << formatMjd( *(setMjd.begin()) );
        else {
            oss << '{';

            bool firstOne = true;
            SetMjdDouble::const_iterator i = setMjd.begin();
            const SetMjdDouble::const_iterator iEnd = setMjd.end();

            for ( ; i != iEnd; ++i ) {
                if ( firstOne )
                    firstOne = false;
                else
                    oss << ", ";

                oss << formatMjd( *i );
            }

            oss << '}';
        }

        return oss.str();
    }

    struct ProducerThreadArgs {
        Server &             server;
        void                 (*shutdownCallback)( void * );
        void *               shutdownCallbackArg;

        ProducerThreadArgs( Server & srvr,
                            void (*callback)( void * ),
                            void * callbackArgs ) :
            server( srvr ),
            shutdownCallback( callback ),
            shutdownCallbackArg( callbackArgs ) { };

    };


    void
    producerThreadEntrypoint( const ProducerThreadArgs & args )
    try {
        //programLogInfoIfPossible( "Thread running" );

        {
            // run the server which should block
            args.server.run( false );

            if ( args.shutdownCallback != 0 ) {
                //programLogInfoIfPossible( "Calling shutdown callback..." );

                (*(args.shutdownCallback))( args.shutdownCallbackArg );

                //programLogInfoIfPossible( "Shutdown callback returned" );
            }
        }

        //programLogInfoIfPossible( "Thread completing" );
    } catch ( ... ) {
        programLogErrorIfPossible(
            "Coming out of producerThreadEntrypoint on an exception - " +
            getStringForCaught() );

        throw;
    }

    string
    stringForModeType( const obsRecord2::ModeType mode )
    {
        switch ( mode ) {
            case obsRecord2::MODE_IDLE:    return "MODE_IDLE";
            case obsRecord2::MODE_NORMAL:  return "MODE_NORMAL";
        }

        ostringstream oss;

        oss << "<Unknown mode " << static_cast< int >( mode ) << ">";

        return oss.str();
    }



}  // namespace < anonymous >


class DefaultCorrControl::CobraClient {
    public:
        explicit CobraClient(int controlPort);

        ~CobraClient( );

        // for COBRA and CARMA boards
        void setBw( const string & bwText,
                    int            seqNo,
                    int            astroBandNo
                    );

        // for C3G boards
        void setBw( const vector<string> & bwText,
                    int            seqNo,
                    const vector<unsigned> & astroBandNo
                    );

        void setSourceName( const string & sourceName );

        void setDelaySamps(
            const cobra::CorrelatorInterpolatorSamples & samps,
            const string &                               sourceName );

        // for COBRA and CARMA boards
        void setDcSettings( float freqInHz,
                            bool  sbIsUpper,
                            bool  bdcEnabled );

        // for C3G boards
        void setDcSettings( const vector<float> & freqInHz );

        void optimizeThresholds( int seqNo );

        void flattenPhases( int seqNo );

        void calibrateSpectra( bool enable,
                               bool cache,
                               int count,
                               int seqNo  );

        void enableCorrelation( bool enable );

        void setWalshColumns(
                const vector<int> & cols90,
                const vector<int> & cols180,
                const int nStates90,
                const int nStates180,
                const bool noiseEnabled );

    private:
        class OpReq;
        class SetBwOpReq;
        class SetSourceNameOpReq;
        class SetDelaySampsOpReq;
        class SetDcSettingsOpReq;
        class OptimizeThresholdsOpReq;
        class FlattenPhasesOpReq;
        class CalibrateSpectraOpReq;
        class SetWalshColumnsOpReq;
        class EnableCorrelationOpReq;

        struct SharedQueue {
            PthreadMutex     requestGuard;
            bool             quitRequested;
            deque< OpReq * > opReqDeque;

            PthreadCond      requestCond;
        };

        struct CobraOpThreadArgs;

        static void CobraOpThreadEntrypoint( const CobraOpThreadArgs & args );

        void queueOpReq( auto_ptr< OpReq > opReq );

        static void waitForAllNormal( const WorkResultSet & wrs,
                                      const unsigned long   milliseconds );

        SharedQueue sharedQueue_;
        ::pthread_t processThread_;
};


struct DefaultCorrControl::CobraClient::CobraOpThreadArgs {
    int             controlPort;
    SharedQueue *   sharedQueue;
};

//--------------------------------------------------
// Parent OpReq class for all operation requests.
//--------------------------------------------------
class DefaultCorrControl::CobraClient::OpReq {
    public:
        explicit OpReq( const WorkResult & wr );

        virtual ~OpReq( );

        virtual void couldNotConnect( );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient) = 0;

    protected:
        void postAbnormalResult( const string & errorText,
                                 const char *   context );

        void postNormalResult(const char* context, bool ignoreNoWaiters=false);

    private:
        WorkResult wr_;
};


//--------------------------------------------------
// Set bandwidth operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::SetBwOpReq : public OpReq {
    public:
        // for COBRA and CARMA boards
        SetBwOpReq( const string &     bwText,
                    int                seqNo,
                    const int   astroBandNo,
                    const WorkResult & wr );

        // for C3G boards
        SetBwOpReq( const vector<string> &   bwText,
                    int                      seqNo,
                    const vector<unsigned> & astroBandNo,
                    const WorkResult & wr );

        virtual void couldNotConnect( );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient);

    private:
        const string           bwText_;
        const vector<string>   bwTextVec_;
        const int              seqNo_;
        const int              astroBandNo_;
        const vector<unsigned> astroBandNoVec_;
};

//--------------------------------------------------
// Set source name operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::SetSourceNameOpReq : public OpReq {
    public:
        explicit SetSourceNameOpReq( const string &     sourceName,
                                     const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );

    private:
        const string sourceName_;
};


//--------------------------------------------------
// Set delay samples operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::SetDelaySampsOpReq :
public OpReq {
    public:
        SetDelaySampsOpReq(
            const cobra::CorrelatorInterpolatorSamples & samps,
            const string &                               sourceName,
            const WorkResult &                           wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );

    private:
        const cobra::CorrelatorInterpolatorSamples samps_;
        const string                               sourceName_;
};


//--------------------------------------------------
// Set downconverter settings operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::SetDcSettingsOpReq : public OpReq {
    public:
        // COBRA, CARMA
        SetDcSettingsOpReq( const float  freqInHz,
                            const bool   sbIsUpper,
                            const bool   bdcEnabled,
                            const WorkResult & wr );
        //C3G
        SetDcSettingsOpReq( const vector<float> & freqInHz,
                            const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );

    private:
        const float freqInHz_;
        //@todo make this const but requires change in cobra library
        //as well.
        vector<float> freqInHzVec_;
        const bool  sbIsUpper_;
        const bool  bdcEnabled_;
};

//--------------------------------------------------
// Optimize digitizer thresholds operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::OptimizeThresholdsOpReq: public OpReq {
    public:
        OptimizeThresholdsOpReq( const int seqNo,
                                 const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );

    private:
        const int                       seqNo_;
};

//--------------------------------------------------
// Flatten phases operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::FlattenPhasesOpReq: public OpReq {
    public:
        FlattenPhasesOpReq( const int seqNo, const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );


    private:
        const int                       seqNo_;
};

//--------------------------------------------------
// Calibrate spectra (flatten amplitudes) operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::CalibrateSpectraOpReq: public OpReq {
    public:
        CalibrateSpectraOpReq( const bool enable, const bool cache,
                               const int count, const int seqNo,
                               const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );


    private:
        const bool enable_;
        const bool cache_;
        const int  count_;
        const int  seqNo_;
};

//--------------------------------------------------
// Select Walsh table columns operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::SetWalshColumnsOpReq: public OpReq {
    public:
        SetWalshColumnsOpReq( const vector<int> & cols90,
                              const vector<int> & cols180,
                              const int nStates90, const int nStates180,
                              const bool noiseEnabled,
                              const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient );

    private:
        std::vector<unsigned int> cols90_;
        std::vector<unsigned int> cols180_;
        unsigned int nStates90_;
        unsigned int nStates180_;
        bool noiseEnabled_;
};

//--------------------------------------------------
// Enable correlation operation request
//--------------------------------------------------
class DefaultCorrControl::CobraClient::EnableCorrelationOpReq: public OpReq {
    public:
        EnableCorrelationOpReq( const bool enable , const WorkResult & wr );

        virtual void
        doIt( cobra::CorrelatorBandControlClient & rawClient);

    private:
        bool enable_;
};

//--------------------------------------------------
// Dispatch class for cobra calls.
//--------------------------------------------------
DefaultCorrControl::CobraClient::CobraClient( const int controlPort ) :
sharedQueue_(),
processThread_()
{
    sharedQueue_.quitRequested = false;

    const string initialNdc = "Cobra op thread";
    CobraOpThreadArgs args;

    args.controlPort = controlPort;
    args.sharedQueue = &sharedQueue_;

    //programLogInfoIfPossible( "Starting cobra op thread..." );

    processThread_ =
        StartPthreadWithCopy( CobraOpThreadEntrypoint,
                              args,
                              initialNdc );

    //programLogInfoIfPossible( "Cobra op thread started" );
}


DefaultCorrControl::CobraClient::~CobraClient( )
try {
    deque< OpReq * > oldOpReqDeque;

    //programLogInfoIfPossible( "Quitting cobra op thread" );
    {
        const ScopedLock< PthreadMutex > lock( sharedQueue_.requestGuard );

        sharedQueue_.quitRequested = true;
        sharedQueue_.opReqDeque.swap( oldOpReqDeque );
    }

    sharedQueue_.requestCond.Signal();

    if ( oldOpReqDeque.empty() == false ) {
        //programLogInfoIfPossible( "Deleting unprocessed op requests" );

        deque< OpReq * >::const_iterator i = oldOpReqDeque.begin();
        const deque< OpReq * >::const_iterator iEnd = oldOpReqDeque.end();

        for ( ; i != iEnd; ++i ) {
            try {
                delete (*i);
            } catch ( ... ) {
                // Just stifle any exception
                programLogErrorIfPossible(
                    "Stifling exception deleting unprocessed op request" );
            }
        }

        oldOpReqDeque.clear();
    }

    void * result = 0;

    //programLogInfoIfPossible( "Joining to cobra op thread..." );

    ::pthread_join( processThread_, &result );

    //programLogInfoIfPossible( "Joined to cobra op thread" );
} catch ( ... ) {
    // Just stifle any exceptions

    programLogErrorIfPossible(
        "Stifling exception"
        " in DefaultCorrControl::CobraClient::~CobraClient - " +
        getStringForCaught() );

    return;
}


// Queue the operation for dispatch
void
DefaultCorrControl::CobraClient::queueOpReq( auto_ptr< OpReq > opReq )
{
    {
        ScopedLockManager< PthreadMutex >
            lockManager( sharedQueue_.requestGuard );

        lockManager.lock();

        if ( sharedQueue_.quitRequested ) {
            lockManager.unlock();

            const string msg = "Op queue already has a quit request posted";

            programLogErrorIfPossible( msg );

            throw CARMA_ERROR( msg );
        }

        {
            const size_t queueItems = sharedQueue_.opReqDeque.size();
            if ( queueItems >= 64 ) {
                lockManager.unlock();

                ostringstream oss;

                oss << "Op queue already has " << queueItems << " items";

                programLogErrorIfPossible( oss.str() );

                throw CARMA_ERROR( oss.str() );
            }
        }

        sharedQueue_.opReqDeque.push_back( opReq.get() );
        opReq.release();

        lockManager.unlock();
    }

    sharedQueue_.requestCond.Signal();
}


// wait for operation completion
void
DefaultCorrControl::CobraClient::waitForAllNormal(
    const WorkResultSet & wrs,
    const unsigned long   milliseconds )
{
    try {
        wrs.waitForAll( milliseconds,
                        true,
                        WorkResultSet::GOOD_POST_STATE );
    } catch ( const WorkResultSet::WaitError & waitErr ) {
        if ( (waitErr.hadAbnormals() == false) && waitErr.hadUnfinishedKeys() )
            programLogWarnIfPossible( getStringForCaught() );
        else
            programLogErrorIfPossible( getStringForCaught() );
    }
}


void
DefaultCorrControl::CobraClient::CobraOpThreadEntrypoint(
    const CobraOpThreadArgs & args )
try {

    if ( args.controlPort == 0 ) {
        const string msg = "Correlator control port has not been set";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( args.sharedQueue == 0 ) {
        const string msg = "Shared queue pointer is NULL";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    const int controlPort = args.controlPort;
    SharedQueue & sharedQueue = *(args.sharedQueue);

    ACE_Time_Value timeoutValue( 45, 0 );  // 45 seconds
    auto_ptr< cobra::CorrelatorBandControlClient > rawClient;

    while ( true ) {
        ScopedLockManager< PthreadMutex >
            lockManager( sharedQueue.requestGuard );

        lockManager.lock();

        while ( (sharedQueue.quitRequested == false) &&
                sharedQueue.opReqDeque.empty() )
            sharedQueue.requestCond.Wait( sharedQueue.requestGuard );

        if ( sharedQueue.quitRequested )
            break;

        auto_ptr< OpReq > opReq;
        {
            OpReq * const temp = sharedQueue.opReqDeque.front();
            sharedQueue.opReqDeque.pop_front();
            opReq.reset( temp );
        }

        lockManager.unlock();
        if ( rawClient.get() == 0 ) {
            rawClient =
                auto_ptr< cobra::CorrelatorBandControlClient >(
                    new cobra::CorrelatorBandControlClient );

            rawClient->setTimeout( timeoutValue );
        }

        if ( !rawClient->connected() ) {
            const int status = rawClient->connect( controlPort, 0 );

            if ( status < 0 ) {
                ostringstream oss;

                oss << "cobra::CorrelatorBandControlClient::connect("
                    << controlPort << ", 0) failed with status="
                    << status;

                programLogErrorIfPossible( oss.str() );

                try {
                    opReq->couldNotConnect( );
                } catch ( ... ) {
                    // Just stifle any exception

                    programLogErrorIfPossible(
                        "opReq->couldNotConnect()"
                        " resulted in exception - " +
                        getStringForCaught() );
                }

                continue;
            }
        }

        try {
            opReq->doIt( *rawClient );
        } catch ( ... ) {
            // Just stifle any exception

            programLogErrorIfPossible(
                "opReq->doIt() resulted in exception - " +
                getStringForCaught() );
        }
    }

} catch ( ... ) {
    programLogErrorIfPossible(
        "Coming out of DefaultCorrControl::CobraClient::processEntrypoint"
        " on an exception - " + getStringForCaught() );

    throw;
}


//--------------------------------------
// CobraClient::OpReq parent class implementation
//--------------------------------------
DefaultCorrControl::CobraClient::OpReq::OpReq( const WorkResult & wr ) :
wr_( wr )
{
}


DefaultCorrControl::CobraClient::OpReq::~OpReq( )
try {
} catch ( ... ) {
    // Just stifle any exceptions

    programLogErrorIfPossible(
        "Stifling exception"
        " in DefaultCorrControl::CobraClient::OpReq::~OpReq - " +
        getStringForCaught() );

    return;
}


void
DefaultCorrControl::CobraClient::OpReq::couldNotConnect()
{
    postAbnormalResult( "Could not connect to cobra server",
                        "OpReq::couldNotConnect" );
}


void
DefaultCorrControl::CobraClient::OpReq::postAbnormalResult(
    const string &     errorText,
    const char * const context )
{
    try {
        wr_.postAbnormal( errorText );
    } catch ( const WorkResultSet::PostError & postErr ) {
        const ScopedLogNdc ndc( context );

        if ( postErr.getPostState() ==
             WorkResultSet::NO_WAITERS_DROPPED_POST_STATE )
            programLogWarnIfPossible( getStringForCaught() );
        else
            programLogErrorIfPossible( getStringForCaught() );
    }
}


void
DefaultCorrControl::CobraClient::OpReq::postNormalResult(
    const char* const context, const bool ignoreNoWaiters )
{
    try {
        wr_.postNormal();
    } catch ( const WorkResultSet::PostError & postErr ) {
        const ScopedLogNdc ndc( context );

        if (postErr.getPostState() ==
               WorkResultSet::NO_WAITERS_DROPPED_POST_STATE) {
            if (ignoreNoWaiters == false)
                programLogWarnIfPossible( getStringForCaught() );
        }
        else
            programLogErrorIfPossible( getStringForCaught() );
    }
}
//--------------------------------------
// END CobraClient::OpReq parent class implementation
//--------------------------------------


// ==== Set bandwidth implementation =================================
// COBRA and CARMA
DefaultCorrControl::CobraClient::SetBwOpReq::SetBwOpReq(
    const string &     bwText,
    const int          seqNo,
    const int          astroBandNo,
    const WorkResult & wr ) :
OpReq( wr ),
bwText_( bwText ),
seqNo_( seqNo ),
astroBandNo_( astroBandNo )
{
}

// C3G
DefaultCorrControl::CobraClient::SetBwOpReq::SetBwOpReq(
    const vector<string> &  bwText,
    const int          seqNo,
    const vector<unsigned> & astroBandNo,
    const WorkResult & wr ) :
OpReq( wr ),
bwTextVec_( bwText ),
seqNo_( seqNo ),
astroBandNo_(0),
astroBandNoVec_( astroBandNo )
{
}


void
DefaultCorrControl::CobraClient::SetBwOpReq::couldNotConnect()
{
    OpReq::couldNotConnect( );
}


void
DefaultCorrControl::CobraClient::SetBwOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    ScopedLogNdc ndc("DefaultCorrControl::CobraClient::SetBwOpReq::doIt");
    const bool isVectorized = (bwTextVec_.size() > 0 );
    struct ::timeval tvBefore;
    gettimeofday( &tvBefore, 0 );

    {
        ostringstream oss;
        oss << "Dispatching setBandwidth to "
            << (isVectorized ? "c3g" : "cobra/carma")
            << "raw client (seqNo =" << seqNo_ << ")";
        programLogInfoIfPossible( oss.str() );
    }

    int status;
    const size_t vecSize = bwTextVec_.size();
    if ( vecSize == 0 ) {
        status = rawClient.setBandwidth( bwText_, seqNo_, astroBandNo_ );
    } else {
        status = rawClient.setBandwidth( bwTextVec_, astroBandNoVec_, seqNo_ );
    }

    struct ::timeval tvAfter;
    gettimeofday( &tvAfter, 0 );

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();

        // Log a message
        ostringstream oss;

        if ( isVectorized ) {
            oss << "cobra::CorrelatorBandControlClient::setBandwidth("
                << "vector size = " << vecSize
                << ", seqNo = seqNo_) failed after "
                << getLatencyText( tvBefore, tvAfter )
                << " with status=" << status;
        } else {
            oss << "cobra::CorrelatorBandControlClient::setBandwidth(\""
                << bwText_ << "\", " << seqNo_
                << ", " << astroBandNo_
                << ") failed after " << getLatencyText( tvBefore, tvAfter )
                << " with status=" << status;
        }

        postAbnormalResult( oss.str(), "SetBwOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else {

        postNormalResult("SetBwOpReq::doIt", true);

        // Debug message to figure out why configband is taking
        // too long.

        if ( !isVectorized ) {
            ostringstream oss;
            oss << "cobra::CorrelatorBandControlClient::setBandwidth(\""
                << bwText_ << "\", " << seqNo_
                << ", " << astroBandNo_
                << ") took " << getLatencyText( tvBefore, tvAfter )
                << ", returning with status=" << status;
            programLogNoticeIfPossible( oss.str() );
        }

    }
}


void
DefaultCorrControl::CobraClient::setBw(
    const string & bwText,
    const int      seqNo ,
    const int      astroBandNo )
{
    string wrsId;
    {
        ostringstream oss;

        oss << "setBw(" << bwText << ", " << seqNo << "," << astroBandNo << ")";

        wrsId = oss.str();
    }

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new SetBwOpReq( bwText, seqNo, astroBandNo, wr ) ) );
    }
}

void
DefaultCorrControl::CobraClient::setBw(
    const vector<string> & bwText,
    const int      seqNo ,
    const vector<unsigned> & astroBandNo )
{
    // precondition astroBandNo.size() = bwText.size().
    // CorrelatorHandle must ensure this.
    string wrsId;
    {
        ostringstream oss;

        oss << "vectorized setBw( seqNo = "<<seqNo<<" size = "<<bwText.size();

        wrsId = oss.str();
    }

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "c3g op request" );

        queueOpReq(
            auto_ptr< OpReq >( new SetBwOpReq( bwText, seqNo, astroBandNo, wr ) ) );
    }

}



// ==== Sourcename implementation =================================
DefaultCorrControl::CobraClient::SetSourceNameOpReq::SetSourceNameOpReq(
    const string &     sourceName,
    const WorkResult & wr ) :
OpReq( wr ),
sourceName_( sourceName )
{
}


void
DefaultCorrControl::CobraClient::SetSourceNameOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    const int status = rawClient.sourceName( sourceName_ );
    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();

        // Log a message
        ostringstream oss;

        oss << "cobra::CorrelatorBandControlClient::sourceName(\""
            << sourceName_ << ") failed with status=" << status;

        postAbnormalResult( oss.str(), "SetSourceNameOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "SetSourceNameOpReq::doIt" );
}


void
DefaultCorrControl::CobraClient::setSourceName( const string & sourceName )
{
    WorkResultSet wrs( "setSourceName(" + sourceName + ")" );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new SetSourceNameOpReq( sourceName, wr ) ) );
    }

    waitForAllNormal( wrs, 2000 );
}

// ==== Delay samples implementation =================================

DefaultCorrControl::CobraClient::SetDelaySampsOpReq::SetDelaySampsOpReq(
    const cobra::CorrelatorInterpolatorSamples & samps,
    const string &                               sourceName,
    const WorkResult &                           wr ) :
OpReq( wr ),
samps_( samps ),
sourceName_( sourceName )
{
}


void
DefaultCorrControl::CobraClient::SetDelaySampsOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    const int status = rawClient.delaySamples( samps_, sourceName_ );
    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();

        // Log a message
        ostringstream oss;

        oss << "cobra::CorrelatorBandControlClient::delaySamples("
            << "samps_, \"" << sourceName_
            << "\") failed with status=" << status;

        postAbnormalResult( oss.str(), "SetDelaySampsOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "SetDelaySampsOpReq::doIt" );
}


// NOTE:  Why is sourceName needed here?  It is already set
// via setNoiseSourceState.  Potential for synchronization errors
// for 32-state Walsh tables.  This argument may be removed in
// future cobra release.
void
DefaultCorrControl::CobraClient::setDelaySamps(
    const cobra::CorrelatorInterpolatorSamples & samps,
    const string &                               sourceName )
{
    string wrsId;
    {
        ostringstream oss;

        oss << "setDelaySamps(samps, " << sourceName << ")";

        wrsId = oss.str();
    }

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >(
                new SetDelaySampsOpReq( samps, sourceName, wr ) ) );
    }

    waitForAllNormal( wrs, 2000 );
}


// ==== downconverterSettings implementation =================================
DefaultCorrControl::CobraClient::SetDcSettingsOpReq::SetDcSettingsOpReq(
    const float        freqInHz,
    const bool         sbIsUpper,
    const bool         bdcEnabled,
    const WorkResult & wr ) :
OpReq( wr ),
freqInHz_( freqInHz ),
sbIsUpper_( sbIsUpper ),
bdcEnabled_( bdcEnabled )
{

}

DefaultCorrControl::CobraClient::SetDcSettingsOpReq::SetDcSettingsOpReq(
    const vector<float> & freqInHz,
    const WorkResult & wr ) :
OpReq( wr ),
freqInHz_( 0.0 ),
freqInHzVec_( freqInHz ),
sbIsUpper_( false ),
bdcEnabled_( false )
{
}


void
DefaultCorrControl::CobraClient::SetDcSettingsOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient )
{
    const size_t vecSize = freqInHzVec_.size();
    bool isVectorized = ( vecSize > 0 );
    int status;
    if ( isVectorized ) {
      // !!!! These must be populated for full 2G functionality (only).
      vector<int> sbIsUpperVec(vecSize, false),
                 bdcEnabledVec(vecSize, false);
      status = rawClient
                .downconverterSettings( freqInHzVec_, sbIsUpperVec, bdcEnabledVec );
    } else {
      status = rawClient
                .downconverterSettings( freqInHz_, sbIsUpper_, bdcEnabled_ );
    }

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();

        // Log a message
        ostringstream oss;

        if ( isVectorized ) {
            oss << "cobra::CorrelatorBandControlClient::downconverterSettings("
                << "vector size = " << vecSize
                << ") failed with status=" << status;
        } else {
            oss << "cobra::CorrelatorBandControlClient::downconverterSettings("
                << freqInHz_ << ", "
                << boolalpha << sbIsUpper_
                << ", "
                << boolalpha << bdcEnabled_
                << ") failed with status=" << status;
        }

        postAbnormalResult( oss.str(), "SetDcSettingsOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "SetDcSettingsOpReq::doIt" );
}


void
DefaultCorrControl::CobraClient::setDcSettings(
        const vector<float> & freqInHz )
{
    ScopedLogNdc ndc("DefaultCorrControl::CobraClient::setDcSettings");
    string wrsId;
    {
        ostringstream oss;
        oss << "setDcSettings( vecsize = " <<freqInHz.size() ;
        wrsId = oss.str();
    }

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "c3g op request" );

        queueOpReq(
            auto_ptr< OpReq >(
                new SetDcSettingsOpReq( freqInHz, wr )) );
    }

    waitForAllNormal( wrs, 2000 );
}

void
DefaultCorrControl::CobraClient::setDcSettings(
    const float freqInHz,
    const bool  sbIsUpper,
    const bool  bdcEnabled)
{
    string wrsId;
    {
        ostringstream oss;

        oss << "setDcSettings(" << freqInHz << ", "
            << (sbIsUpper ? "true" : "false") << ","
            << (bdcEnabled? "true" : "false") << ")";

        wrsId = oss.str();
    }

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >(
                new SetDcSettingsOpReq( freqInHz, sbIsUpper, bdcEnabled, wr ) ) );
    }

    waitForAllNormal( wrs, 2000 );
}

// ==== optimizeThresholds implementation ====================================
DefaultCorrControl::CobraClient::
OptimizeThresholdsOpReq::OptimizeThresholdsOpReq ( const int seqNo,
        const WorkResult & wr ) :
OpReq( wr ),
seqNo_(seqNo)
{
}


void
DefaultCorrControl::CobraClient::OptimizeThresholdsOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient )
{
    const ScopedLogNdc ndc( "DefaultCorrControl::CobraClient::OptimizeThresholdsOpReq");
    {
        ostringstream oss;
        oss << "Dispatching optimizeThresholds to cobra raw client (seqNo =" << seqNo_ << ")";
        programLogInfoIfPossible( oss.str() );
    }
    const int status = rawClient.optimizeThresholds( true, seqNo_ );
    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();
        ostringstream oss;
        oss << "call to cobra::CorrelatorBandControlClient::"
            << "optimizeThresholds( force=true, seqNo=" << seqNo_ << ") failed with status="
            << status;
        postAbnormalResult( oss.str(), "OptimizeThresholdsOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "OptimizeThresholdsOpReq::doIt" );
}

void
DefaultCorrControl::CobraClient::optimizeThresholds( int seqNo )
{
    ostringstream os;
    os << "optimizeThresholds(seqNo="<<seqNo<<")";
    const string wrsId = os.str();

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new OptimizeThresholdsOpReq(seqNo, wr) ) );
    }

    waitForAllNormal( wrs, 20000 );
}

// ==== flattenPhases implementation =======================================
DefaultCorrControl::CobraClient::
FlattenPhasesOpReq::FlattenPhasesOpReq( const int seqNo,
                                        const WorkResult & wr ) :
OpReq( wr ),
seqNo_(seqNo)
{
}

void
DefaultCorrControl::CobraClient::flattenPhases( int seqNo )
{
    ostringstream os;
    os << "flattenPhases(seqNo="<<seqNo<<")";
    const string wrsId = os.str();

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new FlattenPhasesOpReq(seqNo, wr) ) );
    }

    waitForAllNormal( wrs, 20000 );
}

void
DefaultCorrControl::CobraClient::FlattenPhasesOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    const ScopedLogNdc ndc( "DefaultCorrControl::CobraClient::FlattenPhasesOpReq");
    {
        ostringstream oss;
        oss << "Dispatching flattenPhases command to cobra raw client (seqNo =" << seqNo_ << ")";
        programLogInfoIfPossible( oss.str() );
    }
    const int refAntNo = -1;
    const int status = rawClient.flattenPhases( refAntNo, seqNo_ );

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();
        ostringstream oss;
        oss << "call to cobra::CorrelatorBandControlClient::"
            << "flattenPhases(" << seqNo_ << ") failed with status="
            << status;
        postAbnormalResult( oss.str(), "FlattenPhasesOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "FlattenPhasesOpReq::doIt" );
}

// ==== calibrateSpectra implementation =======================================
//
DefaultCorrControl::CobraClient::
CalibrateSpectraOpReq::CalibrateSpectraOpReq(
                       const bool enable, const bool cache,
                       const int count, const int seqNo,
                       const WorkResult & wr ) :
OpReq( wr ),
enable_(enable),
cache_(cache),
count_(count),
seqNo_(seqNo)
{
}

void
DefaultCorrControl::CobraClient::calibrateSpectra( bool enable, bool cache,
                                                   int count, int seqNo )
{
    ostringstream os;
    os << "calibrateSpectra("
       << boolalpha
       << enable
       << ", cache = "
       << cache
       << ", count = "
       << count
       << ", seqNo ="
       << seqNo
       << ")"
       ;
    const string wrsId = os.str();
 // DEBUG
    //const ScopedLogNdc ndc( "DefaultCorrControl::CobraClient::calibrateSpectra");
    //programLogInfoIfPossible( wrsId );

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new CalibrateSpectraOpReq(enable, cache,
                                                         count, seqNo, wr) )
                             );
    }

    // wait five seconds plus the integration time
    // (count = number of half-second frames)
    unsigned long waitTime = 5000 + 500*count;
    waitForAllNormal( wrs, waitTime );
}

void
DefaultCorrControl::CobraClient::CalibrateSpectraOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    const ScopedLogNdc ndc( "DefaultCorrControl::CobraClient::CalibrateSpectraOpReq");
    {
        ostringstream oss;
        oss << "::doIt( enable = "
            << boolalpha
            << enable_
            << ", cache = "
            << cache_
            << ", count = "
            << count_
            << ", seqNo ="
            << seqNo_
            << ")"
            ;
        programLogInfoIfPossible( oss.str() );
    }
    const int status = rawClient.calibrate( enable_, cache_, count_, seqNo_ );

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();
        ostringstream oss;
        oss << "call to cobra::CorrelatorBandControlClient::"
            << "calibrate( enable = "
            << boolalpha
            << enable_
            << ", cache = "
            << cache_
            << ", count = "
            << count_
            << ", seqNo ="
            << seqNo_
            << ") failed with status="
            << status;
        postAbnormalResult( oss.str(), "CalibrateSpectraOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "CalibrateSpectraOpReq::doIt" );
}

// === Set Walsh Columns implemetnation ===========================
DefaultCorrControl::CobraClient::
SetWalshColumnsOpReq::SetWalshColumnsOpReq(
                       const vector<int> & cols90,
                       const vector<int> & cols180,
                       const int nStates90, const int nStates180,
                       const bool noiseEnabled,
                       const WorkResult & wr ) :
OpReq( wr ),
cols90_(cols90.begin(),cols90.end()),
cols180_(cols180.begin(),cols180.end()),
nStates90_(static_cast<unsigned int>(nStates90)),
nStates180_(static_cast<unsigned int>(nStates180)),
noiseEnabled_(noiseEnabled)
{
}

void
DefaultCorrControl::CobraClient::
setWalshColumns( const vector<int> & cols90,
                 const vector<int> & cols180,
                 const int nStates90,
                 const int nStates180,
                 const bool noiseEnabled )
{
    ostringstream os;
    os << "setWalshColumns("
       << " cols90.size = "
       << cols90.size()
       << " cols180.size = "
       << cols180.size()
       << " nStates90 = "
       << nStates90
       << " nStates180 = "
       << nStates180
       << boolalpha
       << " noiseEnabled = "
       << noiseEnabled
       << ")"
       ;
    const string wrsId = os.str();
 // DEBUG
    const ScopedLogNdc ndc( "DefaultCorrControl::CobraClient::setWalshColumns");
    programLogInfoIfPossible( wrsId );

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new SetWalshColumnsOpReq(
                    cols90, cols180, nStates90, nStates180, noiseEnabled, wr)
                             )
                  );
    }

    // wait three seconds (?)
    const unsigned long waitTime = 3000 ;
    waitForAllNormal( wrs, waitTime );
}

void
DefaultCorrControl::CobraClient::SetWalshColumnsOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{
    const string methodName( "DefaultCorrControl::CobraClient::SetWalshColumnsOpReq");
    const ScopedLogNdc ndc( methodName );
    ostringstream oss;
    oss << "setWalshColumns("
       << " cols90.size = "
       << cols90_.size()
       << " cols180.size = "
       << cols180_.size()
       << " nStates90 = "
       << nStates90_
       << " nStates180 = "
       << nStates180_
       << boolalpha
       << " noiseEnabled = "
       << noiseEnabled_
       << ")"
       ;
    const string remoteCall = oss.str();
    programLogInfoIfPossible( remoteCall );
    const int status = rawClient.setWalshColumns(
            cols90_, cols180_, nStates90_, nStates180_, noiseEnabled_ );

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();
        ostringstream os;
        os << "call to cobra::CorrelatorBandControlClient::"
           << remoteCall
           << " failed with status="
           << status;
        postAbnormalResult( os.str(), methodName.c_str() );
        programLogErrorIfPossible( os.str() );
    } else
        postNormalResult( methodName.c_str() );
}

// ==== EnableCorrelation implementation ===================================
DefaultCorrControl::CobraClient::
EnableCorrelationOpReq::EnableCorrelationOpReq( const bool enable,
                                        const WorkResult & wr ) :
OpReq( wr ),
enable_( enable )
{
}

void
DefaultCorrControl::CobraClient::
EnableCorrelationOpReq::doIt(
    cobra::CorrelatorBandControlClient & rawClient)
{

    int status;
    vector<int> boardStatus;
    if ( enable_ )
        status = rawClient.enableCorrelation( boardStatus );
    else
        status = rawClient.disableCorrelation( boardStatus );

    if ( status < 0 ) {
        // Close the control connection, and reconnect next time
        rawClient.close();

        // Log a message
        ostringstream oss;

        oss <<  "cobra::CorrelatorBandControlClient::"
            << ( enable_ ? "enableCorrelation(" : "disableCorrelation(" )
            << ") failed with status=" << status;

        postAbnormalResult( oss.str(), "EnableCorrelationOpReq::doIt" );
        programLogErrorIfPossible( oss.str() );
    } else
        postNormalResult( "EnableCorrelationOpReq::doIt" );
}

void
DefaultCorrControl::CobraClient::enableCorrelation( bool enable )
{
    ostringstream os;
    os << "enableCorrelation("<< boolalpha << enable <<")";
    const string wrsId = os.str();

    WorkResultSet wrs( wrsId );
    {
        const WorkResult wr = wrs.addKey( "cobra op request" );

        queueOpReq(
            auto_ptr< OpReq >( new EnableCorrelationOpReq(enable , wr) ) );
    }

    waitForAllNormal( wrs, 20000 );
}

//========================================================================
//               DefaultCorrControl class implementation
//========================================================================
struct DefaultCorrControl::AccumDelaySamps {
    cobra::CorrelatorInterpolatorSamples rawSamps;
};


DefaultCorrControl::DefaultCorrControl( const int controlPort ) :
guard_(),
cobraClient_( (controlPort == 0) ?
                  0 :
                  new CobraClient( controlPort ) ),
requestedNoiseSourceState_( INVALID_NOISE_SOURCE_STATE ),
accumDelaySamps_()
{
    // Nothing
}


DefaultCorrControl::~DefaultCorrControl( )
try {
    cobraClient_.reset();
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in "
            "DefaultCorrControl::~DefaultCorrControl - " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exception
    }

    // Just stifle any exceptions
    return;
}


void
DefaultCorrControl::startControlServer( corba::Server & server,
                                        const string & servedObjName,
                                        void (*shutdownCallback)( void * ),
                                        void * const   shutdownCallbackArg )
{
    namespace POA_cco = POA_carma::correlator::obsRecord2;

    server.addServant< POA_cco::Correlator_I_tie >( *this, servedObjName );

    try {
        const string initialNdc =
            "DefaultCorrControl(" + servedObjName + ") producer thread";

        ProducerThreadArgs args( server,
                                 shutdownCallback,
                                 shutdownCallbackArg );

        //programLogInfoIfPossible( "Starting producer thread..." );

        StartPthreadWithCopy( producerThreadEntrypoint,
                              args,
                              initialNdc );

        //programLogInfoIfPossible( "Producer thread started" );
    } catch ( ... ) {
        programLogErrorIfPossible(
            "Starting producer thread generated an exception - " +
            getStringForCaught() );

        throw;
    }
}


void
DefaultCorrControl::setBandwidth( const obsRecord2::BandWidthType bw,
                                  const obsRecord2::FpgaModeType  fpgaMode,
                                  const CORBA::Long               seqNo ,
                                  const CORBA::Long               astroBandNo)

try {
    // Create string to match cobra ini file section for the
    // input bandwidth and fpga modes.
    const string bwText = getIniString( bw , fpgaMode );
    {
        ostringstream oss;

        oss <<  "Setting bandwidth to " << bwText << " with seqNo=" << seqNo
            << " on AstroBand " << astroBandNo;

        programLogInfoIfPossible( oss.str() );
    }

    const ScopedLogNdc ndc( "DefaultCorrControl::setBandwidth" );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    cobraClient_->setBw( bwText, seqNo, astroBandNo );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
DefaultCorrControl::setBandwidthVector( const BandWidthSeq & bw,
                                  const FpgaModeSeq  & fpgaMode,
                                  const CORBA::Long    seqNo ,
                                  const carma::util::SeqLong & astroBandNo)
try {
    const ScopedLogNdc ndc( "DefaultCorrControl::setBandwidth<vector>" );
    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }
    vector<BandWidthType> b = convertSequenceToVector<BandWidthType>(bw);
    vector<FpgaModeType>  f = convertSequenceToVector<FpgaModeType>(fpgaMode);
    vector<unsigned>   abNo = convertSequenceToVector<unsigned>(astroBandNo);
    vector<string> bandwidth;
    for( unsigned short i = 0; i<b.size(); ++i ) {
        bandwidth.push_back( getIniString(b[i], f[i]) );
    }

    const GuardLock guardLock( guard_ );

    cobraClient_->setBw( bandwidth, seqNo, abNo);

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
DefaultCorrControl::flattenPhases( const CORBA::Long seqNo )
try {
    const ScopedLogNdc ndc( "DefaultCorrControl::flattenPhase" );

    ostringstream oss;
    oss << "Performing correlator phase flattening with seqNo = " << seqNo ;
    programLogInfoIfPossible( oss.str() );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    cobraClient_->flattenPhases( seqNo );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
DefaultCorrControl::calibrateSpectra( const CORBA::Boolean enable,
                                      const CORBA::Boolean cache,
                                      const CORBA::Long count,
                                      const CORBA::Long seqNo )
try {
    const ScopedLogNdc ndc( "DefaultCorrControl::calibrateSpectra" );

    ostringstream oss;
    oss << "Performing correlator spectral calibration with seqNo = " << seqNo ;
    programLogInfoIfPossible( oss.str() );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    cobraClient_->calibrateSpectra( enable, cache, count, seqNo );

} catch ( ... ) {
    rethrowCaughtAsUser();
}



void
DefaultCorrControl::setNoiseSourceState( const CORBA::Boolean isOn )
try {
    programLogInfoIfPossible(
        "Setting assumed noise source isOn state to " +
        string( isOn ? "TRUE" : "FALSE" ) );

    const ScopedLogNdc ndc( "DefaultCorrControl::setNoiseSourceState" );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    if ( isOn )
        requestedNoiseSourceState_ = ON_NOISE_SOURCE_STATE;
    else
        requestedNoiseSourceState_ = OFF_NOISE_SOURCE_STATE;

    const string & sourceNameRef =
        ((requestedNoiseSourceState_ == ON_NOISE_SOURCE_STATE) ?
            kNoiseSourceName :
            kRfSourceName);

    cobraClient_->setSourceName( sourceNameRef );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
DefaultCorrControl::optimizeThresholds( const CORBA::Long seqNo )
try {

    const ScopedLogNdc ndc( "DefaultCorrControl::optimizeThresholds" );
    ostringstream oss;
    oss << "Performing correlator threshold optimization with seqNo = "
        << seqNo ;
    programLogInfoIfPossible( oss.str() );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }
    cobraClient_->optimizeThresholds( seqNo );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
DefaultCorrControl::setInputDelayTriplets(
    const DelayTripletSeq & tripletSeq )
try {
    const int seqLength = tripletSeq.length();

    {
        multiset< int > inputNos;
        SetMjdDouble mjd0s;
        SetMjdDouble mjd1s;
        SetMjdDouble mjd2s;

        for ( int i = 0; i < seqLength; ++i ) {
            inputNos.insert( tripletSeq[ i ].inputNumber );
            mjd0s.insert( tripletSeq[ i ].timestamp0 );
            mjd1s.insert( tripletSeq[ i ].timestamp1 );
            mjd2s.insert( tripletSeq[ i ].timestamp2 );
        }

    /* too verbose
        ostringstream oss;

        oss << "Setting delay triplets for inputs "
            << formatAsRanges( inputNos )
            << " with ts0=" << formatSetMjd( mjd0s )
            << ", ts1="     << formatSetMjd( mjd1s )
            << " and ts2="  << formatSetMjd( mjd2s );

        programLogInfoIfPossible( oss.str() );
        */
    }

    const ScopedLogNdc ndc( "DefaultCorrControl::setInputDelayTriplets" );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    if ( accumDelaySamps_.get() == 0 ) {
        accumDelaySamps_ =
            auto_ptr< AccumDelaySamps >( new AccumDelaySamps );
    }

    for ( int i = 0; i < seqLength; ++i ) {
        const int inputNumber = tripletSeq[ i ].inputNumber;

        cobra::CorrelatorInputDelays corrTriplet;
        {
            cobra::CorrelatorTimestamp ts;
            cobra::CorrelatorInputDelay dly;

            ts.mjdDays( tripletSeq[ i ].timestamp0 );
            dly.nanoseconds( tripletSeq[ i ].delay0 );
            corrTriplet.insert( ts, dly );

            ts.mjdDays( tripletSeq[ i ].timestamp1 );
            dly.nanoseconds( tripletSeq[ i ].delay1 );
            corrTriplet.insert( ts, dly );

            ts.mjdDays( tripletSeq[ i ].timestamp2 );
            dly.nanoseconds( tripletSeq[ i ].delay2 );
            corrTriplet.insert( ts, dly );
        }

        const cobra::CorrelatorInterpolatorSamples::iterator it =
            accumDelaySamps_->rawSamps.find( inputNumber );

        if ( it == accumDelaySamps_->rawSamps.end() ) {
            accumDelaySamps_->rawSamps.insert(
                make_pair( inputNumber, corrTriplet ) );
        } else
            it->second = corrTriplet;
    }

    const string & sourceNameRef =
        ((requestedNoiseSourceState_ == ON_NOISE_SOURCE_STATE) ?
            kNoiseSourceName :
            kRfSourceName);

    cobraClient_->setDelaySamps( accumDelaySamps_->rawSamps, sourceNameRef );
} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
DefaultCorrControl::setWalshColumns(
                const SeqLong & cols90,
                const SeqLong & cols180,
                CORBA::Long nStates90,
                CORBA::Long nStates180,
                CORBA::Boolean noiseEnabled )
try {

    const ScopedLogNdc ndc( "DefaultCorrControl::setWalshColumns" );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }
    const GuardLock guardLock( guard_ );
    vector<int> v90 = convertSequenceToVector<int>(cols90);
    vector<int> v180 = convertSequenceToVector<int>(cols180);
    cobraClient_->setWalshColumns( v90, v180, nStates90, nStates180, noiseEnabled);

} catch ( ... ) {
    rethrowCaughtAsUser();
}


void
DefaultCorrControl::setDownconverterSettings(
    const CORBA::Double  freqInGHz,
    const carma::correlator::obsRecord2::SidebandType sb,
    const CORBA::Boolean bdcEnabled )
try {
    const bool sbIsUpper = (sb == carma::correlator::obsRecord2::UPPER_SB);

    {
        ostringstream oss;

        oss << "setting assumed downconverter frequency to " << freqInGHz
            << " GHz and sideband to " << (sbIsUpper ? "UPPER" : "LOWER")
            << ", blockDconEnabled = " << boolalpha << bdcEnabled
            ;

        programLogInfoIfPossible( oss.str() );
    }

    const ScopedLogNdc ndc( "DefaultCorrControl::setDownconverterSettings" );

    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const float cobraFreqInHz = static_cast< float >( freqInGHz * 1e9 );

    const GuardLock guardLock( guard_ );

    cobraClient_->setDcSettings( cobraFreqInHz, sbIsUpper, bdcEnabled );
} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
DefaultCorrControl::setDownconverterSettingsVector( const carma::util::SeqFloat  & freqs )
try {
    const ScopedLogNdc ndc( "DefaultCorrControl::setDownConvertersettings<vector>" );
    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }
    // convert to vector then from GHz to Hertz
    vector<float> f = convertSequenceToVector<float>(freqs);
    for(vector<float>::iterator i= f.begin(); i!=f.end(); ++i )
        *i *= 1.0E9;

    const GuardLock guardLock( guard_ );
    cobraClient_->setDcSettings( f );

} catch ( ... ) {
    rethrowCaughtAsUser();
}

void
DefaultCorrControl::enableCorrelation( const CORBA::Boolean enable )
try {
    const ScopedLogNdc ndc( "DefaultCorrControl::enableCorrelation" );

    {
        // debug
        ostringstream os;
        os << " ( " << boolalpha << enable << ")";
        programLogInfoIfPossible( os.str() );
    }



    if ( cobraClient_.get() == 0 ) {
        programLogErrorIfPossible( "cobraClient_ is NULL" );
        return;
    }

    const GuardLock guardLock( guard_ );

    cobraClient_->enableCorrelation( enable );

} catch ( ... ) {
    rethrowCaughtAsUser();
}
