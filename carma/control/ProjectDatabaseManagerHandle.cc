/**
 * CARMA control Project Database Manager interface implementation
 *
 * @author Douglas N. Friedel
 * @author Marc Pound
 */

#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/ProjectDatabaseManagerHandle.h"
#include "carma/monitor/ProjectDatabaseManagerSubsystem.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedLogNdc.h"

using namespace std;
using namespace carma;
using namespace carma::control;
using namespace carma::observertools;
using namespace carma::monitor;
using namespace carma::util;

namespace {

// lock for manipulating thread-sensitive member variables
::pthread_mutex_t gReadWriteLock = PTHREAD_MUTEX_INITIALIZER;
typedef ScopedLock< ::pthread_mutex_t > ReadWriteLock;

} // namespace < anonymous >


ProjectDatabaseManagerHandle::ProjectDatabaseManagerHandle(
    MonitorSystem &                   carmaMonitor,
    ControlSubsystemBase::Reachable & reachable ) :
ProjectDatabaseManagerRemoteObjHandle(
    PROJECT_DATABASE_MANAGER_NAME,
    &(reachable.projectDatabaseManager()),
    &(carmaMonitor.projectDatabaseManager()),
    &(carmaMonitor),
    true, // log remote calls
    true  // log unreachable
    ),
    queryResultId_(-1),
    editResultId_(-1),
    editSuccess_(false),
    runId_(-1),
    trialId_(-1),
    projSeq_(new observertools::ProjectSequence)
{
}


ProjectDatabaseManagerHandle::~ProjectDatabaseManagerHandle( )
try {
} catch ( ... ) {
    // stifle all exceptions
    
    return;
}


void
ProjectDatabaseManagerHandle::projectQuery( 
        const observertools::ItemValueSequence ivSeq,
        const int requestId )
{
    // Note SaCI::queryProject is mutexed
    // Three cases to consider
    // 1. Normal operation
    //      i) Call comes in to SaCI::queryProject.
    //     ii) SaCI::queryProjects creates result ID and
    //       submits a project query that will be tagged
    //       with that result ID when completed.
    //    iii) Workerpool is spawned off 
    //        a) out projectSequence parameter is declared but not initialzed.
    //        b) DO initializes and fills in the out parameter psv.
    //        c) projSeq_ takes ownership of the reference, freeing 
    //           whatever projSeq_ previously held.
    //        d) queryResultId_ is set to ID generated by SaCI.
    //    iv) Worker completes normally within the timeout
    //     v) SaCI::queryProject calls getProjectSequence( requestID ), 
    //        taking ownership of the ProjectSequence reference
    //     vi) SaCI::queryProject completes normally
    //
    // 2. Worker Timed out
    //      i) Call comes in to SaCI::queryProject.
    //     ii) SaCI::queryProjects creates result ID and
    //       submits a project query that will be tagged
    //       with that result ID when completed.
    //    iii) WorkerPool is spawned off 
    //       CASE 1) WorkerPool times out and DO call never returns
    //        a) out projectSequence parameter is declared but not initialzed.
    //        b) psv does not get initialized nor filled in
    //           resultID_ not incremented, stays at value for last 
    //            successful query
    //           projSeq_ points to last succesful query
    //        c) SaCI::queryProject calls getProjectSequence( requestID ), 
    //        which throws exception because requestID does not match queryResultId_.
    //        d) Exception is caught & rethrown by queryProject back to python 
    //           level.
    //       CASE 2) WorkerPool times out and DO call completes late, and
    //       an overlapping query comes in.
    //          a) out projectSequence parameter is declared but not initialzed.
    //          b) queryResultId_/projectSeq_ manipulation is locked
    //             against overlapping read/write.
    //          c) If the late query writes before the new query gets
    //          to read, then the new query will throw an exception that
    //          the resultIds don't match. Said exception is thrown back 
    //          to python.
    //          d) If the late query writes after the new query reads,
    //          it doesn't matter.
    //
    //
    // 3. Exception
    //     i) Exception before DO call caught by SaCI::queryProject and rethrown
    //     back to python.  resultID_ and projSeq_ unchanged from last
    //     successful values.
    //     ii) Exception from DO call or psv._retn() logged and resultID_
    //        set to -1.  SaCI::queryProject call to 
    //        getProjectSequence(requestID)
    //        will throw exception because requestID does not match queryResultId_.
    //        Exception is caught & rethrown by queryProject back to 
    //        python level.
    //
    programLogNoticeIfPossible("PDBMH");
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::queryProject");

    if (attemptToReconnectIfNeeded()) {
        programLogNoticeIfPossible("PDBMH 1");
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << setiosflags(ios::fixed)
                << "ProjectDatabaseManagerHandle::queryProject("
                << itemValueSequenceToString( ivSeq )
                << ")";
                
            remoteCallString = oss.str( );
        }
        programLogNoticeIfPossible("PDBMH 2");

        try {
            const double sendTime = Time::MJD();
            programLogNoticeIfPossible("PDBMH 3");
            
            // Note we do not need to initialize psv,
            // as it is an out-parameter and initialization
            // is done by the server. 
            // Calling _var.out() is the same as passing in the _var
            // but makes the code intent more transparent.
            observertools::ProjectSequence_var psv;
            programLogNoticeIfPossible("PDBMH 3.5");
            remoteObj()->projectQueryInOut( ivSeq, psv.out() );
            programLogNoticeIfPossible("PDBMH 3.75");

            logSentCommandIfNeeded( remoteCallString, sendTime );
        { 
            const ReadWriteLock lock( gReadWriteLock );
            if ( queryResultId_ > requestId ) {
                programLogNoticeIfPossible("PDBMH 4");
                // The remote call was took too long to return
                // and another query has lapped it.
                // Don't overwrite the 2nd query
                programLogNoticeIfPossible("releasing read-write lock via timeout return");
                return;
            }
            // Avoid a deep copy.
            // projSeq_ take ownership as _retn() relinquishes.
            // Note this will also release any reference 
            // that projSeq_ was previously pointing to.
            programLogNoticeIfPossible("PDBMH 5");
            projSeq_ = psv._retn();
            programLogNoticeIfPossible("PDBMH 6");

            // on success, set the resultID
            queryResultId_ = requestId;
        }

        }
        catch ( const carma::observertools::ProjectDatabaseException & ex){
              // gonna match on the next read anyway
                programLogNoticeIfPossible("PDBMH 99");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                queryException_ = oss.str();

                queryResultId_ = -1;
        }
        catch ( const carma::util::UserException & ex ) {
                programLogNoticeIfPossible("PDBMH 88");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                queryException_ = oss.str();
                queryResultId_ = -1;
        }
        catch ( const CORBA::Exception & ex ) {
            { // ehh, probably don't need this.  queryResultId_ is not
              // gonna match on the next read anyway
                programLogNoticeIfPossible("PDBMH 77");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex;
                queryException_ = oss.str();

                queryResultId_ = -1;
            }
            processException( remoteCallString, ex );
        }
    } 
}


observertools::ProjectSequence * 
ProjectDatabaseManagerHandle::getProjectSequence( const int id )
{
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::getProjectSequence");
    const ReadWriteLock lock( gReadWriteLock );

    if ( id == queryResultId_ ) {
        return projSeq_._retn();
    } else {
        ostringstream os;
        if ( id < queryResultId_ ) 
            os << "Mismatched project edit IDs ( "
                << id << "," << queryResultId_ << "). "
                << "An overlapping call wiped out the previous instance "
                << queryResultId_ << ".";
        else if( queryResultId_ == -1 )
            os << "An exception was thrown: " << queryException_;
        else
            os << "The PDB remote call #" 
               << id << " did not return or returned late."
               << " If you continue to get errors like this, you should restart the pdbmHost process";

        throw CARMA_ERROR( os.str() );
    }
}


void
ProjectDatabaseManagerHandle::addScriptOrCatalog( 
    const observertools::ProjectId pid,
    const char * scriptFile, 
    const char * catalogFile )
{

    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::addScriptOrCatalog");
    if (attemptToReconnectIfNeeded( )) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << setiosflags(ios::fixed)
                << "ProjectDatabaseManagerHandle::addScriptOrCatalog("
                << observertools::projectIdToObsblockId( pid )
                << " , "
                << scriptFile  << " , "
                << catalogFile
                << ")";
                
            remoteCallString = oss.str( );
        }

        try {
            const double sendTime = Time::MJD();
            
            remoteObj()->projectOscriptAdd(
                pid.project.c_str(), 
                pid.obsblock.c_str(), 
                pid.subobsblock.c_str(), 
                scriptFile,
                catalogFile
               );
            logSentCommandIfNeeded( remoteCallString, sendTime );

        }
        catch ( const CORBA::Exception & ex ) {
            processException( remoteCallString, ex );
        }
    } 
}


void
ProjectDatabaseManagerHandle::projectEdit( 
	        const observertools::ProjectId pid,
		const observertools::ItemValueSequence ivSeq,
		const observertools::EditStatus action,
		const int editRequestId
		)
{
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::projectEdit");
    if (attemptToReconnectIfNeeded()) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << setiosflags(ios::fixed)
                << "ProjectDatabaseManagerHandle::projectEdit("
                << observertools::projectIdToObsblockId( pid )
                << " , "
                << itemValueSequenceToString( ivSeq ) << " , "
                << action
                << ")";
                
            remoteCallString = oss.str( );
        }

        try {
            const double sendTime = Time::MJD();
            
            // This parameter passed by reference, but
            // does not need to be declared
            // as Boolean_out ( typedef Boolean & ).
            // See Henning&Vinoski page 277.
            CORBA::Boolean editSuccessOut = false;
            CORBA::Short trial = pid.trial;
            programLogNoticeIfPossible("PDBMH E1");
            remoteObj()->projectEditInOut(
                pid.project.c_str(), 
                pid.obsblock.c_str(), 
                pid.subobsblock.c_str(), 
                trial, ivSeq, action, editSuccessOut
            );

            programLogNoticeIfPossible("PDBMH E2");
            logSentCommandIfNeeded( remoteCallString, sendTime );

            { 
                // note we use the same lock instance as for 
                // projectQuery to avoid inconsistencies in
                // a projectQuery overlapping a projectEdit.
                const ReadWriteLock lock( gReadWriteLock );
                if ( editResultId_ > editRequestId ) {
                    // The remote call was took too long to return
                    // and another query has lapped it.
                    // Don't overwrite the 2nd query
                    programLogNoticeIfPossible("releasing read-write lock via timeout return");
                    return;
                }

                // According to Doug, "the end user should never
                // get a false returned to them." The false
                // return state only happens on internal calls;
                if ( editSuccessOut == false )
                {
                    ostringstream os;
                    os << remoteCallString << " returned FAILED state." ;
                    programLogErrorIfPossible( os.str() );
                }

                editSuccess_ = editSuccessOut;

                // on success, set the resultID
                editResultId_ = editRequestId;
            }
        }
        catch ( const carma::observertools::ProjectDatabaseException & ex){
              // gonna match on the next read anyway
                programLogNoticeIfPossible("PDBMH 99");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                editException_ = oss.str();
                editResultId_ = -1;
        }
        catch ( const carma::util::UserException & ex ) {
                programLogNoticeIfPossible("PDBMH 88");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                editException_ = oss.str();
                editResultId_ = -1;
        }
        catch ( const CORBA::Exception & ex ) {
            { 
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex;
                editException_ = oss.str();
                editResultId_ = -1;
            }
            processException( remoteCallString, ex );
        }
    } 
}


bool
ProjectDatabaseManagerHandle::getEditResult( const int id )
{
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::getEditResult");
    const ReadWriteLock lock( gReadWriteLock );
    if ( id == editResultId_ ) {
        return editSuccess_;
    } else {
        ostringstream os;
        if ( id < editResultId_ ) 
            os << "Mismatched project edit IDs ( "
               << id << "," << editResultId_ << "). "
               << "An overlapping call wiped out the previous instance "
               << editResultId_ << ".";
        else if( editResultId_ == -1 )
            os << "An exception was thrown: " << editException_;
        else 
            os << "The PDB remote call #" << id 
               << " did not return or returned late."
               << " If you continue to get errors like this, you should restart the pdbmHost process";

        throw CARMA_ERROR( os.str() );
    }
}



void
ProjectDatabaseManagerHandle::runProject( const char* projectID, const char* obsblock,
                     const char* subObsblock, const bool isCommissioning,
                     const bool isDualCorr, const char* arrayConfig1,
                     const char* arrayConfig2,
                     const char* scriptFile, const char* catalogFile,
                     const int requestId
		)
{
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::runProject");
    if (attemptToReconnectIfNeeded()) {
        string remoteCallString;
        {
            ostringstream oss;
            
            oss << setiosflags(ios::fixed)
            << "ProjectDatabaseHandle::projectRun( projectID = "
	        << projectID << ", obsblock = " << obsblock << ", subObsblock = "
	        << subObsblock << ", isCommissioning = " << isCommissioning
	        << ", isDualCorr = " << isDualCorr << ", arrayConfig1 = "
	        << arrayConfig1 << ", arrayConfig2 = " << arrayConfig2
	        << ", scriptFile = " << scriptFile << ", catalogFile = "
	        << catalogFile << " )";
                
            remoteCallString = oss.str( );
        }

        try {
            const double sendTime = Time::MJD();
            
            CORBA::Short trialIdOut = -1;
            programLogNoticeIfPossible("PDBMH R1");
            remoteObj()->runProjectInOut(projectID,
                obsblock,subObsblock,
                isCommissioning,isDualCorr,
                arrayConfig1,arrayConfig2,
                scriptFile,catalogFile,
                trialIdOut
            );
            programLogNoticeIfPossible("PDBMH R2");
            logSentCommandIfNeeded( remoteCallString, sendTime );

            { 
                // note we use the same lock instance as for 
                // projectQuery to avoid inconsistencies in
                // a projectQuery overlapping a projectEdit.
                const ReadWriteLock lock( gReadWriteLock );
                if ( runId_ > requestId ) {
                    // The remote call was took too long to return
                    // and another query has lapped it.
                    // Don't overwrite the 2nd query
                    programLogNoticeIfPossible("releasing read-write lock via timeout return");
                    return;
                }

                trialId_ = trialIdOut;

                // on success, set the resultID
                runId_ = requestId;
            }
        }
        catch ( const carma::observertools::ProjectDatabaseException & ex){
              // gonna match on the next read anyway
                programLogNoticeIfPossible("PDBMH 99");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                runException_ = oss.str();
                runId_ = -1;
        }
        catch ( const carma::util::UserException & ex ) {
                programLogNoticeIfPossible("PDBMH 88");
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex.errorMsg;
                runException_ = oss.str();
                runId_ = -1;
        }
        catch ( const CORBA::Exception & ex ) {
            { 
                const ReadWriteLock lock( gReadWriteLock );
                ostringstream oss;
                oss << ex;
                runException_ = oss.str();
                runId_ = -1;
            }
            processException( remoteCallString, ex );
        }
    } 
}


short
ProjectDatabaseManagerHandle::getRunResult( const int id )
{
    ScopedLogNdc ndc("ProjectDatabaseManagerHandle::getRunResult");
    const ReadWriteLock lock( gReadWriteLock );
    if ( id == runId_ ) {
        return trialId_;
    } else {
        ostringstream os;
        if ( id < runId_ ) 
            os << "Mismatched project run IDs ( "
               << id << "," << runId_ << "). "
               << "An overlapping call wiped out the previous instance "
               << runId_ << ".";
        else if( runId_ == -1 )
            os << "An exception was thrown: " << runException_;
        else 
            os << "The PDB remote call #" << id 
               << " did not return or returned late."
               << " If you continue to get errors like this, you should restart the pdbmHost process";

        throw CARMA_ERROR( os.str() );
    }
}
