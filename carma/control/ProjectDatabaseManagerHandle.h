#ifndef CARMA_CONTROL_PROJECTDATABASEMANAGER_HANDLE_H
#define CARMA_CONTROL_PROJECTDATABASEMANAGER_HANDLE_H

/**
 *
 * @file
 *
 * Carma control interface to the Project Database Manager
 *
 * @author Douglas N. Friedel
 * @author Marc Pound
 *
 */
#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/observertools/ItemValue.h"
#include "carma/observertools/ProjectDatabaseManager.h"
#include "carma/observertools/ProjectDatabaseManager_skel.h"
#include "carma/observertools/PDB_Util.h"

namespace carma{
namespace control{

typedef RemoteObjHandleT<observertools::ProjectDatabaseManager>
    ProjectDatabaseManagerRemoteObjHandle;

//! @brief Manages Project Database Manager control DO connections
class ProjectDatabaseManagerHandle : 
    public ProjectDatabaseManagerRemoteObjHandle{
    public:
    /**
     * Constructor
     *
     * @param carmaMonitor carma::monitor::MonitorSystem& monitor system
     * @param subarrayMonitor carma::monitorControlSubsystemBase::Subarray&
     */
    ProjectDatabaseManagerHandle(
	monitor::MonitorSystem &carmaMonitor,
	monitor::ControlSubsystemBase::Reachable & reachable);

    virtual ~ProjectDatabaseManagerHandle();

    /**
     * Query the project database for a project sequence.
     * Note this method has void return signature because
     * we pass it through makeHandleMethodFunctorGroup, which
     * can only take void methods.
     * The return project sequence is passed back through
     * the ProjectSequence_var argument.
     * @param ivSeq The itemValueSequence describing the query
     * @param projSeq The returned project Sequence.
     * @param requestId An id number that keeps track of multithreaded
     * project queries. The value of this number is set by SubarrayControl.
     */
    void projectQuery( const observertools::ItemValueSequence ivSeq,
	               const int requestId );

    /**
     * @return the projectSequence associated with the input
     * request id
     * @param requestId An id number that keeps track of multithreaded
     * project queries. The value of this number is set by SubarrayControl.
     * An access request here should match the id from a previous call
     * to projectQuery.
     */
    observertools::ProjectSequence * getProjectSequence( int requestId );

    /**
     * Add a script and/or catalog to a project
     */
    void addScriptOrCatalog( const observertools::ProjectId pid,
			     const char * scriptFile,
			     const char * catalogFile
	                   );

    /**
     * Edit a project
     * @param pid The Project
     * @param ivSeq The itemValueSequence describing the items to be editted.
     * @param action A enumerated action (e.g., COPY, DELETE, etc)
     * @param editRequestId An id number that keeps track of multithreaded
     * project edits. The value of this number is set by SubarrayControl.
     */
    void projectEdit( 
	        const observertools::ProjectId pid,
		const observertools::ItemValueSequence ivSeq,
		const observertools::EditStatus action,
		const int editRequestId
		);

    /**
     * @return the success state of a previous call to projectEdit
     * @param editRequestId An id number that keeps track of multithreaded
     * project edits. The value of this number is set by SubarrayControl.
     * An access request here should match the id from a previous call
     * to projectEdit
     */
    bool getEditResult( int editRequestId );


    void runProject(const char* projectID, const char* obsblock,
                     const char* subObsblock, const bool isCommissioning,
                     const bool isDualCorr, const char* arrayConfig1,
                     const char* arrayConfig2,
                     const char* scriptFile, const char* catalogFile,
                     const int requestId);
    
    short getRunResult( int requestId );                 
    
    private:

    int queryResultId_;
    std::string queryException_;
    int editResultId_;
    std::string editException_;
    bool editSuccess_;
    int runId_;
    std::string runException_;
    short trialId_;
    observertools::ProjectSequence_var projSeq_;

};

} //namespace control
} //namespace carma

#endif  //CARMA_CONTROL_PROJECTDATABASEMANAGER_HANDLE_H
