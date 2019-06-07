#ifndef CARMA_OBSERVERTOOLS_PROJECTDATABASEMANAGER_H
#define CARMA_OBSERVERTOOLS_PROJECTDATABASEMANAGER_H

#include <carma/observertools/ProjectDatabaseManager.h>
#include <carma/observertools/PDB_Monitor.h>
#include <carma/util/PthreadMutex.h>
#include <boost/shared_ptr.hpp>
#include <string>

namespace carma {
namespace observertools {

struct PDBMArgs {
    std::string hostname;
    unsigned short port;
    std::string database;
    bool rt;

    PDB_Monitor_Ptr monitor;
    double awdelay;
};

class ProjectDatabaseManagerImpl
{
public:
    /**
     * Constructor
     *
     * @param args the PDBMArgs structure
     */
    ProjectDatabaseManagerImpl(const struct PDBMArgs &args);

    /**
     * Check the database for consistency.
     *
     * This ensures that all items in the database have the correct schema.
     *
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     */
    void checkDatabase() const;

    /**
     * <p>Query the database for projects
     *
     * @param theQuery the query parameters
     * @param isControl Is control asking the query - this reduces the returned
     *   data
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    ProjectSequence *projectQuery(const ItemValueSequence &theQuery) const;

    /**
     * Version of projectQuery which sets the project sequence value
     * via an out parameter instead of a return value.
     */
    void projectQueryInOut(
            const ItemValueSequence &theQuery,
            ProjectSequence_out pSeq) const;

    /**
     * Find the observing script of the given project and obsblock
     *
     * @param projectID the project ID (e.g. c0002)
     * @param obsblock the obsblock name
     * @param subObsblock the sub-obsblock name
     * @return "none" if there is no observing script
     *
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    StringSequence *projectOscriptFind(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock) const;

    /**
     * Add an observing script to a trial
     * @param projectID the project ID (e.g. c0002)
     * @param obsblock the obsblock name
     * @param subObsblock the sub-obsblock name
     *
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    void projectOscriptAdd(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock,
            const char *scriptFile,
            const char *catalogFile) const;

    /**
     * Edit items in the given project
     * @param projectID the project ID (e.g. c0002)
     * @param obsblock the obsblock name
     * @param subObsblock the sub-obsblock name
     * @param trial the trial number
     * @param itemValueSeq the items and values to be edited
     *
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    CORBA::Boolean projectEdit(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock,
            const CORBA::Short trial,
            const ItemValueSequence &editItems,
            const EditStatus action) const;

    /**
     * Version of projectEdit which sets the boolean success value
     * via an out parameter instead of a return value.
     */
    void projectEditInOut(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock,
            const CORBA::Short trial,
            const ItemValueSequence &editItems,
            const EditStatus action,
            const CORBA::Boolean_out success) const;

    CORBA::Short runProject(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock,
            const bool isCommissioning,
            const bool isDualCorr,
            const char *arrayConfig1,
            const char *arrayConfig2,
            const char *scriptFile,
            const char *catalogFile) const;

    void runProjectInOut(
            const char *projectID,
            const char *obsblock,
            const char *subObsblock,
            const bool isCommissioning,
            const bool isDualCorr,
            const char *arrayConfig1,
            const char *arrayConfig2,
            const char *scriptFile,
            const char *catalogFile,
            CORBA::Short &trialID) const;

    CORBA::Short isUp() const;

    CORBA::Boolean isCommissioning(const char *pid) const;

    /**
     * Add project(s) to the database
     * @param filename the name of the file containing the project(s) to add
     *
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    void projectAdd(const char *fileName) const;

    /**
     * Add project(s) to the database
     * @param xmlString a string containing the xml entries for the new project
     * @throw CORBA::SystemExeption
     * @throw carma::util::UserException
     * @throw carma::observertools::ProjectDatabaseException
     */
    bool projectAddAsString(const char *xmlString) const;

    /**
     * Convert a numeric grade to its letter equivalent.
     * @param grade The input numeric grade.
     * @see letterToGrade(const char *)
     */
    char *gradeToLetter(float grade) const;

    /**
     * Convert a letter grade to its numeric equivalent.
     * @param letter The input letter grade, e.g. "B+"
     * @see gradeToLetter(float)
     */
    float letterToGrade(const char *letter) const;

private:
    // No copying
    ProjectDatabaseManagerImpl(const ProjectDatabaseManagerImpl &rhs);
    ProjectDatabaseManagerImpl& operator=(const ProjectDatabaseManagerImpl &rhs);

    const struct PDBMArgs args_;
    mutable carma::util::PthreadMutex mutex_;
};

} // namespace carma::observertools
} // namespace carma

/* vim: set ts=4 sts=4 sw=4 et: */

#endif // CARMA_OBSERVERTOOLS_PROJECTDATABASEMANAGER_H
