#ifndef CARMA_DBMS_RESULTSCACHE_H
#define CARMA_DBMS_RESULTSCACHE_H

/**
 * @file
 *
 * ResultsCache class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include <string>
#include <map>
#include "carma/dbms/Column.h"
#include "carma/dbms/Typedefs.h"
#include "carma/util/Logger.h"
#include "carma/util/PthreadMutex.h"

namespace carma {
    namespace dbms {

        class DBConnection;
        class DBConfigurator;
        class Table;
/**
 *  A class for caching and retrieving results of database queries.  
 *  This class is instantiated as a singleton via a factory method.  
 * Usage: to get the singleton instance of this object, use<br>
 * <code>ResultsCache &rc = ResultsCache::getCache()</code><br>
 * To indicate that
 * the application is finished with the singleton (and to free the memory
 * it is using), use <br>
 * <code>ResultsCache::closeCache()</code><br>
 * Note the recommended usage of these methods is from the parent application
 * (rather than by the objects it instantiates), since a call to <br><code>
 * closeCache()</code><br> will delete the singleton, so that any objects still
 * using it will get a shock. Similarly, no object should rely solely on the
 * existance of singleton. If they wish to use it, they should check for
 * its existence by calling <br><code>ResultsCache::cacheExists()</code><br>
 * and if it doesn't exist create it (not recommended) or run
 * alternative code to submit the necessary db query.
 * This class is thread-safe, all methods which alter the singleton or other
 * static data members are wrapped in carma::util::Mutex lock()/unlock() calls.
 * @todo need to set (and check for) upper bound to the amount of memory that
 * can be used by cached objects. 
 < 
 */
class ResultsCache {
public:


    /**
     * get the singleton instance of the ResultsCache, the specified (valid)
     * DBConnection object will be used for database queries
     * @param dbc a valid database connection, in the case where the singleton
     *        ResultsCache object already exists, this parameter is ignored
     * @return the single instance of the results cache
     */
    static ResultsCache& getCache(const DBConnection * const dbc=NULL);

    /**
     * indicate that one's use of the cache is complete.  If we are 
     * The singleton instance will be deleted. It is safe to call 
     * getCache() at anytime before or after this function is called.  
     * However, calling this method deletes all cached results
     */
    static void closeCache();

    /**
     * does the singleton currently exist?
     * @return true if the singleton exists
     */
    static bool cacheExists();

    /**
     * return (and cache if not already done) a mapping of all tagIDs in the
     * database to their corresponding monitor configurations
     * @return the map of tagIDs to monitor configurations
     * REMOVING BECAUSE VERY EXPENSIVE
     */
    //const carma::dbms::ID2MDMap& getMonitorConfigurations();
    
    /**
     * return (and cache if not already done) the full monitor configuration 
     * table. This table includes all configurations, past and present, which
     * means there may be multiple entries for some tagIDs.  To get just
     * the current configurations, use getCurrentMonitorConfigurationTable()
     * instead.
     * @return the full monitor configuration table
     */
    const carma::dbms::Table& getFullMonitorConfigurationTable();

    /**
     * get a table containing the current monitor configurations, This table
     * is guaranteed to have exactly one entry per tagID
     * @return the table of current configurations
     */
    const carma::dbms::Table& getCurrentMonitorConfigurationTable();

    /**
     * return the map of tagIDs to canonical names
     */
    const std::map<int,std::string>& getTagIDToNameMap() const;

    /**
     * return (and cache if not already done) a column listing all the
     * tagIDs in the DB
     * @return all the tagIDs in the DB
     */
    //const carma::dbms::Column<int>& getAllTagIDs();

    /**
     * return (and cache if not already done) the MonitorDataTableIndex table
     * @return the MonitorDataTableIndex table
     */
    //const carma::dbms::Table& getMonitorDataTableIndexTable();

    /**
     * add an object to the cache of results tables
     * @param id the string identifier for this table for later retrieval
     * @param table the table to cache
     * @throws IllegalArgumentException if the identifier already exists
     *         in this cache
     */
    void cache(const std::string& identifier, const carma::dbms::Table& table);

    /**
     * get a cached table from the table cache
     * @param identifier the table identifier
     * @return the table corresponding to the identifier
     * @throws carma::util::NotFoundException if the identifier does not exist
     *         in the table cache
     */
    const carma::dbms::Table& getTable(const std::string& identifier) const;
    
    /**
     * does the specified id (and hence table) exist in the table cache?
     * @param identifier the table identifier
     * @return true if the id exists in the table cache
     */
    bool tableIDExists(const std::string identifier) const;

protected:
    ResultsCache();
    ResultsCache(const carma::dbms::DBConnection* const dbc);

    
private:
    //static carma::dbms::ID2MDMap *tagID2MD_;
    const static carma::dbms::Table *fullMCTable_;
    const static carma::dbms::Table *currentMCTable_;
    //const static carma::dbms::Column<int> *allTagIDs_;
    const carma::dbms::DBConnection *dbc_;
    log4cpp::Category& logger_;
    // number of clients currently using the singleton
    static ResultsCache *resultsCache_;
    static std::map<std::string,carma::dbms::Table> * tableCache_;
    static std::map<int,std::string> * tagIDToNameMap_;

    /**
     * mutex for locking code which preforms write operations on static
     * members, we could use a number of mutexes, one per method say, but
     * that is probably overkill, can do it if it seems necessary in the
     * future
     */
    static carma::util::PthreadMutex resultsCacheMutex_;
    
};

}}


#endif // CARMA_DBMS_RESULTSCACHE_H
