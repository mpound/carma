#ifndef CARMA_DBMS_MONITORDATAQUERYMANAGER_H
#define CARMA_DBMS_MONITORDATAQUERYMANAGER_H

/**
 * @file
 * MonitorDataQueryManager class.
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataQueryManager.h,v 1.24 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <set>
#include <vector>
#include "carma/util/Time.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/MonitorPointSelector.h"

namespace carma {
namespace dbms {

    class DBConnection;
    class Filter;
    class OneComponentFilter;
    class MonitorPointFilter;
    //class MultiComponentFilter;
    //class Selector;
    class TimeRangeFilter;
    class TagIDFilter;
    class TagIDSetFilter;
    

/**
 * Class to compose and execute a query of the monitor database.
 * A query is specified using a set of 
 * <code>carma::dbms::MonitorPointSelector</code>s, a 
 * <code>carma::dbms::TimeRangeFilter</code>, and an optional set of 
 * <code>carma::dbms::MonitorPointFilter</code>s. 
 * If a monitor point is represented (via its <code>tagID</code>) in the
 * set of specified <code>MonitorPointSelector</code>s but is not represented
 * in the set of <code>MonitorPointFilter</code>s, the filter that is applied
 * is based on its <code>tagID</code> alone.<P>
 * <h2>Query Types</h2>
 * <h3>Narrow Result Set Queries</h3>
 * This class permits two types of queries.  The first type, referred to as
 * <b>narrow result set queries</b>, are queries which isolate each monitor
 * point in the set of <code>MonitorPointSelector</code>s. For each monitor
 * point, the following query is performed:<br>
 * <code>SELECT [selection specification] FROM T where 
 * [time range filter specification] AND [monitor point filter specification]
 * </code><br>
 * where <code>[selection specification]</code> is determined from the 
 * <code>MonitorPointSelector</code>, <code>T is the table or union of the 
 * set of tables which contain times falling within the 
 * <code>TimeRangeFilter</code>, <code>[time range filter specification]</code>
 * represents the filtering imposed by the <code>TimeRangeFilter</code>, and
 * <code>[monitor point filter specification]</code> is the filtering imposed 
 * by the <code>MonitorPointFilter</code> with the same <code>tagID</code>
 * as the <code>MonitorSelector</code> in question, if any.  If there
 * is no <code>MonitorPointFilter</code> with the same tagID as the 
 * <code>MonitorPointSelector</code> in question, a filter of the form<br>
 * <code>tagID=[the tagID of the MonitorPointSelector in question]</code></br>
 * is used.<p>
 * Thus, a result set is produced for each <code>MonitorPointSelector</code>,
 * using the specified <code>TimeRangeFilter</code> and a filter specific
 * to the monitor point in question. These queries are executed, and the
 * result sets returned to the client using the <code>execNarrowQueries()
 * </code> method.
 * <h3>Wide Result Set Query</h3>
 * The second form of query this class allows is the so-called <b>
 * wide result set query</b>.  In this case, all the 
 * <code>MonitorPointSelector</code>s and <code>MonitorPointFilter</code>s
 * (if any) are combined in a single query which generally requires several
 * table joins. The produced result set has the same number of columns as
 * the sum of all the columns specified by all the 
 * <code>MonitorPointSelector</code>s, (hence the name <b>wide result set</b>).
 * Individual <code>MonitorPointFilter</code>s are all ANDed together and
 * ANDed with the <code>TimeRangeFilter</code> to produce a single filter.
 * Again, in cases where a <code>MonitorPointSelector</code> does not have
 * a corresponding <code>MonitorPointFilter</code>, a simple filter on tagID
 * is used. This query is executed and the wide result set returned to the
 * client via the <code>execWideQuery()</code> method.  Because some truly
 * monstrous result sets can be produced in this way (and because doing so
 * can bring the database server to its knees) there are limitations placed
 * on how many <code>MonitorPointSelector</code>s and 
 * <code>MonitorPointFilter</code>s can be specified, which ultimately 
 * determines the maximum number of allowed columns in the result set and
 * the maximum number of table joins which are permitted to produce it.
 * FIXME: Guidelines still to come.<p>
 * <h2>Other Notes</h2><br>
 * This class attempts to
 * use the <code>carma::dbms::ResultsCache</code> singleton, and thus 
 * the parent application should instantiate the singleton via a call to
 * <code>ResultsCache::getCache()</code> (and should also dispose of the
 * singleton when all objects have finished with it via a call to <br><code>
 * ResultsCache::closeCache()</code><br>).
 * If run at the telescope site where only recent data are kept, the 
 * <code>shouldCheckArchive()</code> method can be run to determine if the
 * time range may include data which is no longer in the site database and
 * therefore should be submitted to the long term archive.
 * @see carma::dbms::MonitorPointSelector
 * @see carma::dbms::MonitorPointFilter
 * @see carma::dbms::TimeRangeFilter
 * @see carma::dbms::ResultsCache
 */
class MonitorDataQueryManager {

public:
    /**
     * constructor
     * @param time range filter which also contains information on what average
     *        type to search for
     * @param otherFilters other filter to apply, will be AND'ed to trFilter
     * @param valid db connection to use
     * @throws DBConnectionException
     * @throws IllegalArgumentException if the lesser time in the time 
     *         range filter is less than the value returned by 
     *         <code>getBeginningOfProduction()</code>
     */
    /*
    MonitorDataQueryManager
        (const std::set<const carma::dbms::Selector*>& selectors,
         const carma::dbms::TimeRangeFilter * const trFilter,
         const carma::dbms::Filter * const otherFilter,
         const carma::dbms::DBConnection * const dbc);
    */
    /**
     * Create a monitor data query with only a time range filter and a set
     * of MonitorPointSelectors.  
     * @param selectors the set of monitor point selectors to use
     * @param trFilter time range filter which also contains information on 
     * what average type to search for
     * @param dbc valid db connection to use
     * @throws DBConnectionException
     * @throws IllegalArgumentException if the lesser time in the time 
     *         range filter is less than the value returned by 
     *         <code>getBeginningOfProduction()</code>
     */
    MonitorDataQueryManager
        (const MPSelectorSet& selectors,
         const carma::dbms::TimeRangeFilter * const trFilter,
         const carma::dbms::DBConnection * const dbc);

    /**
     * Create a monitor data query with a time range filter which is ANDed to
     * a set of MonitorPointFilters (the monitor point filters in the set are 
     * all ORed together) and a set of MonitorPointSelectors.  
     * The monitor point filters in @p mpfilters must all have unique tagIDs
     * @param selectors the set of monitor point selectors to use
     * @param mpFilters the set of monitor point filters to be used
     * @param trFilter time range filter which also contains information on 
     * what average type to search for
     * @param dbc valid db connection to use
     * @throws DBConnectionException
     * @throws IllegalArgumentException if the lesser time in the time 
     *         range filter is less than the value returned by 
     *         <code>getBeginningOfProduction()</code>
     */
    MonitorDataQueryManager
        (const MPSelectorSet& selectors,
         const carma::dbms::TimeRangeFilter * const trFilter,
         const std::set<const carma::dbms::MonitorPointFilter * >& mpfilters,
         const carma::dbms::DBConnection * const dbc);

    /**
     * get the aggregate data types used by this query
     * @return the aggregate data types used by the query
     */
    std::set<carma::dbms::MonitorAggregateType> getAggregateDataTypes() const;


    //bool shouldCheckArchive() const;

    /**
     * destructor deletes any "temporary" tables created for wide query
     */
    ~MonitorDataQueryManager();

    /**
     * get the frameCount that marks the beginning of production
     * @return get the frameCount that marks the beginning of production
     */
    static carma::util::frameType getBeginningOfProduction();

    /**
     * get the tables which this class will need to query to produce
     * the result set
     * @return the tables which will be queried
     */
    std::map<carma::dbms::MonitorAggregateType, std::vector<std::string> > 
        getTablesToQuery() const;

    /**
     * get the queries which should be run to set up the tables that will
     * be those on which the narrow SELECT queries are run, as well as the
     * first set of queries that will produce the final wide result set
     * @return the set up queries which should be run (in the order they 
     *         appear in the vector)
     */
    std::vector<std::string> getSetUpQueries() const { return setUpQueries_; }

    /**
     * get the queries which should be run to set up the tables that will
     * be those on which the final wide result set query is run. In order
     * to produce the wide result set from the original monitor data tables,
     * first the queries from <code>getSetUpQueries()</code> are run, then
     * the queries (if any) from <code>getWideSetUpQueries()</code> are run,
     * and finally, the query from <code>getWideRSQuery()</code> is run.
     */
    std::vector<std::string> getWideSetUpQueries() const { 
        return wideSetUpQueries_; 
    }

    /**
     * get the SELECT queries which will produce (generally) multiple narrow
     * (as in narrow table, no joins) result sets
     */
    std::map<int,std::string> getNarrowRSQueries() const {
        return narrowRSQueries_;
    }

    /**
     * get the final wide result set query
     * @return the wide result set query 
     */
    std::string getWideRSQuery() const { return wideRSQuery_; }

    /**
     * execute "narrow" queries.  These queries in general produce several
     * tables (1 table per tagID); no joins are done between tables 
     * representing different tagIDs
     * This method is not const because it modifies the bool switch to indicate
     * that the set up queries for this object have been run
     * @return the result sets, the returned map has tagIDs as keys and
     *         Tables (result sets) as values
     */
    std::map<int,carma::dbms::Table> execNarrowQueries();
    
    /**
     * execute "wide" query.  This query produces a single table which is
     * the result of joining all the necessary tables which are reflected
     * in the monitor point selectors and filters.
     * This method is not const because it modifies the bool switch to indicate
     * that the wide set up queries for this object have been run
     * @return the single result set which may contain very many columns
     */
    carma::dbms::Table execWideQuery();

    /**
     * Return the column name string for the given column type.
     * If column is COLUMN_VALUE, and avgType_ isn't FRAME_AVG,
     * return COLUMN_INTEGRATEDVALUE instead.
     */
    std::string mdqmGetColumnName(DBColumn column)const;
protected:
    const carma::dbms::DBConnection *dbc_;
    const carma::dbms::TimeRangeFilter *trFilter_;
    //const carma::dbms::Filter *otherFilter_;
    const std::set<const carma::dbms::MonitorPointFilter *> mpFilters_;
    MPSelectorSet selectors_;
    carma::dbms::MonitorAverageType avgType_;
    std::map<carma::dbms::MonitorAggregateType,std::string> tmpTableName_;


    /**
     * holds the monitor data tables relevant to this query
     */
    std::map<carma::dbms::MonitorAggregateType, carma::dbms::Table> 
        tablesToQuery_;

    std::map<carma::dbms::MonitorAggregateType, 
        std::vector<const carma::dbms::MonitorPointSelector * > >
        aggType2MPSelectors_;

    std::map<carma::dbms::MonitorAggregateType, 
        std::vector<const carma::dbms::MonitorPointFilter * > >
        aggType2MPFilters_;

    std::map<unsigned, const carma::dbms::MonitorPointFilter * > tagID2MPFilter_;

    /**
     * holds the filter clauses
     */
    std::map<carma::dbms::MonitorAggregateType, std::string> 
        aggType2FilterClause_;


    /**
     * the aggregate data types relevant to this query
     */
    std::set<MonitorAggregateType> aggTypes_;
    const static std::string TAGID_TO_DATATYPE;
    /**
     * this variable is only used if the ResultsCache singleton doesn't exist
     * and cannot be created
     */
    carma::dbms::Table tagIDToDataTypeID_;

    /**
     * the queries in the order they should be submitted to the database
     * which create and populate the tables which will actually be queried
     * to produce the requested results set(s)
     */
    std::vector<std::string> setUpQueries_;
    bool haveRunSetUpQueries_;

    /**
     * queries to execute to produce (generally) several "narrow" (as in
     * tables, no joins are done) result sets
     */
    std::map<int,std::string> narrowRSQueries_;

    /**
     * the queries in the order they should be submitted to the database
     * which create and populate temporary tables necessary for the single
     * "wide" query
     */
    std::vector<std::string> wideSetUpQueries_;
    bool haveRunWideSetUpQueries_;

    std::vector<std::string> tablesToDropUponDestruction_;


    /**
     * query to execute to produce a singe "wide" (generally using several
     * joins) table
     */
    std::string wideRSQuery_;

    /**
     * temporary table counter to better assure that created temp tables
     * will have unique names
     */
    static int tCount_;

    std::string wideErrMsg_;

    /**
     * determine which aggregate table types will need to be used in the query
     */
    void determineAggregateTableTypes_();

    /**
     * handle one component filters when determining which aggregate table
     * types need to be queried
     */
    /*
    void handleOneComponentFilter_
        (const carma::dbms::OneComponentFilter * const ocf);
    */
    /**
     * handle a TagIDFilter when determining which aggregate table types need
     * to be queried
     */
    //void handleTagIDFilter_(const carma::dbms::TagIDFilter * const tif);

    /**
     * handle a TagIDSetFilter when determining which aggregate table types 
     * need to be queried
     */
    //void handleTagIDSetFilter_(const carma::dbms::TagIDSetFilter * const tisf);

    /*
    void handleMultiComponentFilter_ 
        (const carma::dbms::MultiComponentFilter * const tcf);
    */

    /**
     * determine which monitor data tables contain info relevant for this query
     * this is essentially the set of tables which have data in the specified
     * time range for the specified average type and also contain data of (one
     * of) the aggregate types
     */
    void determineTablesToQuery_();

    /**
     * cache the tag id to data type mapping
     */
    
    void cacheTagIDToDataType_();
    
    /**
     * create the queries to submit to the database
     */
    void createQueries_();
    
    /**
     * do some sanity checking on values passed to the constructor
     */
    void doSanityChecks_();

    /**
     * execute the set up queries
     */
    void execSetUpQueries_() const;

    /**
     * execute the CREATE TABLE queries necessary to produce the tables
     * which will be joined in the wide query
     */
    void execWideSetUpQueries_() const;

    /**
     * create the set up queries, these are the queries which create the
     * temporary tables which are ultimately the targets of the select
     * queries which create the result sets which are accessible via the
     * public API
     */
    void createSetUpQueries_();

    /**
     * create the narrow result set queries, ie these are queries which
     * will produce info on a single tagID per result set
     */
    void createNarrowRSQueries_();

    /**
     * create the wide result set query, ie this is the query which
     * will create a single, wide table containing <code>n</code> columns where
     * <code>n</code> is the sum of all the columns specified in the
     * monitor point selectors which were passed to the constructor
     */
    void createWideRSQuery_();

    void createJoinSetUpQueries_
        (const std::vector
         < std::vector<const carma::dbms::MonitorPointSelector * > >& mpssVec);

    void init_();

    void constructFilterClauses_();
};
}}

#endif // CARMA_DBMS_MONITORDATAQUERYMANAGER_H

