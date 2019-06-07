#ifndef CARMA_DBMS_MONITORPOINTSELECTOR_H
#define CARMA_DBMS_MONITORPOINTSELECTOR_H

/**
 * @file
 * MonitorPointSelector class.
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorPointSelector.h,v 1.20 2011/12/21 22:56:43 mpound Exp $ *
 * $CarmaCopyright$
 *
 */
#include <string>
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/MonitorDescription.h"
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/Selector.h"

namespace carma {
namespace dbms {

    class DBConnection;

/**
 * Class to represent an SQL selection (portion of SELECT clause) based
 * on the attributes of a monitor point
 */
class MonitorPointSelector : public Selector {

public:

    //friend class MonitorDataQueryManager;

    /**
     * @struct SelectionMask
     * Holds info on which monitor point fields should
     * be selected (ie returned) by a query), if a value is set to true, 
     * the corresponding column is included in the selection (in cases where
     * the selection makes sense, see the description of individual fields), 
     * if false it is not.
     */
    typedef struct {
        /**
         * @var bool SelectionMask::time 
         * @brief If true, the time column (<code>frameCount</code> for  frame
         * and minute averages, <code>integrationID</code> for astronomical 
         * integrations) is selected.  Default setting is true.
         */
        bool time;
        /**
         * @var bool SelectionMask::tagID
         * @brief If true, the <code>tagID</code> column is selected.  
         * Default setting is false.
         */
        bool tagID;
        /**
         * @var bool SelectionMask::value
         * @brief If true, the <code>value</code> column is selected. In the 
         * case of complex values both parts of the value (the 
         * <code>realpart</code> and <code>imagpart</code> columns) are 
         * selected.  Default setting is true.
         */
        bool value;
        /**
         * @var bool SelectionMask::blankingFlagID
         * if true, the <code>blankingFlagID</code> column is selected.
         * These values in general are not of much use to end users.
         * Default setting is false.
         */
        bool blankingFlagID;
        /**
         * @var bool SelectionMask::blankingFlag
         * @brief If true, the <code>blankingFlag</code> column is from the 
         * <code>BlankingFlags</code> table  is selected. This is the string 
         * value corresponding to the <code>blankingFlagID </code> value. A 
         * join will have to be done to extract this value. These values are 
         * generally more useful to end users than <code>blankingFlagID</code>
         * values. Default setting is true.
         */
        bool blankingFlag;
        /**
         * @var bool SelectionMask::validityID
         * @brief If true, the <code>validityID</code> column is selected.
         * These values in general are not of much use to end users.
         * Default setting is false.
         */
        bool validityID;
        /**
         * @var bool SelectionMask::validity
         * @brief If true, the <code>validity</code> column from
         * the <code>Validities</code> table  is selected. This
         * is the string value corresponding to the <code>validityID
         * </code> value. A join will have to be done to extract this
         * value. These values are generally more useful to end users
         * than <code>validityID</code> values.
         * Default setting is true.
         */
        bool validity;
        /**
         * @var bool SelectionMask::iSample
         * @brief If true, the <code>iSample</code> column is selected. In the
         * case of string monitor points, which have no iSample values,
         * setting this field is prohibited.
         * Default setting is true except in the case where the monitor point
         *         is of data type string
         */
        bool iSample;
        /**
         * @var bool SelectionMask::enumString
         * @brief If true, the corresponding enumerator string column is 
         * selected if the monitor point has data type enumeration. In the
         * case where the monitor point is not of type enumeration, setting
         * this field to true is prohibited.
         * Default setting is true if the monitor point is of type enumeration.
         */
        bool enumString;
        /**
         * @var bool SelectionMask::maxValue
         * @brief If true, the <code>maxValue</code> column is selected. In the
         * case of frame averages and/or string monitor points, 
         * which have no <code>maxValue</code> values, setting this 
         * field to true is prohibited. In the case of complex monitor points, 
         * setting this field to true will cause both parts of the
         * associated value (the <code>maxRealPart</code> and <code>
         * maxImagPart</code> columns) to be selected.
         * Default setting is false.
         */
        bool maxValue;
        /**
         * @var bool SelectionMask::minValue
         * @brief If true, the <code>minValue</code> column is selected. In the
         * case of frame averages and/or string monitor points, 
         * which have no <code>minValue</code> values, setting this 
         * field to true is prohibited. In the case of complex monitor points, 
         * setting this field to true will cause both parts of the
         * associated value (the <code>minRealPart</code> and <code>
         * minImagPart</code> columns) to be selected.
         * Default setting is false.
         */
        bool minValue;
    } SelectionMask;

    /**
     * constructor
     * the object is intialized with all the members of its selection mask
     * set to false, with the exception of <code>.time</code> and 
     * <code>.value</code> which are set to true
     * @param tagID the tagID of the monitor point to select
     * @param dbc a valid database connection, needed to get the data type
     *        of the monitor point with the specified tagID
     */
    MonitorPointSelector(const unsigned& tagID, 
                         const carma::dbms::DBConnection* const dbc);

    /**
     * constructor
     * the object is intialized with all the members of its selection mask
     * set to false, with the exception of <code>.time</code> and 
     * @param tagID the tagID of the monitor point to select
     * @param dataType the dataType of the point with the specified tagID.
     *        It better be correct! if unsure, use the alternate constructor
     *        which will get the datatype from the database
     */
    MonitorPointSelector(const unsigned& tagID, 
                         carma::dbms::MonitorPointDataType dataType);

    /**
     * destructor
     */
    ~MonitorPointSelector();

    /**
     * get the tagID of the monitor point represented by this selection
     */
    unsigned getTagID() const { return tagID_; }

    /**
     * set the selection mask
     * @param mask the selection mask to use

     * @param quietly If true, don't throw an exception for inappropriate
     * requests. (Quietly override them).
     * @throws carma::util::IllegalArgumentException if certain fields of the
     *         @p mask are true and this makes no sense based on the monitor
     *         point's data type (see the documentation regarding
     *         carma::dbms::MonitorPointSelection::SelectionMask for 
     *         details)
     */
    void setSelectionMask(const SelectionMask& mask, bool quietly=false);

    /**
     * set the selection mask using a string.
     * @param maskString String containing "|" separated field names.
     *	      A field name may be any of
     *	      time|tagID|value|blankingFlagID|blankingFlag|validityID|
     *	      validity|maxValue|minValue|all|none
     *        All means all fields. None means remove all fields.
     *	      Individual field names may be prepended (with no intervening
     *        space) by a "-" to indicate that field is to be removed from
     *        the list. Entries are processed left to right.
     * @throws carma::util::IllegalArgumentException if certain fields of the
     *         @p mask are true and this makes no sense based on the monitor
     *         point's data type (see the documentation regarding
     *         carma::dbms::MonitorPointSelection::SelectionMask for 
     *         details)
     */

    void setSelectionMask(const std::string &maskString);

    /**
     * get the selection mask
     */
    SelectionMask getSelectionMask() const;

    carma::dbms::MonitorDescription getMonitorConfiguration() const;

    /**
     * string representation of this selection
     */
    std::string toString() const;

    /**
     * SQL string representation of the selection. 
     * @param avgType the average type
     * @param table optional table name from where the selection is to
     *        occur. If not "", prepends @p table + "." to the column name
     * @param columnAliasPrepender if not "", results in " AS " + @p 
     *        columnAliasPrepender + "." column name being
     *        appended to the column selection
     * @param blankingFlagTableAlias alias to use for the blanking flag table
     *        only used if a blanking flag column selection is set
     * @param validityTableAlias alias to use for the validity table
     *        only used if a validity column selection is set
     * @param enumIndexTableAlias alias to use for the enumerator index table
     *        only used if the selection mask's enumString field is true and
     *        if the data type is enumeration
     * @param onlyAliases return only the column aliases as a comma delimited
     *        list
     */
    std::string toString
        (const carma::dbms::MonitorAverageType& avgType, 
         const std::string& table="", 
         const std::string& columnAliasPrepender="",
         const std::string& blankingFlagTableAlias="",
         const std::string& validityTableAlias="",
         const std::string& enumIndexTableAlias="", 
         const bool& onlyAliases=false) 
        const;


    /**
     * set the column prefix for the fields returned in the result set
     * for example if the mask is set to select only the <code>value</code>,
     * the column name containing these values will be "joe.value" if
     * this method is called with <code>columnPrefix=joe</code>
     * @param columnPrefix the string to prefix column names with
     */
    void setColumnPrefix(const std::string& columnPrefix) {
        columnPrefix_ = columnPrefix;
    }

    std::string getColumnPrefix() const { return columnPrefix_; }

    /**
     * should the enumerator string column be selected based on the selection
     * mask and on the datatype?
     * @return if the enumerator string column should be selected
     */
    bool doSelectEnumString() const;

    /**
     * returns the number of supplemental tables which will need to be
     * joined to the main monitor table(s) to produce the desired selection
     * <il>
     * <ul>mask.blankingFlag == true =&gt; +1
     * <ul>mask.validity == true =&gt; +1
     * <ul>mask.enumString == true && datatype == enumeration =&gt; +2
     */
    unsigned getSupplementalTableJoinCount() const;

    /**
     * get the number of columns which will be selected based on the selection
     * mask, monitor point data type, and average type
     * @return the number of columns which will be selected
     */
    unsigned selectedColumnCount(carma::dbms::MonitorAverageType& avgType) const;

    /**
     * get the data type of the monitor point associated with this selector
     * @return the data type
     */
    MonitorPointDataType getDataType() const { return dataType_; }

    /**
     * convenience method for setting the SelectionMask to permit selecting
     * only the tagID column
     */
    void selectOnlyTagID();

    /**
     * get a selection mask with all fields set to false
     * @return a selection mask with all fields set to false
     */
    SelectionMask nullSelectionMask() const;

    /**
     * get a selection mask with all fields set to true.
     * @return a selection mask with all fields set to true
     */
    SelectionMask allSelectionMask() const;
 
protected:
    unsigned tagID_;
    // make this a pointer since the default MonitorDescription constructor 
    // is protected
    MonitorDescription *md_;
    SelectionMask mask_;
    //carma::dbms::MonitorAverageType avgType_;
    carma::dbms::MonitorPointDataType dataType_;
    std::string columnPrefix_;

    /**
     * set the default selection
     */
    void setDefaultSelection_();

    static std::string getModifiedColumnSelection_
        (const carma::dbms::DBColumn& columnName, const std::string& table, 
         const std::string& alias, const bool& onlyAlias);

    /**
     * get the column selections as a a vector for making counting, etc
     * easier
     */
    std::vector<std::string> selectedColumns_
        (const carma::dbms::MonitorAverageType& avgType, 
         const std::string& table="", 
         const std::string& columnAliasPrepender="",
         const std::string& blankingFlagTableAlias="",
         const std::string& validityTableAlias="", 
         const std::string& enumIndexTableAlias="", 
         const bool& onlyAliases=false) const;
};

/**
  * Class for sorting a set of MPSelector pointers.
  * Needed to make a set of MPSelector pointers sort correctly.
  */
class MPSelectorCompare
{
    public:
    bool operator()
        (const MonitorPointSelector* lhs, const MonitorPointSelector* rhs) ; 
};

/// A set of MonitorPointSelector points ordered by tagID
typedef std::set<MonitorPointSelector*, MPSelectorCompare> MPSelectorSet;

}}

#endif // CARMA_DBMS_MONITORPOINTSELECTOR_H


