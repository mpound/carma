#ifndef CARMA_DBMS_LOGRECORDSELECTOR_H
#define CARMA_DBMS_LOGRECORDSELECTOR_H

/**
 * @file
 * LogRecordSelector class.
 *
 * @author: Dave Mehringer
 * @version $Id: LogRecordSelector.h,v 1.4 2011/12/21 22:56:43 mpound Exp $ *
 * $CarmaCopyright$
 *
 */
#include <string>
#include <vector>
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/Selector.h"

namespace carma {
namespace dbms {

/**
 * Class to represent an SQL selection (portion of SELECT clause) for a log
 * message
 */
class LogRecordSelector : public Selector {

public:

    /**
     * @struct SelectionMask
     * Holds info on which log record fields should
     * be selected (ie returned) by a query), if a value is set to true, 
     * the corresponding field is included in the selection (in cases where
     * if false it is not.
     */
    typedef struct {
        /**
         * @var bool SelectionMask::frameCount
         * @brief If true, the frameCount field is selected
         * Default setting is true.
         */
        bool frameCount;
        /**
         * @var bool SelectionMask:caller
         * @brief If true, the <code>caller</code> column is selected.  
         * Default setting is true.
         */
        bool caller;
        /**
         * @var bool SelectionMask::priorityID
         * @brief If true, the <code>priorityID</code> column is selected.
         * Default setting is true.
         */
        bool priorityID;
        /**
         * @var bool SelectionMask::priority
         * @brief If true, the <code>priority</code> column from the 
         * <code>LogPriorities</code> table  is selected. This is the string 
         * value corresponding to the <code>priorityID </code> value. A 
         * join will have to be done to extract this value. These values are 
         * generally more useful to end users than <code>priorityID</code>
         * values. Default setting is true.
         */
        bool priority;
        /**
         * @var bool SelectionMask::logname
         * @brief If true, the <code>logname</code> column is selected.
         * Default setting is true.
         */
        bool logname;
        /**
         * @var bool SelectionMask::ndc
         * @brief If true, the <code>ndc</code> column is selected.
         * Default setting is true.
         */
        bool ndc;
        /**
         * @var bool SelectionMask::message
         * @brief If true, the <code>message</code> column is selected.
         * Default setting is true.
         */
        bool message;
    } SelectionMask;

    /**
     * constructor
     * the object is intialized with all the members of its selection mask
     * set to true, except for priorityID
     */
    LogRecordSelector();

    /**
     * destructor
     */
    virtual ~LogRecordSelector();

    /**
     * set the selection mask
     * @param mask the selection mask to use
     */
    void setSelectionMask(const SelectionMask& mask);

    /**
     * get the selection mask
     */
    SelectionMask getSelectionMask() const;

    /**
     * Set the mask value for the Caller column of the selection.
     * @param value The value to set
     */
    void setCallerMask(bool value) 
    {
	mask_.caller = value;
    }

    /**
     * Set the mask value for the Framecount column of the selection.
     * @param value The value to set
     */
    void setFrameCountMask(bool value)
    {
	mask_.frameCount = value;
    }

    /**
     * Set the mask value for the Logname column of the selection.
     * @param value The value to set
     */
    void setLognameMask(bool value)
    {
	mask_.logname = value;
    }

    /**
     * Set the mask value for the Message column of the selection.
     * @param value The value to set
     */
    void setMessageMask(bool value)
    {
	mask_.message = value;
    }

    /**
     * Set the mask value for the Priority column of the selection.
     * @param value The value to set
     */
    void setPriorityMask(bool value)
    {
	mask_.priority = value;
    }

    /**
     * Set the mask value for the PriorityID column of the selection.
     * @param value The value to set
     */
    void setPriorityIdMask(bool value)
    {
	mask_.priority = value;
    }


    /**
     * Set the mask value for the ndc column of the selection.
     * @param value The value to set
     */
    void setNdcMask(bool value)
    {
	mask_.ndc = value;
    }

    /**
     * string representation of this selection
     */
    std::string toString() const;

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
    unsigned selectedColumnCount() const;

    /**
     * get a selection mask with all fields set to false
     * @return a selection mask with all fields set to false
     */
    SelectionMask nullSelectionMask() const;

protected:

    SelectionMask mask_;

    /**
     * set the default selection
     */
    void setDefaultSelection_();


    std::vector<std::string> selectedColumns_() const;

};
}}

#endif // CARMA_DBMS_LOGRECORDSELECTOR_H

