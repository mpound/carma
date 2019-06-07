/**
 * Implementation for the LogRecordSelector class
 *
 * @author: Dave Mehringer
 * @version $Id: LogRecordSelector.cc,v 1.3 2011/12/21 22:56:43 mpound Exp $ *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include <vector>
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/LogRecordSelector.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;



LogRecordSelector::LogRecordSelector() 
{ 
    setDefaultSelection_();
}

LogRecordSelector::~LogRecordSelector() 
{ 
    // does nothing
}

void LogRecordSelector::setDefaultSelection_() 
{
    SelectionMask mask = nullSelectionMask();
    mask.caller        = true;
    mask.frameCount    = true;
    mask.priority      = true;
    mask.logname       = true;
    mask.ndc           = true;
    mask.message       = true;
    setSelectionMask(mask);
}

string LogRecordSelector::toString() const 
{
    return "";
}

void LogRecordSelector::setSelectionMask (const SelectionMask& mask) 
{ 
    mask_ = mask; 
}

LogRecordSelector::SelectionMask 
LogRecordSelector::getSelectionMask() const 
{ 
    return mask_; 
}

LogRecordSelector::SelectionMask 
LogRecordSelector::nullSelectionMask() const 
{
    SelectionMask mask;
    mask.caller             = false;
    mask.frameCount         = false;
    mask.priorityID         = false;
    mask.priority           = false;
    mask.logname            = false;
    mask.ndc                = false;
    mask.message            = false;
    return mask;
}


unsigned LogRecordSelector::getSupplementalTableJoinCount() const 
{
    int count = 0;
    if(mask_.priority) {
        count++;
    }
    return count;
}

unsigned LogRecordSelector::selectedColumnCount() const 
{
    return selectedColumns_().size();
}

vector<string> LogRecordSelector::selectedColumns_() const 
{
    vector<string> columns;
    if(mask_.caller) {
        columns.push_back(getColumnName(COLUMN_CALLER));
    }
    if(mask_.frameCount) {
        columns.push_back(getColumnName(COLUMN_FRAMECOUNT));
    }
    if(mask_.priorityID) {
        columns.push_back(getColumnName(COLUMN_PRIORITYID));
    }
    if(mask_.priority) {
        columns.push_back(getColumnName(COLUMN_PRIORITY));
    }
    if(mask_.logname) {
        columns.push_back(getColumnName(COLUMN_LOGNAME));
    }
    if(mask_.ndc) {
        columns.push_back(getColumnName(COLUMN_NDC));
    }
    if(mask_.message) {
        columns.push_back(getColumnName(COLUMN_MESSAGE));
    }
    return columns;
}
