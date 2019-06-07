/**
 * Implementation for the MonitorPointSelector class
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorPointSelector.cc,v 1.20 2011/12/21 22:56:43 mpound Exp $ *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include <vector>
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "MonitorPointSelector.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;



MonitorPointSelector::MonitorPointSelector(
    const unsigned &               tagID,
    const DBConnection * const dbc ) :
Selector(),
tagID_(tagID),
md_(0) {
    // add call to tagIDExists()
    const Table &fullMonitorConfigTable 
        = (ResultsCache::getCache()).getFullMonitorConfigurationTable();
    int index = fullMonitorConfigTable.getIntColumn("tagID").indexOf(tagID);
    if(index == -1) {
        ostringstream emsg;
        emsg << "tagID " << tagID << " does not exist in the monitor "
             << "configuration database";
        throw CARMA_EXCEPTION(carma::util::NotFoundException,emsg.str());
    }
    
    MonitorConfigurationDatabase mcdb(dbc);
    md_ = new MonitorDescription
        (mcdb.tableRow2MonitorDescription(fullMonitorConfigTable,index));
    dataType_ = md_->getDataType();
    setDefaultSelection_();
    columnPrefix_ = md_->getName();
}

MonitorPointSelector::MonitorPointSelector
    (const unsigned& tagID, MonitorPointDataType dataType) 
        : tagID_(tagID), dataType_(dataType)
{
    setDefaultSelection_();
    ostringstream ss;
    ss << tagID;
    columnPrefix_ = ss.str(); 
}

MonitorPointSelector::~MonitorPointSelector() {
    // delete internally created pointers
    delete md_;
}

void MonitorPointSelector::setDefaultSelection_() {
    SelectionMask mask = nullSelectionMask();
    mask.time          = true;
    mask.value         = true;
    mask.blankingFlag  = true;
    mask.validity      = true;
    mask.iSample       = (dataType_ != DATATYPE_STRING);
    mask.enumString    = (dataType_ == DATATYPE_ENUMERATION);
    setSelectionMask(mask);
}

string MonitorPointSelector::toString() const {
    return "";
}


std::string MonitorPointSelector::toString
    (const MonitorAverageType& avgType, const string& table, 
     const string& columnAliasPrepender, const string& blankingFlagTableAlias,
     const string& validityTableAlias, const string& enumIndexTableAlias,
     const bool& onlyAliases) const {
    return carma::util::vectorToString
        (selectedColumns_
         (avgType, table, columnAliasPrepender, blankingFlagTableAlias,
          validityTableAlias,enumIndexTableAlias,onlyAliases));
}


/*
void MonitorPointSelector::setAverageType(const MonitorAverageType& avgType) {
    avgType_ = avgType;
}
    
MonitorAverageType MonitorPointSelector::getAverageType() const {
    return avgType_;
}
*/

void MonitorPointSelector::setSelectionMask
    (const MonitorPointSelector::SelectionMask& mask, bool quietly) { 
    MonitorPointSelector::SelectionMask tmpmask = mask;
    if(tmpmask.iSample && dataType_ == DATATYPE_STRING) {
      if(!quietly)
      { string emsg = "The iSample field is true, however, the data type of ";
        emsg += "monitor point " + md_->getName() + " is string.  String ";
        emsg += "monitor points have no iSamples. The selection mask is ";
        emsg += "unchanged";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
      }
      else
	tmpmask.iSample = false;
    }
    if(tmpmask.enumString && dataType_ !=  DATATYPE_ENUMERATION) {
      if(!quietly)
      { string emsg = "The enumString field is true, however, the data type ";
        emsg += "of monitor point " + md_->getName() + " is not enumeration. ";
        emsg += "The enumString field may only be true for points of type ";
        emsg += "enumeration. The selection mask is unchanged.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
      }
      else
	tmpmask.enumString = false;
    }     
    if(tmpmask.maxValue && dataType_ == DATATYPE_STRING) {
      if(!quietly)
      { string emsg = "The maxValue field is true, however, the data ";
        emsg += "type of monitor point " + md_->getName() + " is string. ";
        emsg += "String monitor points have no max values ";
        emsg += "The selection mask is unchanged.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
      }
      else
	tmpmask.maxValue = false;
    } 
    if(tmpmask.minValue && dataType_ == DATATYPE_STRING) {
      if(!quietly)
      { string emsg = "The minValue field is true, however, the data ";
        emsg += "type of monitor point " + md_->getName() + " is string. ";
        emsg += "String monitor points have no min values ";
        emsg += "The selection mask is unchanged.";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
      }
      else
	tmpmask.minValue = false;
    } 
    mask_ = tmpmask;
}

void MonitorPointSelector::setSelectionMask(const string &maskString)
{  // Break the input string into a list of tokens.
  vector<string> tokens = carma::util::StringUtils::tokenize( maskString, "| ",
							      true);
  SelectionMask mask = getSelectionMask();
  bool doQuietly = false;
  // Allow a space to be a separator. The true effectively allows both
  // at the same time.
  for(vector<string>::const_iterator iter=tokens.begin(); iter!=tokens.end();
      iter++)
  { const string token = *iter;
    bool isPlus=true;
    string::size_type pos = token.rfind("+");
    if(pos == string::npos)
    {  pos = token.rfind("-");
       if(pos != string::npos)
	 isPlus = false;
    }
    string selection1 = (pos == string::npos) ?
      token : token.substr(pos+1, string::npos);
    string selection =
      util::StringUtils::lowASCIIAlphaNumericToLower(selection1);

    if((selection == "all") || (selection == "*"))
    {   mask = isPlus ? allSelectionMask() : nullSelectionMask();
        doQuietly = true;
    }
    else if(selection == "none")
      mask = nullSelectionMask();
    else if(selection == "most")
    { mask = allSelectionMask();
      mask.blankingFlagID = false;
      mask.validityID = false;
      doQuietly = true;
    }
    else if(selection == "time")
      mask.time = isPlus;
    else if(selection == "tagid")
      mask.tagID = isPlus;
    else if(selection == "value")
      mask.value = isPlus;
    else if(selection == "blankingflagid")
      mask.blankingFlagID = isPlus;
    else if(selection == "blankingflag")
      mask.blankingFlag = isPlus;
    else if(selection == "validity")
      mask.validity = isPlus;
    else if(selection == "isample")
      mask.iSample = isPlus;
    else if(selection == "enumstring")
      mask.enumString = isPlus;
    else if(selection == "maxvalue")
      mask.maxValue = isPlus;
    else if(selection == "minvalue")
      mask.minValue = isPlus;
    else
      { string emsg = selection + " is not a valid selection mask field. ";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, emsg);
      }
  }
  setSelectionMask(mask, doQuietly);
}

MonitorPointSelector::SelectionMask MonitorPointSelector::getSelectionMask() 
    const { 
    return mask_; 
}

MonitorDescription MonitorPointSelector::getMonitorConfiguration() const {
    return *md_;
}

void MonitorPointSelector::selectOnlyTagID() {
    SelectionMask mask = nullSelectionMask();
    mask.tagID = true;
    setSelectionMask(mask);
}

MonitorPointSelector::SelectionMask MonitorPointSelector::nullSelectionMask()
    const {
    SelectionMask mask;
    mask.time               = false;
    mask.tagID              = false;
    mask.value              = false;
    mask.blankingFlag       = false;
    mask.blankingFlagID     = false;
    mask.validity           = false;
    mask.validityID         = false;
    mask.iSample            = false;
    mask.enumString         = false;
    mask.maxValue           = false;
    mask.minValue           = false;
    return mask;
}

MonitorPointSelector::SelectionMask MonitorPointSelector::allSelectionMask()
    const {
    SelectionMask mask;
    mask.time               = true;
    mask.tagID              = true;
    mask.value              = true;
    mask.blankingFlag       = true;
    mask.blankingFlagID     = true;
    mask.validity           = true;
    mask.validityID         = true;
    mask.iSample            = true;
    mask.enumString         = true;
    mask.maxValue           = true;
    mask.minValue           = true;
    return mask;
}

string MonitorPointSelector::getModifiedColumnSelection_
    (const DBColumn& columnName ,const string& table, 
     const string& columnAliasPrepender, const bool& onlyAlias) {
    string cname = getColumnName(columnName);
    string alias = "";
    if(columnAliasPrepender != "") {
        alias = "`" + columnAliasPrepender + "." + cname + "`";
    }
    if(onlyAlias) {
        return alias;
    }
    string mcs = "";
    if(table != "") {
        mcs = table + ".";
    }
    mcs += cname;
    if(alias != "") {
        mcs += " AS " + alias;
    }
    return mcs;
}

bool MonitorPointSelector::doSelectEnumString() const {
    return mask_.enumString;
}

unsigned MonitorPointSelector::getSupplementalTableJoinCount() const {
    int count = 0;
    if(mask_.validity) {
        count++;
    }
    if(mask_.blankingFlag) {
        count++;
    }
    if(mask_.enumString) {
        count += 2;
    }
    return count;
}

unsigned MonitorPointSelector::selectedColumnCount
    (MonitorAverageType& avgType) const {
    return selectedColumns_(avgType).size();
}

vector<string> MonitorPointSelector::selectedColumns_
    (const MonitorAverageType& avgType, const string& table, 
     const string& columnAliasPrepender, const string& blankingFlagTableAlias,
     const string& validityTableAlias, const string& enumIndexTableAlias,
     const bool& onlyAliases) 
    const {
    vector<string> columns;
    if(mask_.time) {
        if(avgType == FRAME_AVG || avgType == MINUTE_AVG) {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_FRAMECOUNT,table,columnAliasPrepender,
                               onlyAliases));
        } else {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_INTEGRATIONID,table,
                               columnAliasPrepender,onlyAliases));
        }
    }
    if(mask_.tagID) {
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_TAGID,table,columnAliasPrepender,
                           onlyAliases));
    }
    if(mask_.value) {
        if(avgType == FRAME_AVG) {
            if(dataType_ == DATATYPE_COMPLEX) {
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_REALPART,
                                   table,columnAliasPrepender,onlyAliases));
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_IMAGPART,table,
                                   columnAliasPrepender,onlyAliases));
            } else {
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_VALUE,table,columnAliasPrepender,
                                   onlyAliases));
            }
        } else {
            if(dataType_ == DATATYPE_COMPLEX) {
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_INTEGRATEDREALPART,table,
                                   columnAliasPrepender,onlyAliases));
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_INTEGRATEDIMAGPART,table,
                                   columnAliasPrepender,onlyAliases));
            } else {
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_INTEGRATEDVALUE,table,
                                   columnAliasPrepender,onlyAliases));
            }
        }
    }
    if(mask_.blankingFlag) {
        string tableName = (blankingFlagTableAlias == "") 
            ? getTableName(BLANKING_FLAGS_TABLE) : blankingFlagTableAlias;
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_BLANKINGFLAG, tableName,
                           columnAliasPrepender,onlyAliases));
    }
    if(mask_.blankingFlagID) {
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_BLANKINGFLAGID,table,columnAliasPrepender,
                           onlyAliases));
    }
    if(mask_.validity) {
        string tableName = (validityTableAlias == "") 
            ? getTableName(VALIDITIES_TABLE) : validityTableAlias;
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_VALIDITY, tableName, columnAliasPrepender,
                           onlyAliases));
    }
    if(mask_.validityID) {
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_VALIDITYID,table,columnAliasPrepender,
                           onlyAliases));
    }
    if(mask_.iSample && dataType_ != DATATYPE_STRING) {
                columns.push_back(getModifiedColumnSelection_
                                  (COLUMN_ISAMPLE,table,columnAliasPrepender,
                                   onlyAliases));
    }        
    if(mask_.enumString) {
        string tableName = (enumIndexTableAlias == "") 
            ? getTableName(MONITOR_ENUM_INDEX_TABLE) : enumIndexTableAlias;
        columns.push_back(getModifiedColumnSelection_
                          (COLUMN_ENUMVALUE, 
                           tableName, columnAliasPrepender,onlyAliases));
    }
    if(mask_.maxValue && avgType != FRAME_AVG) {
        if(dataType_ == DATATYPE_COMPLEX) {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MAXREALPART,table,columnAliasPrepender,
                               onlyAliases));
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MAXIMAGPART,table,columnAliasPrepender,
                               onlyAliases));
        } else {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MAX,table,columnAliasPrepender,
                               onlyAliases));
        }
    }
    if(mask_.minValue && avgType != FRAME_AVG) {
        if(dataType_ == DATATYPE_COMPLEX) {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MINREALPART,table,columnAliasPrepender,
                               onlyAliases));
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MINIMAGPART,table,columnAliasPrepender,
                               onlyAliases));
        } else {
            columns.push_back(getModifiedColumnSelection_
                              (COLUMN_MIN,table,columnAliasPrepender,
                               onlyAliases));
        }
    }
    return columns;
}

bool MPSelectorCompare::operator()
    (const MonitorPointSelector* lhs, const MonitorPointSelector* rhs) 
{ 
    return lhs->getTagID() < rhs->getTagID();
}
