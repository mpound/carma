/**
 * Implementation for the MonitorPointFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: MonitorPointFilter.cc,v 1.10 2005/08/30 14:27:09 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include <vector>
#include "carma/dbms/filter/MonitorPointFilter.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms//MonitorSystemAndDBMSRelationships.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/dbms/filter/BlankingFlagSetFilter.h"
#include "carma/dbms/filter/MonitorPointNumericValueFilter.h"
#include "carma/dbms/filter/MonitorPointNumericMaxValueFilter.h"
#include "carma/dbms/filter/MonitorPointNumericMinValueFilter.h"
#include "carma/dbms/filter/ValiditySetFilter.h"
#include "carma/dbms/filter/StringFilter.h"
#include "carma/dbms/filter/TagIDFilter.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

map<int,MonitorPointDataType> MonitorPointFilter::tagIDToDataType_;


MonitorPointFilter::MonitorPointFilter(
    const uint &               tagID,
    const MonitorAverageType & averageType,
    const Filter * const       filter,
    const DBConnection * const dbc ) :
MultiComponentFilter(AND),
tagID_(tagID),
dbc_(dbc),
averageType_(averageType),
dataTypeHasBeenDetermined_(false) {
    child2_ = filter;
    init_();
}


MonitorPointFilter::MonitorPointFilter(
    const int &                  tagID,
    const MonitorAverageType &   averageType,
    const Filter * const         filter,
    const MonitorPointDataType & dataType ) :
MultiComponentFilter(AND),
tagID_(tagID),
dbc_(NULL),
averageType_(averageType),
dataType_(dataType),
dataTypeHasBeenDetermined_(true) {
    child2_ = filter;
    init_();
}


MonitorPointFilter::~MonitorPointFilter() {
    // child2_ was created outside this object, so don't delete it, but
    // child1_ was created internally, so it needs to be deleted
    delete child1_;
}

/*
string MonitorPointFilter::toString() const {
    return "";
}
*/

MonitorAverageType MonitorPointFilter::getAverageType() const {
    return averageType_;
}

string MonitorPointFilter::name() const {
    return "MonitorPointFilter";
}

MonitorPointDataType MonitorPointFilter::getDataType() const {
    return dataType_;
}

uint MonitorPointFilter::getTagID() const {
    return tagID_;
}

MonitorPointDataType MonitorPointFilter::determineDataType_() {
    if(!dataTypeHasBeenDetermined_) {
        initializeTagIDToDataTypeIfNecessary_();
        map<int, MonitorPointDataType>::const_iterator iter 
            = tagIDToDataType_.find(tagID_);
        if(iter == tagIDToDataType_.end()) {
            ostringstream emsg;
            emsg << "tagID " << tagID_ << " does not exist in the database";
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                  emsg.str());
        }
        dataType_ = iter->second;
        dataTypeHasBeenDetermined_ = true;
    }
    return dataType_;
}


void MonitorPointFilter::validateFilter_() {
    ostringstream emsg;
    if (child2_ == NULL ) {
        emsg << "Input filter cannot be NULL. If you really only want to "
             << "filter on tagID, use a tagID filter";
        throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                              emsg.str());
    }
    const OneComponentFilter *ocf = dynamic_cast<const OneComponentFilter * >
        (child2_);
    vector<const OneComponentFilter * > ocfs;
    if(ocf != NULL) {
        ocfs.push_back(ocf);
    } else {
        const MultiComponentFilter *mcf 
            = dynamic_cast<const MultiComponentFilter * >(child2_);
        if (mcf == NULL) {
            emsg << "Passed-in filter is neither a OneComponentFilter nor a "
                 << "MultiComponentFilter, something is amiss";
            throw CARMA_ERROR(emsg.str());
        }
        mcf->getOneComponentFilterDescendants(&ocfs);
    }
    vector<const OneComponentFilter * >::const_iterator iter = ocfs.begin();
    bool hasBlankingFlagSetFilter = false;
    bool hasValiditySetFilter = false;
    for( ; iter != ocfs.end(); iter++) {
        if(dynamic_cast<const BlankingFlagSetFilter * >(*iter) != NULL) {
            if(hasBlankingFlagSetFilter) {
                emsg << "Filter passed to constructor has more than one "
                     << "BlankingFlagSetFilters, a maximum of one is "
                     << "permitted";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
            hasBlankingFlagSetFilter = true;
        } else if(dynamic_cast<const ValiditySetFilter * >(*iter) != NULL) {
            if(hasValiditySetFilter) {
                emsg << "Filter passed to constructor has more than one "
                     << "ValiditySetFilters, a maximum of one is permitted";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
            hasValiditySetFilter = true;
        } else if
            (dynamic_cast<const MonitorPointNumericValueFilter<int> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericValueFilter<uint> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericValueFilter<float> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericValueFilter<short> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericValueFilter<ushort> * >
             (*iter) != NULL) {
                if(dataType_ == DATATYPE_STRING) {
                emsg << "Monitor point with tagID " << tagID_ << " is a "
                     << "string but a MonitorPointNumericValueFilter was "
                     << "included in the filter passed to the constructor."
                     << "This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
                }
        } else if
            (dynamic_cast<const MonitorPointNumericMaxValueFilter<int> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<uint> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<float> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<short> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMaxValueFilter<ushort> * >
             (*iter) != NULL) {
            if(dataType_ == DATATYPE_STRING) {
                emsg << "Monitor point with tagID " << tagID_ << " is a "
                     << "string but a MonitorPointNumericMaxValueFilter was "
                     << "included in the filter passed to the constructor."
                     << "This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
            if(averageType_ == FRAME_AVG) {
                emsg << "The average type passed to the constructor was "
                     << "FRAME_AVG, but a MonitorPointNumericMaxValueFilter "
                     << "was also included in the filter passed to the "
                     << "constructor. This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
        } else if
            (dynamic_cast<const MonitorPointNumericMinValueFilter<int> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericMinValueFilter<uint> * >
            (*iter) != NULL
            || dynamic_cast<const MonitorPointNumericMinValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMinValueFilter<double> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMinValueFilter<float> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMinValueFilter<short> * >
            (*iter) != NULL 
            || dynamic_cast<const MonitorPointNumericMinValueFilter<ushort> * >
             (*iter) != NULL) {
            if(dataType_ == DATATYPE_STRING) {
                emsg << "Monitor point with tagID " << tagID_ << " is a "
                     << "string but a MonitorPointNumericMinValueFilter was "
                     << "included in the filter passed to the constructor."
                     << "This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
            if(averageType_ == FRAME_AVG) {
                emsg << "The average type passed to the constructor was "
                     << "FRAME_AVG, but a MonitorPointNumericMinValueFilter "
                     << "was also included in the filter passed to the "
                     << "constructor. This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                emsg.str());
            }
        } else if
            (dynamic_cast<const StringFilter * > (*iter) != NULL) {
            if(dataType_ != DATATYPE_STRING) {
                emsg << "Monitor point with tagID " << tagID_ << " is a not "
                     << "string but a StringFilter was "
                     << "included in the filter passed to the constructor."
                     << "This makes no sense";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                      emsg.str());
            }
        } else {
            emsg << "Filter of type " << (*iter)->name() << " is not "
                 << "permitted in filter passed to constructor";
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, 
                                  emsg.str());
        }            
    }        
}    


void MonitorPointFilter::initializeTagIDToDataTypeIfNecessary_() const {
    if(tagIDToDataType_.size() == 0) {
        if(ResultsCache::cacheExists()) {
            const Table &t 
                = ResultsCache::getCache()
                .getCurrentMonitorConfigurationTable();
            makeTagID2DataTypeMap_(t);
        } else {
            if(dbc_ == NULL) {
                ostringstream emsg;
                emsg << "The database must be contacted to determine the "
                     << "data type of the monitor point with tagID=" << tagID_
                     << ". Therefore, the DBConnection pointer cannot be NULL";
                throw CARMA_EXCEPTION(carma::util::IllegalArgumentException,
                                      emsg.str());
            }
            MonitorConfigurationDatabase mcdb(dbc_);
            makeTagID2DataTypeMap_(mcdb.getTagIDToDataTypeIDTable());
        }
    }
}

void MonitorPointFilter::makeTagID2DataTypeMap_
    (const carma::dbms::Table& table) const {
    Column<short> dataTypeID 
        = table.getShortColumn(getColumnName(COLUMN_DATATYPEID));
    Column<short>::const_iterator iter = dataTypeID.begin();
    Column<MonitorPointDataType> dataType("Data Type");
    for( ; iter != dataTypeID.end(); iter++) {
        dataType.push_back(db2mpDataType(*iter));
    }
    tagIDToDataType_ = columnsToMap
        (table.getIntColumn(getColumnName(COLUMN_TAGID)), dataType);
}

void MonitorPointFilter::init_() {
    determineDataType_();
    assert(dataTypeHasBeenDetermined_);
    validateFilter_();
    child1_ = new TagIDFilter(tagID_);
}        
