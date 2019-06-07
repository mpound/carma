/**
 * Implementation for the MonitorPointFilter class
 *
 * @author Original: Dave Mehringer
 * @version $Id: LogMessageFilter.cc,v 1.1 2005/02/10 01:25:05 dmehring Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include <vector>
#include "carma/dbms/filter/LogMessageFilter.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/filter/Caller.h"
#include "carma/dbms/filter/Message.h"
#include "carma/dbms/filter/PriorityFilter.h"
#include "carma/dbms/filter/PrioritySetFilter.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;

LogMessageFilter::LogMessageFilter
    (const TimeRangeFilter& trFilter, const Filter * const filter) 
        : MultiComponentFilter(&trFilter,filter,AND) {
    validateFilter_();
}

LogMessageFilter::~LogMessageFilter() {
}

string LogMessageFilter::name() const {
    return "LogMessageFilter";
}

void LogMessage::validateFilter_() {
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
