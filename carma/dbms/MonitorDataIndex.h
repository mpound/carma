#ifndef CARMA_DBMS_MONITORDATAINDEX_H
#define CARMA_DBMS_MONITORDATAINDEX_H

/**
 * @file
 * MonitorDataIndex class
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"

namespace carma {
namespace dbms {

/**
 * Class for indexing monitor point data areas
 */
class MonitorDataIndex {
public:
    
    /**
     *  constructor
     * @param dataType the mp data type
     * @param avgType the mp average type
     * @param areaType the mp area type
     */
    MonitorDataIndex(const MonitorAggregateType& aggType, 
                     const MonitorAverageType& avgType, 
                     const MonitorDataAreaType& areaType);
 
    /**
     * Destructor
     */
    
    ~MonitorDataIndex();
    

    inline bool operator<(const MonitorDataIndex& rhs) const {
        return (this->numericIndex_ < rhs.numericIndex_);
    }
    
    inline bool operator>(const MonitorDataIndex& rhs) const {
        return (this->numericIndex_ > rhs.numericIndex_);
    }
    
    inline bool operator==(const MonitorDataIndex& rhs) const {
        return (this->numericIndex_ == rhs.numericIndex_);
    }
    
    /**
     * get the data area type associated with the index
     * @return the area type
     */
    inline MonitorDataAreaType getAreaType()const { return areaType_; }

    /**
     * get the average type associated with the index
     * @return the average type
     */
    inline MonitorAverageType getAverageType()const { return avgType_; }

    /**
     * get the aggregate type associated with the index
     * @return the aggregate type
     */
    inline MonitorAggregateType getAggregateType()const { return aggType_; }

    inline int getIndex()const{return numericIndex_;}
protected:
    MonitorAggregateType aggType_; 
    MonitorAverageType avgType_;
    MonitorDataAreaType areaType_;
    int numericIndex_;
};

}}
#endif
