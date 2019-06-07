#ifndef CARMA_SDP_MONITORPOINTVALUE_H
#define CARMA_SDP_MONITORPOINTVALUE_H

/**
 * @file
 * Holds basic info on a monitor point value. Written for science data
 * products app to harvest info from monitor data flat files
 * @author Dave Mehringer
 * @auther Ira W. Snyder
 * @version $Id: MonitorPointValue.h,v 1.2 2012/09/22 01:34:44 iws Exp $
 * $CarmaCopyright$
 */
#include <carma/dbms/MonitorSystemAndDBMSRelationships.h>
#include <carma/monitor/MonitorPoint.h>
#include <carma/util/types.h>

#include <boost/shared_ptr.hpp>

#include <string>

namespace carma {
namespace sdp {

class MonitorPointValue {
public:

    MonitorPointValue(const carma::util::frameType &frameCount, const carma::monitor::MonitorPoint &mp);
    MonitorPointValue(const carma::dbms::MonitorAggregateType &type, const std::string &record);

    /**
     * get the frame count
     */
    carma::util::frameType frameCount() const;

    /**
     * get the tagID of the monitor point
     */
    int tagID() const;

    /**
     * get the blanking flag value associated with this average
     */
    carma::monitor::MonitorPoint::BLANKING_FLAGGING blankingFlag() const;

    /**
     * get the validity of this average
     */
    carma::monitor::MonitorPoint::VALIDITY validity() const;

    /**
     * get the total number of samples in this average
     */
    short totalNumberOfSamples() const;

    /**
     * get the number of valid samples associated with this average
     */
    short numberOfValidSamples() const;

    /**
     * get a string representation of the object
     */
    std::string toString() const;

    /**
     * get the avg/min/max value of the monitor point, as a string
     */
    std::string avgValue() const;
    std::string minValue() const;
    std::string maxValue() const;

    /**
     * parsers for the avg/min/max values returned by above functions
     */
    static short parseShort(const std::string &s);
    static double parseNumeric(const std::string &s);
    static std::complex<double> parseComplex(const std::string &s);

private:

    carma::util::frameType frameCount_;
    int tagID_;
    carma::monitor::MonitorPoint::BLANKING_FLAGGING blankingFlag_;
    carma::monitor::MonitorPoint::VALIDITY validity_;
    short nValidSamples_;
    short nTotalSamples_;

    std::string avgValue_;
    std::string minValue_;
    std::string maxValue_;
};

typedef boost::shared_ptr<MonitorPointValue> MonitorPointValuePtr;

} // namespace carma::sdp
} // namespace carma

#endif // CARMA_SDP_MONITORPOINTVALUE_H
