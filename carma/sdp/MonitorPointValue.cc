/**
 * @author: Dave Mehringer
 * @auther: Ira W. Snyder
 * @version: $Id: MonitorPointValue.cc,v 1.3 2013/09/06 17:55:33 iws Exp $
 * $CarmaCopyright$
 */

#include <carma/sdp/MonitorPointValue.h>
#include <carma/dbms/MonitorData2DBMSConversions.h>
#include <carma/monitor/monitorPointSpecializations.h>
#include <carma/util/IllegalArgumentException.h>
#include <carma/util/StringUtils.h>
#include <carma/util/programLogging.h>

#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/qi_numeric.hpp>
#include <boost/foreach.hpp>

#include <sstream>
#include <vector>

using carma::monitor::MonitorPoint;
using carma::dbms::MonitorAggregateType;

using namespace carma::dbms;
using namespace carma::sdp;
using namespace carma::util;

typedef std::vector<std::string> StringVector;

/* ========================================================================== */
/* String Parsing Helpers                                                     */
/* ========================================================================== */

static short str2short(const std::string &s)
{
    std::string::const_iterator it = s.begin();
    short tmp;

    if (!boost::spirit::qi::parse(it, s.end(), boost::spirit::short_, tmp)) {
        std::ostringstream oss;
        oss << "Unable to parse as short: " << s;
        throw CARMA_ERROR(oss.str());
    }

    if (it != s.end()) {
        std::ostringstream oss;
        oss << "Unable to parse as short (all chars not consumed): " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static int str2int(const std::string &s)
{
    std::string::const_iterator it = s.begin();
    int tmp;

    if (!boost::spirit::qi::parse(it, s.end(), boost::spirit::int_, tmp)) {
        std::ostringstream oss;
        oss << "Unable to parse as int: " << s;
        throw CARMA_ERROR(oss.str());
    }

    if (it != s.end()) {
        std::ostringstream oss;
        oss << "Unable to parse as int (all chars not consumed): " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static double str2double(const std::string &s)
{
    std::string::const_iterator it = s.begin();
    double tmp;

    if (!boost::spirit::qi::parse(it, s.end(), boost::spirit::double_, tmp)) {
        std::ostringstream oss;
        oss << "Unable to parse as double: " << s;
        throw CARMA_ERROR(oss.str());
    }

    if (it != s.end()) {
        std::ostringstream oss;
        oss << "Unable to parse as double (all chars not consumed): " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static std::complex<double> str2complex(const std::string &s)
{
    const StringVector tokens = StringUtils::tokenize(s, "\t", false);
    const double real = str2double(tokens[0]);
    const double imag = str2double(tokens[1]);
    return std::complex<double>(real, imag);
}

/* ========================================================================== */
/* Internal Use Helpers                                                       */
/* ========================================================================== */

static void checkMonitorAggregrateType(const MonitorAggregateType &type)
{
    switch (type) {
    case NUMERIC_TYPE:
    case STRING_TYPE:
    case SHORT_TYPE:
    case COMPLEX_TYPE:
        // these are all valid types
        break;
    default:
        throw CARMA_ERROR("Invalid MonitorAggregateType");
    }
}

static void checkRequiredTokens(
        const MonitorAggregateType &type,
        const size_t &size,
        const std::string &record)
{
    bool error = true;
    if (type == NUMERIC_TYPE && size == 10)
        error = false;

    if (type == STRING_TYPE && size == 7)
        error = false;

    if (type == SHORT_TYPE && size == 10)
        error = false;

    if (type == COMPLEX_TYPE && size == 13)
        error = false;

    if (error) {
        std::ostringstream oss;
        oss << "Record contains incorrect number of fields: " << record;
        throw CARMA_EXCEPTION(IllegalArgumentException, oss.str());
    }
}

static carma::util::frameType parseFrameCount(const std::string &s)
{
    std::istringstream iss(s);
    carma::util::frameType tmp;

    if (!(iss >> tmp)) {
        std::ostringstream oss;
        oss << "Unable to parse as a framecount: " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static int parseTagID(const std::string &s)
{
    int tmp;

    // parse it
    try {
        tmp = str2int(s);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse as a tagID: " << s;
        throw CARMA_ERROR(oss.str());
    }

    // check the tagid for sanity
    if (tmp <= 65536) {
        std::ostringstream oss;
        oss << "TagID must be greater than 65536, but was found to be: str=" << s << " int=" << tmp;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static MonitorPoint::BLANKING_FLAGGING parseBF(const std::string &s)
{
    short tmp;

    // parse it
    try {
        tmp = str2short(s);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse as a BFID: " << s;
        throw CARMA_ERROR(oss.str());
    }

    // convert
    return db2BlankingFlagging(tmp);
}

static MonitorPoint::VALIDITY parseValidity(const std::string &s)
{
    short tmp;

    // parse it
    try {
        tmp = str2short(s);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse as a VALIDITY: " << s;
        throw CARMA_ERROR(oss.str());
    }

    // convert
    return db2Validity(tmp);
}

static short parseNValidSamples(const std::string &s)
{
    short tmp;

    // parse it
    try {
        tmp = str2short(s);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse as a nValidSamples: " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static short parseNTotalSamples(const std::string &s)
{
    short tmp;

    // parse it
    try {
        tmp = str2short(s);
    } catch (...) {
        std::ostringstream oss;
        oss << "Unable to parse as a nTotalSamples: " << s;
        throw CARMA_ERROR(oss.str());
    }

    return tmp;
}

static std::string getMonitorPointValue(const MonitorPoint &mp)
{
    using namespace carma::monitor;

    const MonitorValueType type = mp.getValuetype();

    // monitor point enumerations are a pain
    if (type == MONITOR_VALUE_TYPE_INTEGER) {
        const MonitorPointEnum *mpe = dynamic_cast<const MonitorPointEnum *>(&mp);
        if (mpe != NULL) {
            std::ostringstream oss;
            oss << mpe->getAve();
            return oss.str();
        }
    }

    // monitor point booleans default to "true" and "false": we want integers
    if (type == MONITOR_VALUE_TYPE_BOOLEAN) {
        const MonitorPointBool *mpb = dynamic_cast<const MonitorPointBool *>(&mp);
        return (mpb->getAve()) ? "1" : "0";
    }

    // everything else is sane
    return mp.getAverageToString();
}

/* ========================================================================== */
/* MonitorPointValue Class                                                    */
/* ========================================================================== */

MonitorPointValue::MonitorPointValue(const frameType &frameCount, const MonitorPoint &mp)
    : frameCount_(frameCount)
    , tagID_(mp.getTagID())
    , blankingFlag_(mp.getBlankingFlagging())
    , validity_(mp.getAveValidity())
    , nValidSamples_(mp.getNumValidSamples())
    , nTotalSamples_(mp.getNumSamples())
    , avgValue_(getMonitorPointValue(mp))
    , minValue_(getMonitorPointValue(mp))
    , maxValue_(getMonitorPointValue(mp))
{
}

MonitorPointValue::MonitorPointValue(const MonitorAggregateType &type, const std::string &record)
{
    // check the provided type
    checkMonitorAggregrateType(type);

    // tokenize the record, trim all whitespace
    StringVector tokens = StringUtils::tokenize(record, "\t", false);
    BOOST_FOREACH(std::string &token, tokens) {
        StringUtils::trimWhiteSpaceInplace(token);
    }

    // check that we have the required number of tokens
    checkRequiredTokens(type, tokens.size(), record);

    // common fields
    frameCount_ = parseFrameCount(tokens[0]);
    tagID_ = parseTagID(tokens[1]);
    blankingFlag_ = parseBF(tokens[2]);
    validity_ = parseValidity(tokens[3]);
    nValidSamples_ = parseNValidSamples(tokens[tokens.size() - 2]);
    nTotalSamples_ = parseNTotalSamples(tokens[tokens.size() - 1]);

    // type specific fields
    if (type == NUMERIC_TYPE) {
        avgValue_ = tokens[4];
        maxValue_ = tokens[5];
        minValue_ = tokens[6];

        // numeric type must be parseable as a double
        str2double(avgValue_);
        str2double(minValue_);
        str2double(maxValue_);
    } else if (type == STRING_TYPE) {
        avgValue_ = tokens[4];
        maxValue_ = tokens[4];
        minValue_ = tokens[4];
    } else if (type == SHORT_TYPE) {
        avgValue_ = tokens[4];
        maxValue_ = tokens[5];
        minValue_ = tokens[6];

        // short type must be parseable as a short
        str2short(avgValue_);
        str2short(maxValue_);
        str2short(minValue_);
    } else if (type == COMPLEX_TYPE) {
        avgValue_ = tokens[4] + "\t" + tokens[5];
        maxValue_ = tokens[6] + "\t" + tokens[7];
        minValue_ = tokens[8] + "\t" + tokens[9];

        // complex type must be parseable as a complex
        str2complex(avgValue_);
        str2complex(maxValue_);
        str2complex(minValue_);
    } else {
        // can't happen unless above checks are broken
        throw CARMA_ERROR("Unsupported MonitorAggregateType");
    }
}

carma::util::frameType MonitorPointValue::frameCount() const
{
    return frameCount_;
}

int MonitorPointValue::tagID() const
{
    return tagID_;
}

MonitorPoint::BLANKING_FLAGGING MonitorPointValue::blankingFlag() const
{
    return blankingFlag_;
}

MonitorPoint::VALIDITY MonitorPointValue::validity() const
{
    return validity_;
}

short MonitorPointValue::totalNumberOfSamples() const
{
    return nTotalSamples_;
}

short MonitorPointValue::numberOfValidSamples() const
{
    return nValidSamples_;
}

std::string MonitorPointValue::toString() const
{
    std::ostringstream oss;
    oss << "MonitorPointValue: tagID=" << tagID_ << " avg=" << avgValue_;
    return oss.str();
}

std::string MonitorPointValue::avgValue() const
{
    return avgValue_;
}

std::string MonitorPointValue::minValue() const
{
    return minValue_;
}

std::string MonitorPointValue::maxValue() const
{
    return maxValue_;
}

short MonitorPointValue::parseShort(const std::string &s)
{
    return str2short(s);
}

double MonitorPointValue::parseNumeric(const std::string &s)
{
    return str2double(s);
}

std::complex<double> MonitorPointValue::parseComplex(const std::string &s)
{
    return str2complex(s);
}

/* vim: set tw=112 ts=4 sts=4 sw=4 et: */
