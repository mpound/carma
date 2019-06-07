/**
 * @file
 * implementation of MonitorData2DBMSConversions
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/MonitorData2DBMSConversions.h"

#include "carma/util/ErrorException.h"

#include <sstream>

using namespace std;

namespace {

/*
 * WARNING: IF THESE CONSTANT VALUES ARE ALTERED, THE DATABASE WILL LOSE
 * INTEGRITY. 
 */
const unsigned short DB_INVALID_NO_DATA        =  0;
const unsigned short DB_INVALID_NO_HW          =  8;
const unsigned short DB_INVALID_HW_BAD         = 16;
const unsigned short DB_VALID                  = 24;
const unsigned short DB_VALID_NOT_CHECKED      = 32;
const unsigned short DB_VALID_GOOD             = 40;
const unsigned short DB_VALID_WARNING          = 48;
const unsigned short DB_VALID_ERROR            = 56;
const unsigned short DB_VALID_WARNING_LOW      = 64;
const unsigned short DB_VALID_WARNING_HIGH     = 72;
const unsigned short DB_VALID_ERROR_LOW        = 80;
const unsigned short DB_VALID_ERROR_HIGH       = 88;

const unsigned short DB_UNDETERMINED           =  0;
const unsigned short DB_OK                     = 20;
const unsigned short DB_BLANKED                = 40;
const unsigned short DB_FLAGGED                = 60;
const unsigned short DB_BLANKED_FLAGGED        = 80;

} // namespace < unnamed >

unsigned short carma::dbms::validity2DB(const carma::monitor::MonitorPoint::VALIDITY& validity) 
{
    switch(validity) {
        case carma::monitor::MonitorPoint::INVALID_NO_DATA:
            return DB_INVALID_NO_DATA;
            break;
        case carma::monitor::MonitorPoint::INVALID_NO_HW:
            return DB_INVALID_NO_HW;
            break;
        case carma::monitor::MonitorPoint::INVALID_HW_BAD:
            return DB_INVALID_HW_BAD;
            break;
        case carma::monitor::MonitorPoint::VALID:
            return DB_VALID;
            break;
        case carma::monitor::MonitorPoint::VALID_NOT_CHECKED:
            return DB_VALID_NOT_CHECKED;
            break;
        case carma::monitor::MonitorPoint::VALID_GOOD:
            return DB_VALID_GOOD;
            break;
        case carma::monitor::MonitorPoint::VALID_WARNING:
            return DB_VALID_WARNING;
            break;
        case carma::monitor::MonitorPoint::VALID_ERROR:
            return  DB_VALID_ERROR;
            break;
        case carma::monitor::MonitorPoint::VALID_WARNING_LOW:
            return DB_VALID_WARNING_LOW;
            break;
        case carma::monitor::MonitorPoint::VALID_WARNING_HIGH:
            return DB_VALID_WARNING_HIGH;
            break;
        case carma::monitor::MonitorPoint::VALID_ERROR_LOW:
            return DB_VALID_ERROR_LOW;
            break;
        case carma::monitor::MonitorPoint::VALID_ERROR_HIGH:
            return DB_VALID_ERROR_HIGH;
            break;
        default:
            std::ostringstream ss;
            ss << "Unhandled validity value " << validity;
            throw CARMA_ERROR(ss.str());
    }
}


carma::monitor::MonitorPoint::VALIDITY 
carma::dbms::db2Validity(const unsigned short& dbValidity) {
    switch(dbValidity) {
        case DB_INVALID_NO_DATA:
            return carma::monitor::MonitorPoint::INVALID_NO_DATA;
            break;
        case DB_INVALID_NO_HW:
            return carma::monitor::MonitorPoint::INVALID_NO_HW;
            break;
        case DB_INVALID_HW_BAD:
            return carma::monitor::MonitorPoint::INVALID_HW_BAD;
            break;
        case DB_VALID:
            return carma::monitor::MonitorPoint::VALID;
            break;
        case DB_VALID_NOT_CHECKED:
            return carma::monitor::MonitorPoint::VALID_NOT_CHECKED;
            break;
        case DB_VALID_GOOD:
            return carma::monitor::MonitorPoint::VALID_GOOD;
            break;
        case DB_VALID_WARNING:
            return carma::monitor::MonitorPoint::VALID_WARNING;
            break;
        case DB_VALID_ERROR:
            return carma::monitor::MonitorPoint::VALID_ERROR;
            break;
        case DB_VALID_WARNING_LOW:
            return carma::monitor::MonitorPoint::VALID_WARNING_LOW;
            break;
        case DB_VALID_WARNING_HIGH:
            return carma::monitor::MonitorPoint::VALID_WARNING_HIGH;
            break;
        case DB_VALID_ERROR_LOW:
            return carma::monitor::MonitorPoint::VALID_ERROR_LOW;
            break;
        case DB_VALID_ERROR_HIGH:
            return carma::monitor::MonitorPoint::VALID_ERROR_HIGH;
            break;
        default:
            std::ostringstream ss;
            ss << "Unhandled db validity value " << dbValidity;
            throw CARMA_ERROR(ss.str());
    }
}

string
carma::dbms::validityToString( 
    const carma::monitor::MonitorPoint::VALIDITY validity )
{
    using namespace carma::monitor;
    switch ( validity ) {
        case MonitorPoint::INVALID_NO_DATA:    return "INVALID_NO_DATA";
        case MonitorPoint::INVALID_NO_HW:      return "INVALID_NO_HW";
        case MonitorPoint::INVALID_HW_BAD:     return "INVALID_HW_BAD";
        case MonitorPoint::VALID:              return "VALID";
        case MonitorPoint::VALID_NOT_CHECKED:  return "VALID_NOT_CHECKED";
        case MonitorPoint::VALID_GOOD:         return "VALID_GOOD";
        case MonitorPoint::VALID_WARNING:      return "VALID_WARNING";
        case MonitorPoint::VALID_ERROR:        return "VALID_ERROR";
        case MonitorPoint::VALID_WARNING_LOW:  return "VALID_WARNING_LOW";
        case MonitorPoint::VALID_WARNING_HIGH: return "VALID_WARNING_HIGH";
        case MonitorPoint::VALID_ERROR_LOW:    return "VALID_ERROR_LOW";
        case MonitorPoint::VALID_ERROR_HIGH:   return "VALID_ERROR_HIGH";
        case MonitorPoint::MAX_VALIDITY: break;
    }

    return "No validity string!!";
}

/**
 * convert the MonitorPoint::BLANKING_FLAGGING value to the value which is 
 * written in the db
 */
unsigned short 
carma::dbms::blankingFlagging2DB(
    const carma::monitor::MonitorPoint::BLANKING_FLAGGING& blanking) 
{
    switch(blanking) {
        case carma::monitor::MonitorPoint::UNDETERMINED:
            return DB_UNDETERMINED;
            break;
        case carma::monitor::MonitorPoint::OK:
            return DB_OK;
            break;
        case carma::monitor::MonitorPoint::BLANKED:
            return DB_BLANKED;
            break;
        case carma::monitor::MonitorPoint::FLAGGED:
            return DB_FLAGGED;
            break;
        case carma::monitor::MonitorPoint::BLANKED_FLAGGED:
            return DB_BLANKED_FLAGGED;
            break;
        default:
            std::ostringstream ss;
            ss << "Unhandled blanking_flagging value " << blanking;
            throw CARMA_ERROR(ss.str());
    }
}

carma::monitor::MonitorPoint::BLANKING_FLAGGING 
carma::dbms::db2BlankingFlagging
(const unsigned short& dbBlanking) {
    switch(dbBlanking) {
        case DB_UNDETERMINED:
            return carma::monitor::MonitorPoint::UNDETERMINED;
            break;
        case DB_OK:
            return carma::monitor::MonitorPoint::OK;
            break;
        case DB_BLANKED:
            return carma::monitor::MonitorPoint::BLANKED;
            break;
        case DB_FLAGGED:
            return carma::monitor::MonitorPoint::FLAGGED;
            break;
        case DB_BLANKED_FLAGGED:
            return carma::monitor::MonitorPoint::BLANKED_FLAGGED;
            break;
        default:
            std::ostringstream ss;
            ss << "Unhandled db blanking_flagging value " << dbBlanking;
            throw CARMA_ERROR(ss.str());
    }
}

std::string 
carma::dbms::blankingFlaggingToString( 
        const carma::monitor::MonitorPoint::BLANKING_FLAGGING f )
{
    using namespace carma::monitor;
    switch ( f ) {
        case MonitorPoint::OK:              return "OK";
        case MonitorPoint::UNDETERMINED:    return "UNDETERMINED";
        case MonitorPoint::BLANKED:         return "BLANKED";
        case MonitorPoint::FLAGGED:         return "FLAGGED";
        case MonitorPoint::BLANKED_FLAGGED: return "BLANKED_FLAGGED";

        case MonitorPoint::MAX_BLANKING_FLAGGING: break;
    }

    return "No blanking/flagging string!!";
}
