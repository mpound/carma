#ifndef CARMA_UTIL_LOGGING_UTILS_H
#define CARMA_UTIL_LOGGING_UTILS_H


#include <string>

#include <log4cpp/Priority.hh>


namespace log4cpp {

class Category;

}  // namespace log4cpp


namespace carma {
namespace util {


void logMultipleLines( ::log4cpp::Category &                    logger,
                       const ::log4cpp::Priority::PriorityLevel priorityLevel,
                       const ::std::string &                    s );


}  // namespace carma::util
}  // namespace carma


#endif
