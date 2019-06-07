#ifndef CARMA_UTIL_TAG_AUTHORITY_MODE_H
#define CARMA_UTIL_TAG_AUTHORITY_MODE_H


namespace carma {
namespace util {


typedef enum {
    DB_TAG_AUTHORITY_MODE,
    CONF_FILE_TAG_AUTHORITY_MODE,
    ON_THE_FLY_TAG_AUTHORITY_MODE
} TagAuthorityMode;


}  // namespace carma::util
}  // namespace carma

#endif
