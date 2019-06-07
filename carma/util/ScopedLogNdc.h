#ifndef CARMA_UTIL_SCOPED_LOG_NDC_H
#define CARMA_UTIL_SCOPED_LOG_NDC_H


#include <string>


namespace carma {
namespace util {


class ScopedLogNdc {
    public:
        explicit ScopedLogNdc( const char * s );

        explicit ScopedLogNdc( const ::std::string & s );

        ScopedLogNdc( const char * s,
                      const char * prefix,
                      const char * suffix );

        ScopedLogNdc( const ::std::string & s,
                      const ::std::string & prefix,
                      const ::std::string & suffix );

        /* virtual */ ~ScopedLogNdc( );

    private:
        // No copying
        ScopedLogNdc( const ScopedLogNdc & rhs );
        ScopedLogNdc & operator=( const ScopedLogNdc & rhs );
};


} // namespace carma::util
} // namespace carma


#endif
