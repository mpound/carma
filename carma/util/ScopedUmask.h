#ifndef CARMA_UTIL_SCOPEDUMASK_H
#define CARMA_UTIL_SCOPEDUMASK_H


#include <sys/types.h>
#include <sys/stat.h>


namespace carma {
namespace util {


class ScopedUmask {
    public:
        explicit ScopedUmask( ::mode_t cmask );

        /* virtual */ ~ScopedUmask( );

    private:
        // No copying
        ScopedUmask( const ScopedUmask & rhs );
        ScopedUmask & operator=( const ScopedUmask & rhs );

        const ::mode_t oldCmask_;
};


} // namespace carma::util
} // namespace carma


inline
carma::util::ScopedUmask::ScopedUmask( const ::mode_t cmask ) :
oldCmask_( ::umask( cmask ) )
{
}


inline
carma::util::ScopedUmask::~ScopedUmask( )
try {
    ::umask( oldCmask_ );
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


#endif
