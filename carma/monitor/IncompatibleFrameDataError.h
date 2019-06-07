#ifndef CARMA_MONITOR_INCOMPATIBLEFRAMEDATAERROR_H
#define CARMA_MONITOR_INCOMPATIBLEFRAMEDATAERROR_H

#include "carma/util/ErrorException.h"


namespace carma {
namespace monitor {


class IncompatibleFrameDataError : public carma::util::ErrorException {
    public:
        //! Constructor
        //! @param msg      The message for this exception. 
        //! @param filename The source file containing the code throwing the
        //!                 exception. 
        //! @param lineNo   The line number in the source file where the 
        //!                 exception is created.
        IncompatibleFrameDataError( const ::std::string & msg,
                                    const char *          filename,
                                    int                   lineNo );
};


} // namespace carma::monitor
} // namespace carma


#endif
