#ifndef CARMA_CORRELATOR_TRANSPORT_CORRDATAUPDATER_H
#define CARMA_CORRELATOR_TRANSPORT_CORRDATAUPDATER_H

#include <string>

namespace carma {
namespace correlator {

namespace obsRecord2 {


}  // namespace carma::correlator::obsRecord2


namespace transport {


class CorrDataUpdater {
    public:
        void
        runUpdateLoop( const ::std::string &   doName,
                       const ::std::string &   ecName,
                       const unsigned short    controlPort,
                       const ::std::string &   remoteName,
                       const int               portOffset,
                       const int               bandNo,
                       void                    (*shutdownCallback)( void * ),
                       void *                  shutdownCallbackArg,
                       const ::std::string &   hwType,
                       bool                    spectralLineMode,
                       const int               maxCorrDataAgeFrames );
};


}  // namespace carma::correlator::transport
}  // namespace carma::correlator
}  // namespace carma

#endif
