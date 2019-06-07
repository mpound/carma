#ifndef CARMA_CORRELATOR_TRANSPORT_CORRMONUPDATER_H
#define CARMA_CORRELATOR_TRANSPORT_CORRMONUPDATER_H

#include <string>

namespace carma {
namespace correlator {


namespace lib {

class CorrelatorConfigChecker;

}  // namespace carma::correlator::lib


namespace obsRecord2 {

}  // namespace carma::correlator::obsRecord2


namespace transport {


class CorrMonUpdater {
    public:
        void runUpdateLoop( const int                      bandNo,
                            lib::CorrelatorConfigChecker & ccc,
                            const unsigned short           controlPort,
                            const ::std::string &          controlHost,
                            const int                      portOffset,
                            const ::std::string          & hwStr );
};


}  // namespace carma::correlator::transport
}  // namespace carma::correlator
}  // namespace carma

#endif
