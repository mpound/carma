#ifndef CARMA_CORRELATOR_LIB_CORRELATORDATAPTR_H
#define CARMA_CORRELATOR_LIB_CORRELATORDATAPTR_H

#include <boost/shared_ptr.hpp>

namespace carma {
namespace correlator {
namespace lib {

    class CorrelatorData;
    typedef boost::shared_ptr< CorrelatorData > CorrelatorDataPtr;

}}} // namespace carma::correlator::lib
#endif
