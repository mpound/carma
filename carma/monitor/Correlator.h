/**
 * @file
 * Class to provide information about CARMA Correlator capabilities.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 
#ifndef CARMA_MONITOR_CARMA_CORRELATOR_H
#define CARMA_MONITOR_CARMA_CORRELATOR_H

#include <string>

#include "carma/control/CorrDefs.h"
#include "carma/util/CorrelatorType.h"

namespace carma {
namespace monitor {

class Correlator  {
  public:
    struct Info;

    /**
     * @constructor
     * @brief Contructor - doesnt do anything other than provide access
     *        to static information that defines capabilities of CARMA.
     *
     * @param CorrelatorType - type of correlator
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    Correlator (const carma::util::CorrelatorType type);

    /**
     * @destructor
     * @brief Destroys this instance. No private members.
     */
    ~Correlator();

    /**
     * Returns number of correlators for CARMA as defined in the
     * control design document. Counts all types except Correlator::NONE.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static ::size_t numCorrelatorTypes( );

    /**
     * Returns correlator type for CARMA for the specified index.
     * If index is greater than Correlator::numCorrelatorTypes()
     * then it throws a ::carma::util::ErrorException.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static carma::util::CorrelatorType correlatorType( ::size_t index );

    /**
     * Returns correlator type, for CARMA, as 
     * defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    carma::util::CorrelatorType  correlatorType () const ;

/**
     * Returns correlator name, for CARMA, as 
     * defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    const ::std::string  correlatorTypeName () const ;

    /**
     * Returns correlator name, for CARMA, for the specified correlator type.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static const ::std::string  correlatorTypeName (carma::util::CorrelatorType corrType);

  private:

    const Info & info_;

}; // class Correlator

}  // namespace carma::monitor
}  // namespace carma


#endif  // CARMA_MONITOR_CARMA_CORRELATOR_H
