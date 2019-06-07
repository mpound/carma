/**
 * @file
 * Class to provide information about CARMA Subarray capabilities.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 
#ifndef CARMA_MONITOR_CARMA_SUBARRAYS_H
#define CARMA_MONITOR_CARMA_SUBARRAYS_H

#include "carma/monitor/Correlator.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/ControlSubsystemExt.h"

namespace carma {
namespace monitor {


struct SubarrayInfo;


class Subarray {
  public:
    /**
     * @constructor
     * @brief Contructor - doesnt do anything other than provide access
     *        to static information that defines capabilities of CARMA.
     *
     * @param int subarrayNo - number of subarray
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    explicit Subarray( int subarrayNo );

    /**
     * @destructor
     * @brief Destroys this instance. No private members.
     */
    ~Subarray();

    /**
     * Returns number of subarrays for CARMA as defined in the
     * control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int   numSubarrays () ;

    /**
     * Returns number of bima antennas for CARMA as defined in the
     * control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int   numBimaAntennas () ;

    /**
     * Returns number of ovro antennas for CARMA as defined in the
     * control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int   numOvroAntennas () ;

    /**
     * Returns number of sza antennas for CARMA as defined in the
     * control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static int   numSzaAntennas () ;

    /**
     * Returns subarray name as defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    ::std::string subarrayName( ) const;

    //! @brief Returns subarray name as defined in the control design document
    //!
    //! @param int subarrayNo Number of subarray
    //! @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
    //!     section 2.1
    static ::std::string subarrayName( int subarrayNo );

    //! @brief Returns subarray alphanumeric name
    ::std::string subarrayAlphanumericName( ) const;

    //! @brief Returns subarray alphanumeric name
    //! @param int subarrayNo Number of subarray
    static ::std::string subarrayAlphanumericName( int subarrayNo );

    /**
     * Returns subarray number, for this subarray, as 
     * defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    long  subarrayNumber () const;

    /**
     * Returns a boolean which if true indicates that the subarray has an
     * associated correlator as defined in the control design document.
     * A subarray cannot have both an associated correlator as well as 
     * a shared passive LO reference.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    bool  hasCorrelator () const ;

    /**
     * Returns a reference to the Correlator object associated with
     * this subarray. If hasCorrelator() is false for this subarray, the 
     * method throws an ErrorException.
     *
     * @return const Correlator associated correlator object
     * @except throws ::carma::util::ErrorException if this subarray
     *         has no associated correlator.
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    const Correlator  correlator () const ;

    /**
     * Returns a boolean which if true indicates that the subarray has an
     * associated shared, passive LO reference as defined in the 
     * control design document. A subarray cannot have both an associated 
     * correlator as well as a shared passive LO reference.
     *
     * assert ( ! (subarray.hasCorrelator()  &&  subarray.hasSharedLOref()))
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    bool  hasSharedLOref () const ;

    /**
     * Returns total number of antennas across all subarrays, for CARMA, 
     * as defined in the control design document.
     *
     * @see http://www.mmarray.org/project/WP/Control/controlDesign.pdf 
     *     section 2.1
     */
    static const long totalNumAntennas;

  private:

    const SubarrayInfo & subarrayInfo_;
};  // class Subarray


}  // namespace carma::monitor
}  // namespace carma


#endif  // CARMA_MONITOR_CARMA_SUBARRAYS_H
