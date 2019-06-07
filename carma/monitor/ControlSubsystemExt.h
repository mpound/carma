#ifndef CARMA_MONITOR_CONTROLSUBSYSTEMEXT_H
#define CARMA_MONITOR_CONTROLSUBSYSTEMEXT_H

/**
 * @file
 *
 * Semi-hand-forged extensions to the auto-generated classes for the
 * Control subsystem.  This file was originally
 * created by mpml2cpp but then modified manually by the author.
 *
 * @author: N. S. Amarnath
 *
 * $CarmaCopyright$
 *
 */

#include <set>

#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/Correlator.h"
#include "carma/util/PthreadMutex.h"


namespace carma {
namespace monitor {


/**
 * @brief The monitor system for the Control subsystem
 *
 *
 * This extends the functionality of the auto-generated class,
 * ControlSubsystemBase.
 */
class ControlSubsystem : public ControlSubsystemBase {
public:

    /**
     * Constructor
     * @param buffer   pointer to system frame storage; NULL
     *                         within subsystem
     */
    ControlSubsystem(SystemFrameBuffer * const buffer = 0);

    /**
     * Destructor
     */
    virtual ~ControlSubsystem();

    // add new or overriding method declarations here

    /**
     * Method to return the subarray controller container corresponding to
     * a specific subarray name, for example "Science1".
     *
     * @param  subarrayname const ::std::string name of subarray
     * @return Subarray* pointer to the subarray controller
     *         container corresponding to the specified subarray, i.e.
     *         subarray->name().getValue() == subarrayName
     *         NULL if subarrayName is illegal
     */
    Subarray * getSubarray( const ::std::string & subarrayName ) const;

    /**
     * Method to return the antenna control container corresponding to
     * a given type qualified antenna name, for example, "Bima1".
     *
     * @return antenna pointer to the antenna control container for the
     *        antenna corresponding to "antennaName". If not found,
     *        zero is returned.
     */
    Antenna * antennaByName( const ::std::string & antennaName );

    /**
     * Returns name of subarray with the correspoding subarray number.
     * Subarray numbers must be in the range 1 to
     * subarrayCount(), since CARMA
     * accomodates only subarrayCount()
       subarrays.
     *
     * @param subarrayNumber int subarray #, must be in
     *        [1, subarrayCount()]
     * @return const ::std::string name associated with subarray.
     */
    static ::std::string getSubarrayName( int subarrayNo );

    /**
     * Returns name of subarray with the correspoding subarray number.
     * Subarray numbers must be in the range 1 to
     * subarrayCount(), since CARMA
     * accomodates only subarrayCount()
       subarrays.
     *
     * @param subarrayNumber int subarray #, must be in
     *        [1, subarrayCount()]
     * @return const ::std::string name associated with subarray.
     */
    static ::std::string getSubarrayAlphanumericName( int subarrayNo );

    /**
     * Returns number of maintenance subarray.
     */
    static int getMaintenanceSubarrayNo( );

    ::std::set< Antenna * >
    getSubarrayAntennaGroup( int subarrayNo ) const;

private:

   /**
    * Initializes antennas, assigning them to the default subarray,
    * specified by the defaultSubarrayIndex.
    */
   void initializeAntennas( );

   /**
    * Initializes spectral line correlator, assigning name, correlator number,
    * and band numbers.
    */
   void initializeSpectralLineCorrelator();

   /**
    * Initializes wideband correlator, assigning name, correlator number,
    * and band numbers.
    */
   void initializeWidebandCorrelator();

   void initializeC3gMax8Correlator();
   void initializeC3gMax23Correlator();

   /**
    * Initializes correlators by calling initialization methods for each
    * type of correlator.
    */
   void initializeCorrelators();
};

} // namespace monitor
} // namespace carma

#endif
