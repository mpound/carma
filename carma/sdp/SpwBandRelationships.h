/**
 * @file SpwBandRelationships.h
 *
 * Mapping of correlator bands and sidebands to output spectral windows.
 *
 * $Id: SpwBandRelationships.h,v 1.6 2009/12/01 22:26:33 friedel Exp $
 *
 * @author Athol Kemball
 */

#ifndef CARMA_SDP_SPWBANDRELATIONSHIPS_H
#define CARMA_SDP_SPWBANDRELATIONSHIPS_H

// Carma includes

// Carma tools includes

// C++ standard library includes
#include <vector>
#include <utility>

// Namespace using directives

// Class definitions
namespace carma {
  namespace sdp {
    /**
     * Mapping of correlator bands and sidebands to output spectral windows.
     * This class encodes the system-wide assumptions about the mapping
     * of correlator (band, sideband) tuples to output miriad spectral
     * window number. This is a fixed mapping that, by agreed convention, 
     * ensures that a given band and sideband maps to the same 
     * spectral window under all correlator and observing configurations. 
     * The agreed spw order is: [LSB1, LSB2, LSB3, USB1, USB2, USB3],
     * where [1,2,3] are CARMA band numbers. This class also encodes
     * information about the fixed index order of band-related astro header
     * variables.
     */

    class SpwBandRelationships
      {
      public:
	/** Enumerated sideband types.
	 */
	enum SIDEBAND {
	  LSB = 0,
	  USB = 1,
	  DSB = 2};

	/** Constructor.
	 * Null constructor.
	 */
	SpwBandRelationships();

	/** Desctructor
	 */
	~SpwBandRelationships();

	/** Return the number of output spectral windows.
	 */
	static int numSpw();

	/** Return true if specified spectral window is LSB.
	 */
	static bool isLSB(const int& spw);

	/** Return true if specified spectral window is USB.
	 */
	static bool isUSB(const int& spw);

	/** Return the matching sideband spw for a specified input spw.
	 */
	static int matchingSbSpw(const int& spw);

	/** Map a (band,sideband) tuple to the matching spectral windows.
	 * Note that autocorrelation DSB maps to two spectral windows.
	 */
	static std::vector<int> bandSbToSpw(const int& band,
					    const SIDEBAND& sideband);

	/** Map a spectral window to the matching band number.
	 */
	static int spwToBand(const int& spw);

	/** Map a spectral window to the matching (single) sideband.
	 */
	static SIDEBAND spwToSideband(const int& spw);

	/** Map a (band,sideband) tuple to a unique sequential array index.
	 * This is used in class XMLHandler to index correlator bands.
	 */
	static int bandSbIndex(const int& band, const SIDEBAND& sideband);

	/** Compute inverse mapping of bandSbIndex method.
	 */
	static std::pair<int, SIDEBAND> bandSbIndexInv(const int& index);

	/** Map a (band,sideband) tuple to a bandfreq astro header index.
	 */
	static int bandFreqIndex(const int& band, const SIDEBAND& sideband);

	/** Map a (ant,band,sideband) tuple to a systemp astro header index.
	 */
	static int sysTempIndex(const int& ant, const int& nant,
				const int& band, const SIDEBAND& sideband);

 	/** Map a (ant,band) pair to a psys astro header index.
	 */
	static int psysIndex(const int& ant, const int& nant,
			     const int& band);

	/** Set the number of windows in the data set(6 or 16)
	 */
	static void setNumWin(const int &num);

     private: 

	static int numWin;  // number of windows in data set
      };
  }; // namespace sdp
}; // namespace carma


#endif //CARMA_SDP_SPWBANDRELATIONSHIPS_H
