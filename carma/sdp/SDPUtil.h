/**
 * @file SDPUtil.h
 *
 * Utility class for the science data products subsystem.
 *
 * $Id: SDPUtil.h,v 1.2 2008/12/10 18:03:15 friedel Exp $
 *
 * @author Athol Kemball
 */

#ifndef CARMA_SDP_SDPUTIL_H
#define CARMA_SDP_SDPUTIL_H

// Carma includes

// Carma tools includes

// C++ standard library includes
#include <string>
#include <vector>

// Namespace using directives

// Class definitions
namespace carma {
  namespace sdp {
    /**
     * Utility class for the science data products subsystem.
     * This class contains static utility functions used across
     * the science data products subsystem.
     */

    class SDPUtil
      {
      public:
	/** Compose the astro header file name for a given observing block id.
	 */
	static std::string astroHdrFileName(const std::string& dir,
					    const std::string& obsBlockId);

	/** Compose output SDP file name associated with an astro header file.
	 */
	static std::string sdpFileName(const std::string& dir,
				       const std::string& astroHdrFile);

	/** Delete an existing sdp output file.
	 */
	static void deleteSDPFile(const std::string& outputSDPFile);

	/** Extract the observing block id. from a file name.
	 * This method extracts the observing block id. from an input
	 * monitor point file name or output astronominal header file name.
	 */
	static std::string extractObsBlockId(const std::string& fileName);

	/** Parse an obs. block id. descriptor into sub-components.
	 */
	static std::vector<std::string> parseObsBlockId(const std::string& 
							obsBlockId);
	static void mailRTS(const std::string& message);
      };
  }; // namespace sdp
}; // namespace carma


#endif //CARMA_SDP_SDPUTIL_H
