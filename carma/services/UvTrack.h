#ifndef CARMA_SERVICES_UVTRACK_H
#define CARMA_SERVICES_UVTRACK_H

#include <string>
#include "carma/services/Frequency.h"
#include "carma/services/Interpolator.h"
#include "carma/services/Table.h"


namespace carma {
  namespace services {
      // forward declarations
      class DecAngle;

  class UvTrack {
      public:

	  /**
	   * @param arrayConfig Array configuration, one of
	   * "A","B","C","D","E".  Case-insensitive.
	   * @param freq Frequency of observation
	   */
	  UvTrack( const ::std::string & arrayConfig, 
		   const Frequency     & freq );

	  virtual ~UvTrack( );

	  /**
	   * @return Optimal trackLength in hours, where
	   * the optimal length is that required to reach
	   * 90% of the achievable SNR.
	   * @see CARMA Memo #41
	   */
	  double optimalLength( const DecAngle & dec );
	  double policyLength( const DecAngle & dec );
	  double policyLength( const double decDegrees );
	  double optimalLength( const double decDegrees );

	  /**
	   * @return the maximum length allowed by SSC policy
	   */
	  double maxLength( void ) const {
	      return MAX_ALLOWABLE_LENGTH_HRS;
	  }

      private:
	  void initialize();
	  // Table containing track data
	  Table table_;
	  ::std::string arrayConfig_;
	  services::Frequency freq_;;
	  services::Interpolator * interp_;

	  static const double MAX_ALLOWABLE_LENGTH_HRS;
	  static const double HIGH_FREQ_CUTOFF_GHZ;

  };

  } // services
} // carma


#endif //CARMA_SERVICES_UVTRACK_H
