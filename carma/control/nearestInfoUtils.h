#ifndef CARMA_CONTROL_NEARESTINFO_UTILS_H
#define CARMA_CONTROL_NEARESTINFO_UTILS_H

#include "carma/corba/corba.h"
#include "carma/control/NearestInfo.h"
#include "carma/services/Neighbor.h"

namespace carma {
  namespace control {

      /** 
       * @brief Convert Neighbor objects to an IDL sequence of 
       * NearestInfo objects
       * @return an IDL sequence of NearestInfo objects given an
       *         std::set< carma::services::Neighbor >
       * @param neighborSet The std::set< carma::services::Neighbor > to 
       * convert.
       */
       NearestInfoSeq_var convertNeighborSetToNearestInfoSeq( 
	       const carma::services::NeighborSet & neighborSet );

       /**
	* @brief Copy the data from a single services::Neighbor object 
	* to a control::NearestInfo object.
	* @param neighbor Input Neighbor object
	*/
       NearestInfo copyNeighbor( const carma::services::Neighbor & neighbor );

  };
};

#endif //CARMA_CONTROL_NEARESTINFO_UTILS_H
