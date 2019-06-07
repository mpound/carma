#ifndef SZA_UTIL_CENTERMANAGER_H
#define SZA_UTIL_CENTERMANAGER_H

/**
 * @file CenterManager.h
 * 
 * Tagged: Fri Apr 30 18:02:48 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <list>
#include <map>
#include <vector>

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Center.h"

#include "carma/szaarrayutils/source.h"

namespace sza {
  namespace util {
    
    class CenterManager {
    public:
      
      /**
       * Constructor.
       */
      CenterManager();
      
      /**
       * Destructor.
       */
      virtual ~CenterManager();

      /**
       * Change the center for a set of antennas.
       */
      Center* changeCenter(AntNum::Id antennas, sza::array::SourceId srcId);

      /**
       * Return the center described by a source id.
       */
      Center* getCenter(sza::array::SourceId* srcId, bool throwOnError=true);
      
      /**
       * Return the center described by a catalog number
       */
      Center* getCenter(unsigned catNumber);
      
      /**
       * Return the center for a set of antennas
       */
      Center* getCenter(AntNum::Id antennas, bool reportError=true);

      /**
       * Get a pointer to all centers we know about.
       */
      std::list<Center* >* getCenterList();

      /**
       * Get a vector of center-antenna associations.  Defaults to
       * returning centers for all antennas.
       */
      std::vector<std::pair<sza::array::SourceId, AntNum::Id> >
	getCenterAssociations(AntNum::Id antennas);

      /**
       * Update a cache window to reflect the most stringent
       * requirements of any of the sources we are managing which need
       * updating.
       *
       * If refTime is non-zero, use the min of (tmax calculated from
       * sources, or refMaxTime)
       */
      void updateCacheWindow(sza::array::CacheWindow* window, 
			     double refMaxTime=0.0);

    private:

      /**
       * A list of centers associated with at last one antenna
       */
      std::list<Center* > centerList_;

      /**
       * A map of catalog-number, center associations.
       */
      std::map<unsigned, Center* > centerBySourceIdMap_;

      /**
       * A map of antenna-center associations
       */
      std::map<AntNum::Id, Center* > centerByAntennaIdMap_;

      /**
       * Update the map of centers by antenna id
       */
      void updateAntennaCenterMap();

    }; // End class CenterManager
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CENTERMANAGER_H
