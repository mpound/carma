// $Id: CorrelatorSet.h,v 1.6 2014/06/04 17:09:52 mpound Exp $

#ifndef CARMA_UTIL_CORRELATORSET_H
#define CARMA_UTIL_CORRELATORSET_H

/**
 * @file 
 * 
 * Tagged: Wed Sep 19 09:06:32 PDT 2012
 * 
 * @version: $Revision: 1.6 $, $Date: 2014/06/04 17:09:52 $
 * 
 * @author Erik Leitch
 */

#include <string>
#include <sstream>
#include <vector>
#include "carma/util/CorrelatorType.h"

namespace carma {
  namespace util {

    /**
     * @brief CorrelatorSet encapsulates a set of correlators to 
     * facilitate management of
     * addition and removal of correlators to a subarray. 
     *
     * The class provides methods to discover what kinds of correlators are 
     * in the set (SL, WB, C3GMAX8, C3GMAX23), without having
     * to know the detailed bitmask/enum.
     */
    class CorrelatorSet {
    public:

      /**
       * Constructor.
       */
      CorrelatorSet();

      /**
       * Constructor.
       * @param correlator designation enum
       */
      CorrelatorSet(carma::util::CorrelatorType corr);

      /**
       * Constructor.
       * @param corr monitor namespace correlator designation enum
      CorrelatorSet(MonitorCorrelatorDesignation corr);
       */

      /**
       * Destructor.
       */
      virtual ~CorrelatorSet();


      /** Initialize this correlator set
       * @param corr correlator designation enum
       */
      void initialize(carma::util::CorrelatorType corr);

      /** Initialize this correlator set
       * @param corr monitor namespace correlator designation enum
      void initialize(MonitorCorrelatorDesignation corr);
       */

      /** Add a correlator to this set
       * @param corr correlator designation enum
       */
      void addCorrelator(carma::util::CorrelatorType corr);

      /** Remove a correlator from this set
       * @param corr correlator designation enum
       */
      void removeCorrelator(carma::util::CorrelatorType corr);


      /** 
       * @return string representation of enum 
       * this correlator set
       */
      std::string corrTypeString() const;

      /**
       * @return the underlying correlator type value
       */
      CorrelatorType corrType() const;

      /** 
       * @return string representation of monitor namespace enum this correlator set
       */
      std::string mpString() const;
      
      /**
       * @return true if this set represents a single correlator
       */
      bool isSingleCorrelator() const;
      
      /**
       * @return true if this set is empty
       */
      bool isEmpty() const;

      /**
       * @return true if this set contains all correlators
       */
      bool isAll() const;

      // Return true if this set represents any of the following
      // single correlators

      /** 
       * @return true if this set contains ONLY the spectral correlator.
       */
      bool isSpectral() const;

      /** 
       * @return true if this set contains ONLY the wideband correlator.
       */
      bool isWideband() const;

      /** 
       * @return true if this set contains ONLY the C3G Max8 correlator.
       */
      bool isC3gMax8() const;      

      /** 
       * @return true if this set contains ONLY the C3G Max23 correlator.
       */
      bool isC3gMax23() const;

      /** 
       * @return true if this set contains any the C3G band.
       */
      bool isC3g() const; 

      // Return true if this set includes any of the following single
      // correlators

      /** 
       * @return true if this set contains the spectral correlator.
       */
      bool includesSpectral() const;

      /** 
       * @return true if this set contains the wideband correlator.
       */
      bool includesWideband() const;

      /** 
       * @return true if this set contains the C3G Max8 correlator.
       */
      bool includesC3gMax8() const;      

      /** 
       * @return true if this set contains the C3G Max23 correlator.
       */
      bool includesC3gMax23() const;

      /** Add the spectral correlator to this set */
      void addSpectral();

      /** Add the wideband correlator to this set */
      void addWideband();

      /** Add the C3GMAX8 correlator to this set */
      void addC3gMax8();

      /** Add the C3GMAX23 correlator to this set */
      void addC3gMax23();

      /** 
       * @return true if this set includes the specified correlator set
       * @param corr correlator designation enum
       */
      bool includes(const carma::util::CorrelatorType corr) const;

      /** 
       * @return true if this set includes the specified correlator set
       * @param set another CorrelatorSet instance
       */
      bool includes(const CorrelatorSet& set) const;


      /** 
       * @return true if the passed correlator is the first one (in
       * bit order) in the set
       * @param corr correlator designation enum
       */
      bool firstCorrelatorIs(carma::util::CorrelatorType corr) const;

      
      /** @return the number of correlators contained in this set */
      unsigned nCorrelator() const;

      
      /** @return vector of correlator Designations that this set includes */
      std::vector<carma::util::CorrelatorType> getControlCorrelatorDesignations() const;

      /** @return correlator designation enum for this set */
      carma::util::CorrelatorType getControlCorrelatorDesignation() const;

      /** @return correlator designation enum of the 
       * first (in bit order) correlator in this set 
       */
      carma::util::CorrelatorType getFirstControlCorrelatorDesignation() const;

       /** Assignment operator 
       * @param corr correlator designation enum
       */ 
      void operator=(carma::util::CorrelatorType corr);

      /** Equality operator 
       * @param corr correlator designation enum
       */
      bool operator==(carma::util::CorrelatorType corr);

      /** Equality operators
       * @param set another CorrelatorSet instance
       */
      bool operator==(const CorrelatorSet & set) const; // Strict equality

      // Print operators

      /** output stream operator */
      friend std::ostream& operator<<(std::ostream& os, const CorrelatorSet& set);




    private:

      carma::util::CorrelatorType corrSet_;

    }; // End class CorrelatorSet

    std::ostream& operator<<(std::ostream& os, const CorrelatorSet& set);

  } // End namespace util
} // End namespace carma



#endif // End #ifndef CARMA_UTIL_CORRELATORSET_H
