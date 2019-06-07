#ifndef SZA_UTIL_CENTER_H
#define SZA_UTIL_CENTER_H

/**
 * @file Center.h
 * 
 * Tagged: Tue Apr 27 13:16:50 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Source.h"

#include "carma/szaarrayutils/source.h"

namespace sza {
  namespace util {
    
    class Center {
    public:
      
      /**
       * Constructor.
       */
      Center(sza::array::SourceId srcId,
	     AntNum::Id antennas=AntNum::ANTNONE);
      
      /**
       * Destructor.
       */
      virtual ~Center();
      
      /**
       * Add antennas from the set associated with this center.
       */
      void addAntennas(AntNum::Id);

      /**
       * Remove antennas from the set associated with this center.
       */
      void removeAntennas(AntNum::Id);

      /**
       * Return the set of antennas associated with this pointing
       * center.
       */
      AntNum::Id getAntennas();

      /**
       * Remove antennas from the set associated with this center.
       */
      bool isEmpty();

      /**
       * Return true if a set of antnenas are asociated with this
       * pointing center.
       */
      bool isSet(AntNum::Id antennas);

      /**
       * Return the name of this pointing center
       */
      std::string getName();

      /**
       * Return the catalog number of this pointing center
       */
      unsigned getCatalogNumber();

      /*
       * Return the type of this pointing center
       */
      sza::array::SourceType getType();

      /**
       * Return a pointer to the ephemeris cache of this pointing center
       */
      double* getEphemerisCache();

      /**
       * Return a pointer to the id of this pointing center
       */
      sza::array::SourceId* getSourceId();

      /** 
       * Return a pointer to the window over which the last cached
       * values are valid.
       */
      sza::array::CacheWindow* getWindow();

      /**
       * Return a pointer to this object's source
       */
      sza::util::Source* getSource();

    private:

      sza::array::SourceId srcId_;
      AntNum::Id antennas_;

      // Quadratic interpolation cache for ephemeris sources

      double ephem_[3];

      // A struct for specifying the time window over which the last
      // cached ephemeris values are valid.

      sza::array::CacheWindow window_;

      // An object for managing source ephemerides.
      
      sza::util::Source source_;

    }; // End class Center
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CENTER_H
