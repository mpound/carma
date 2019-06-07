#ifndef SZA_UTIL_CORRELATORBAND_H
#define SZA_UTIL_CORRELATORBAND_H

/**
 * @file CorrelatorBand.h
 * 
 * Started: Mon Jan 12 13:58:55 UTC 2004
 * 
 * @author Erik Leitch
 */
#include <iostream>
#include <sstream>
#include <string>

#include "carma/szautil/Directives.h"
#include "carma/szautil/Frequency.h"

namespace sza {
  namespace util {
    
    /**
     * A class to enumerate a single Band, or a set of Bands.
     */
    class CorrelatorBand {
      
      //------------------------------------------------------------
      // Public members
      //------------------------------------------------------------
      
    public:
      
      // The total number of 500 MHz bands
      
      static const unsigned int NBAND = 16;
      
      // Each 500 MHz band is correlated in NCHAN channels.  The lag
      // correlator actually produces 17 channels, but the end points
      // are aliased.  We will pack only the central N-2 bands
      
      static const unsigned int NCHAN_TOTAL = 17;
      
      /**
       * Enumerate known bands
       */
      enum Id {
	BANDNONE = 0x0, // Make these orthogonal bits, so we can OR
			// them together.
	BAND0    = 0x1,
	BAND1    = 0x2,
	BAND2    = 0x4,
	BAND3    = 0x8,
	BAND4    = 0x10,
	BAND5    = 0x20,
	BAND6    = 0x40,
	BAND7    = 0x80,
	BAND8    = 0x100,
	BAND9    = 0x200,
	BAND10   = 0x400,
	BAND11   = 0x800,
	BAND12   = 0x1000,
	BAND13   = 0x2000,
	BAND14   = 0x4000,
	BAND15   = 0x8000,
	BANDMAX  = BAND15, // This should always be set to the last valid band
	BANDALL  = 0xFFFF,
      } id_;
      
      /**
       * Constructor with Band enumerator
       */
      CorrelatorBand(Id id);
      
      /**
       * Constructor with Band number as int
       *
       * @throws Exception
       */
      CorrelatorBand(unsigned int);
      
      /**
       * Constructor with uninitialized band
       */
      CorrelatorBand();
      
      /**
       * Copy constructor
       *
       * @throws Exception
       */
      CorrelatorBand(const CorrelatorBand& band);
      
      /**
       * Check if this object specifies a valid single band
       */
      bool isValidSingleBand();
      
      /**
       * Return string representing this band.
       */ 
      std::string bandName();
      static std::string bandName(unsigned iBand);
      
      /**
       * Set the id of this antenna enumerator.
       */
      void setId(CorrelatorBand::Id id);
      
      /**
       * Return an integer band index associated with this
       * enumerator.
       *
       * @throws Exception if this enumerator does not represent
       * a single valid band.
       */
      unsigned int getIntId();
      
      /**
       * Return an integer band index suitable for passing to the
       * downconverter API
       */
      unsigned short getDcBandIndex();
      unsigned short getDcNodeIndex();
      
      /**
       * Return the maximum number of bands we know about
       */
      unsigned int getBandMax();
      
      /**
       * Return true if the passed is is part of this object's band
       * set.  
       */
      bool isSet(CorrelatorBand::Id id);
      
      /**
       * Return true if the passed is is part of this object's band
       * set.  
       */
      bool isSet(CorrelatorBand& band);
      
      //------------------------------------------------------------
      // Member operators
      //------------------------------------------------------------
      
      /**
       * Add two band enumerators
       */
      const CorrelatorBand operator+(const CorrelatorBand& band);
      
      /**
       * Define < for two band enumerators
       */
      bool operator<(const CorrelatorBand band);
      
      /**
       * Define <= for two band enumerators
       */
      bool operator<=(const CorrelatorBand band);
      
      /**
       * Define > for two band enumerators
       */
      bool operator>(const CorrelatorBand band);
      
      /**
       * Define >= for two band enumerators
       */
      bool operator>=(const CorrelatorBand band);
      
      /**
       * Define equality for two band enumerators
       */
      bool operator==(const CorrelatorBand band);
      
      /**
       * Prefix increment
       */
      const CorrelatorBand& operator++();
      
      /**
       * Postfix increment
       */
      const CorrelatorBand operator++(int);
      
      //------------------------------------------------------------
      // Non-member operator methods
      //------------------------------------------------------------
      
      /**
       * Allows cout << band
       */
      friend std::ostream& operator<<(std::ostream& os, const CorrelatorBand& band);
      
      /**
       * Allows os << band
       */
      friend std::ostringstream& operator<<(std::ostringstream& os, 
					    const CorrelatorBand& band);
      /**
       * Allows expressions like BAND0+BAND1
       */
      friend CorrelatorBand::Id operator+(const CorrelatorBand::Id id1, 
					  const CorrelatorBand::Id id2);
      
      /**.......................................................................
       * Return the bandwidth
       */
      static Frequency bandWidth();
      
      /**.......................................................................
       * Return the bandwidth per channel
       */
      static Frequency bandWidthPerChannel();
      
      //------------------------------------------------------------
      // Private members
      //------------------------------------------------------------
      
    private:
      
      // The bandwidth of a band
      
      static const Frequency bandWidth_;
      
      // The bandwidth of a single channel
      
      static const Frequency bandWidthPerChannel_;
      
      /**
       * Return true if the passed index specifies a valid band
       */
      bool isValid(unsigned int band);
      
      /**
       * Convert from integer index to enumerator
       */
      Id intToId(unsigned int iband);
      
      /**
       * Check for consistency between CorrelatorBand::BANDMAX and NBAND
       */
      void checkMaxBand();
      
    }; // End class CorrelatorBand
    
    /**
     * Allows cout << band
     */
    std::ostream& operator<<(std::ostream& os, const CorrelatorBand& band);
    
    /**
     * Allows os << band
     */
    std::ostringstream& operator<<(std::ostringstream& os, 
				   const CorrelatorBand& band);
    /**
     * Allows expressions like BAND0+BAND1
     */
    CorrelatorBand::Id operator+(const CorrelatorBand::Id id1, 
				 const CorrelatorBand::Id id2);
    
  }; // End namespace util
}; // End namespace sza

#endif

