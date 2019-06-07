#ifndef SZA_UTIL_ANTNUM_H
#define SZA_UTIL_ANTNUM_H

/**
 * @file AntNum.h
 * 
 * Tagged: Fri Nov 14 12:39:31 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <iostream>
#include <sstream>
#include <string>

namespace sza {
  namespace util {
    
    /**
     * A class to enumerate a single Antenna, or a set of Antennas.
     */
    class AntNum {
      
      //------------------------------------------------------------
      // Public members
      //------------------------------------------------------------
      
    public:
      
      static const unsigned int NANT = 8;
      static const unsigned int NBASE = (NANT*(NANT-1))/2;
      
      /**
       * Enumerate known receivers
       */
      enum Id {
	ANTNONE = 0, // Make these orthogonal bits, so we can OR them
		     // together, as below.
	ANT0    = 1,
	ANT1    = 2,
	ANT2    = 4,
	ANT3    = 8,
	ANT4    = 16,
	ANT5    = 32,
	ANT6    = 64,
	ANT7    = 128,
	ANTMAX  = ANT7, // This should always be set to the last valid antenna
	SPARE   = 256,
	ANTALL  = ANT0 | ANT1 | ANT2 | ANT3 | ANT4 | ANT5 | ANT6 | ANT7
      } id_;
      
      /**
       * Constructor with Antenna enumerator
       */
      AntNum(Id id);
      
      /**
       * Constructor with Antenna number as int
       *
       * @throws Exception
       */
      AntNum(unsigned int);
      
      /**
       * Constructor with uninitialized antenna
       */
      AntNum();
      
      /**
       * Copy constructor
       *
       * @throws Exception
       */
      AntNum(const AntNum& antNum);
      
      /**
       * Copy constructor
       *
       * @throws Exception
       */
      AntNum(AntNum* antNum);
      
      /**
       * Check if this object specifies a valid single receiver
       */
      bool isValidSingleAnt();
      
      static bool isValidSingleAnt(Id antId);
      
      /**
       * Return a CORBA object name constructed from this Antenna
       * enumerator
       */ 
      std::string getObjectName();
      
      /**
       * Return a CORBA object name constructed from this Antenna
       * enumerator
       */ 
      std::string getEventChannelName();
      
      /**
       * Return a board name in the register database constructed
       * from this Antenna enumerator
       */ 
      std::string getAntennaName();
      
      /**
       * Return a string representation of this antenna set
       */ 
      std::string getString();
      
      /**
       * Return a prefix suitable for use in logging
       */ 
      std::string getLoggerPrefix();
      
      /**
       * Return an integer antenna index associated with this
       * enumerator, as expected by the delay engine.
       *
       * @throws Exception if this enumerator does not represent
       * a single valid antenna.
       */
      unsigned int getDelayEngineIntId();
      
      /**
       * Return an integer antenna index associated with this
       * enumerator.
       *
       * @throws Exception if this enumerator does not represent
       * a single valid antenna.
       */
      unsigned int getIntId();
      
      /**
       * Return the antenna id associated with this
       * enumerator.
       */
      Id getId();
      
      /**
       * Return an integer antenna index suitable for passing to
       * downconverter API methods.
       *
       * @throws Exception if this enumerator does not represent
       * a single valid antenna.
       */
      unsigned short getDcAntennaIndex();
      unsigned short getDcNodeIndex();
      unsigned short getCarmaAntennaIndex();
      
      /**
       * Return the maximum number of antennas we know about
       */
      unsigned int getAntMax();
      
      /**
       * Set the antennas represented by this object
       */
      void set(AntNum::Id id);
      
      /**
       * Return true if the passed id is part of this object's antenna
       * set.  
       */
      bool isSet(unsigned id);
      
      /**
       * Return true if the passed id is part of this object's antenna
       * set.  
       */
      bool isSet(AntNum::Id id);
      
      /**
       * Return true if the passed AntNum's id is part of this object's antenna
       * set.  
       */
      bool isSet(AntNum& antNum);
      
      /**
       * Return true if the passed AntNum's id is part of this object's antenna
       * set.  
       */
      bool isSet(AntNum* antNum);
      
      /**
       * Set the id of this antenna enumerator.
       */
      void setId(AntNum::Id);
      
      /**
       * Set the id of this antenna enumerator.
       */
      void setId(unsigned int id);
      
      /**
       * Set the id of this antenna enumerator.
       */
      void setId(const AntNum& antNum);
      
      /**
       * Set the id of this antenna enumerator from the name of the
       * host machine
       */ 
      void setIdFromHost();
      
      /**
       * Convert from integer index to enumerator
       */
      static AntNum::Id intToId(unsigned int iant);
      
      /**
       * Convert from integer index to enumerator
       */
      static unsigned idToInt(AntNum::Id id);
      
      /**
       * Return a string representing an antenna set.
       */
      std::string printAntennaSet();
      std::string printAntennaSet(AntNum::Id id);
      
      //------------------------------------------------------------
      // Member operators
      //------------------------------------------------------------
      
      /**
       * Add two antenna enumerators
       */
      const AntNum operator+(const AntNum& rx);
      
      /**
       * Define < for two antenna enumerators
       */
      bool operator<(const AntNum rx);
      
      /**
       * Define <= for two antenna enumerators
       */
      bool operator<=(const AntNum rx);
      
      /**
       * Define > for two antenna enumerators
       */
      bool operator>(const AntNum rx);
      
      /**
       * Define >= for two antenna enumerators
       */
      bool operator>=(const AntNum rx);
      
      /**
       * Define equality for two antenna enumerators
       */
      bool operator==(const AntNum rx);
      
      /**
       * Prefix increment
       */
      const AntNum& operator++();
      
      /**
       * Postfix increment
       */
      const AntNum operator++(int);
      
      //------------------------------------------------------------
      // Non-member operator methods
      //------------------------------------------------------------
      
      /**
       * Allows cout << ant
       */
      friend std::ostream& operator<<(std::ostream& os, const AntNum& rx);
      
      /**
       * Allows expressions like ANT0+ANT1
       */
      friend AntNum::Id operator+(const AntNum::Id id1, 
				  const AntNum::Id id2);
      
      /**
       * Allows expressions like ANTALL-ANT1
       */
      friend AntNum::Id operator-(const AntNum::Id id1, 
				  const AntNum::Id id2);
      
      /**
       * Return true if the passed index specifies a valid receiver
       */
      friend bool isValidAnt(unsigned int ant);
      
      /**
       * Return true if the passed index specifies a valid antenna set.
       */
      friend bool isValidAntennaSet(AntNum::Id antennas);
      
      /**
       * Return a string representing an antenna set.
       */
      friend std::string printAntennaSet(AntNum::Id antennas);
      
      /**
       * Convert from integer index to enumerator
       */
      friend AntNum::Id intToId(unsigned int iant);
      
      /**
       * Convert from integer index to enumerator
       */
      friend unsigned idToInt(AntNum::Id id);
      
      //------------------------------------------------------------
      // Private members
      //------------------------------------------------------------
      
    private:
      
      /**
       * Check for consistency between AntNum::ANTMAX and NANT
       */
      void checkMaxAnt();
      
    }; // End class AntNum
    
    bool isValidAnt(unsigned int ant);
    
    /**
     * Allows cout << ant
     */
    std::ostream& operator<<(std::ostream& os, const AntNum& rx);
    
    /**
     * Allows expressions like ANT0+ANT1
     */
    AntNum::Id operator+(const AntNum::Id id1, 
				const AntNum::Id id2);
    
    /**
     * Allows expressions like ANTALL-ANT1
     */
    AntNum::Id operator-(const AntNum::Id id1, 
				const AntNum::Id id2);
    
    /**
     * Return true if the passed index specifies a valid receiver
     */
    bool isValidAnt(unsigned int ant);
    
    /**
     * Return true if the passed index specifies a valid antenna set.
     */
    bool isValidAntennaSet(AntNum::Id antennas);
    
    /**
     * Return a string representing an antenna set.
     */
    std::string printAntennaSet(AntNum::Id antennas);
    
    /**
     * Convert from integer index to enumerator
     */
    AntNum::Id intToId(unsigned int iant);
    
    /**
     * Convert from integer index to enumerator
     */
    unsigned idToInt(AntNum::Id id);
    
  }; // End namespace util
}; // End namespace sza

#endif
