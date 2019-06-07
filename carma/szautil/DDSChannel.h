#ifndef SZA_UTIL_DDSCHANNEL_H
#define SZA_UTIL_DDSCHANNEL_H

/**
 * @file DDSChannel.h
 * 
 * Tagged: Thu Aug 19 16:17:07 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>

namespace sza {
  namespace util {
    
    class DDSChannel {
    public:
      
      enum Id {
	DDSNONE = 0x0,
	DDS0    = 0x1,      // Valid DDS channels -- these are numbered 0-23 in the firmware
	DDS1    = 0x2, 
	DDS2    = 0x4,
	DDS3    = 0x8,
	DDS4    = 0x10,
	DDS5    = 0x20,
	DDS6    = 0x40,
	DDS7    = 0x80,
	DDS8    = 0x100,
	DDS9    = 0x200,   
	DDS10   = 0x400,   
	DDS11   = 0x800,   
	DDS12   = 0x1000,  
	DDS13   = 0x2000,  
	DDS14   = 0x4000,  
	DDS15   = 0x8000,  
	DDS16   = 0x10000, 
	DDS17   = 0x20000, 
	DDS18   = 0x40000, 
	DDS19   = 0x80000, 
	DDS20   = 0x100000,
	DDS21   = 0x200000,
	DDS22   = 0x400000,
	DDSMAX  = DDS22,
	DDSALL  = DDS0|DDS1|DDS2|DDS3|DDS4|DDS5|DDS6|DDS7|DDS8|DDS9|DDS10|DDS11|DDS12|DDS13|DDS14|DDS15|DDS16|DDS17|DDS18|DDS19|DDS20|DDS21|DDS22
      };
      
      
      // Constructor
      
      DDSChannel(Id id) {
	id_ = id;
      }
      
      DDSChannel() {
	id_ = DDSNONE;
      }
      
      void setDDSId(Id id) {
	id_ = id;
      }
      
      // Convert from DDS Id to integer index expected by the lobe rotator
      
      static unsigned DDSToLRInt(Id id);
      unsigned LRInt();
      
      // Convert from integer index expected by the lobe rotatorDDS Id to 
      
      static Id LRIntToDDSId(unsigned intId);
      
      // Return true if this is a valid single DDS channel
      
      static bool isValidSingleChannel(Id id);
      bool isValidSingleChannel();
      
      // Return true, if this is a valid channel set
      
      static bool isValidChannelSet(DDSChannel::Id channels);
      
      /**
       * Convert from integer index to enumerator
       */
      static Id intToId(unsigned int iant);
      
      /**
       * Convert from integer index to enumerator
       */
      static unsigned idToInt(DDSChannel::Id id);
      
      /**
       * Return the integer DDS Channel Id associated with this enumerator.
       */
      static unsigned int getIntId(Id id);
      unsigned int getIntId();
      
      // Returna a string version of the DDS channels
      
      static std::string printChannels(Id id);
      std::string printChannels();
      
      //------------------------------------------------------------
      // Member operators
      //------------------------------------------------------------
      
      /**
       * Add two antenna enumerators
       */
      const DDSChannel operator+(const DDSChannel& channel);
      
      /**
       * Define < for two antenna enumerators
       */
      bool operator<(const DDSChannel channel);
      
      /**
       * Define <= for two antenna enumerators
       */
      bool operator<=(const DDSChannel channel);
      
      /**
       * Define > for two antenna enumerators
       */
      bool operator>(const DDSChannel channel);
      
      /**
       * Define >= for two antenna enumerators
       */
      bool operator>=(const DDSChannel channel);
      
      /**
       * Define equality for two antenna enumerators
       */
      bool operator==(const DDSChannel channel);
      
      /**
       * Prefix increment
       */
      const DDSChannel& operator++();
      
      /**
       * Postfix increment
       */
      const DDSChannel operator++(int);
      
      /**
       * Return true if the passed id is part of this object's DDS channel
       * set.  
       */
      bool isSet(unsigned id);
      bool isSet(DDSChannel::Id id);
      bool isSet(DDSChannel& dds);
      
      /**
       * Return the integer DDS Channel Id associated with this enumerator.
       */
      Id getId();
      
      //------------------------------------------------------------
      // Non-member operator methods
      //------------------------------------------------------------
      
      /**
       * Allows cout << ant
       */
      friend std::ostream& operator<<(std::ostream& os, const DDSChannel& rx);
      
      /**
       * Allows expressions like DDS0+DDS1
       */
      friend DDSChannel::Id operator+(const DDSChannel::Id id1, 
				      const DDSChannel::Id id2);
      
      /**
       * Allows expressions like DDSALL-DDS1
       */
      friend DDSChannel::Id operator-(const DDSChannel::Id id1, 
				      const DDSChannel::Id id2);
      
      /**
       * Set the id of this antenna enumerator.
       */
      void setId(Id id);
      
    private:
      
      Id id_;
      
    }; // End class DDSChannel
    
    /**
     * Allows cout << ant
     */
    std::ostream& operator<<(std::ostream& os, const DDSChannel& rx);
    
    /**
     * Allows expressions like DDS0+DDS1
     */
    DDSChannel::Id operator+(const DDSChannel::Id id1, 
			     const DDSChannel::Id id2);
    
    /**
     * Allows expressions like DDSALL-DDS1
     */
    DDSChannel::Id operator-(const DDSChannel::Id id1, 
			     const DDSChannel::Id id2);
    
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DDSCHANNEL_H
