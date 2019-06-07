// $Id: CarmaConfig.h,v 1.4 2012/06/27 18:05:09 eml Exp $

#ifndef SZA_UTIL_CARMACONFIG_H
#define SZA_UTIL_CARMACONFIG_H

/**
 * @file CarmaConfig.h
 * 
 * Tagged: Thu Aug 28 15:18:31 PDT 2008
 * 
 * @version: $Revision: 1.4 $, $Date: 2012/06/27 18:05:09 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/AntennaType.h"
#include "carma/szautil/ArrayConfig.h"
#include "carma/szautil/Length.h"
#include "carma/szautil/Mutex.h"
#include "carma/szautil/Percent.h"

#include <iostream>
#include <string>
#include <vector>

#ifdef CARMA
#undef CARMA
#endif

namespace sza {
  namespace util {

    class CarmaConfig {
    public:

      //------------------------------------------------------------
      // Struct to encapsulate an antenna
      //------------------------------------------------------------

      struct Antenna {

	int antNumber_;             // The number of the antenna
				    // associated with this pad
	sza::util::Length diameter_;
	sza::util::Length sweptVolumeDiameter_;
	sza::util::Length elAxisHeight_;
	sza::util::Length cylinderDiameter_;

	unsigned antFlag_;
	bool tracking_;
	unsigned subarrayNumber_;

	Antenna() {
	  antNumber_ = -1;
	  tracking_ = false;
	  subarrayNumber_ = 0;
	}

	Antenna(const Antenna& location) {
	  *this = location;
	}

        Antenna(Antenna& location) {
	  *this = location;
	}

	void operator=(const Antenna& location) {
	  *this = (Antenna&) location;
	}


	void operator=(Antenna& ant) {
	  antNumber_           = ant.antNumber_;
	  diameter_            = ant.diameter_;
	  sweptVolumeDiameter_ = ant.sweptVolumeDiameter_;
	  elAxisHeight_        = ant.elAxisHeight_;
	  cylinderDiameter_    = ant.cylinderDiameter_;
	  antFlag_             = ant.antFlag_;
	  tracking_            = ant.tracking_;
	  subarrayNumber_      = ant.subarrayNumber_;
	}
	
	sza::util::AntennaType::Type getAntType();

	// Write the contents of this object to an ostream

	std::string antType();

	// Allows cout << Antenna

	friend std::ostream& operator<<(std::ostream& os, Antenna& pad);
      };

      //------------------------------------------------------------
      // Flags to indicate which antennas occupy a pad in which
      // configuration
      //------------------------------------------------------------

      enum Config {
	NONE   =   0x0,
	UNKNOWN=   0x0,

	A_OVRO =   0x1,
	A_BIMA =   0x2,

	A      = A_OVRO | A_BIMA,

	B_OVRO =   0x4,
	B_BIMA =   0x8,

	B      = B_OVRO | B_BIMA,

	C_OVRO =  0x10,
	C_BIMA =  0x20,

	C      = C_OVRO | C_BIMA,

	D_OVRO =  0x40,
	D_BIMA =  0x80,

	D      = D_OVRO | D_BIMA,

	E_OVRO = 0x100,
	E_BIMA = 0x200,

	E      = E_OVRO | E_BIMA,

	H_SZA  =  0x400,
	L_SZA  =  0x800,
	I_SZA  = 0x1000,
	BP_SZA = 0x2000,
	AP_SZA = 0x4000,

	H      = H_SZA,
	L      = L_SZA,
	I      = I_SZA,
	BP     = BP_SZA,
	AP     = AP_SZA,

	BO_OVRO =  0x8000,
	BO_BIMA = 0x10000,

	BO      = BO_OVRO | BO_BIMA,

	CO_OVRO =    0x20000,
	CO_BIMA =    0x40000,

	CO      = CO_OVRO | CO_BIMA,

	DO_OVRO =    0x80000,
	DO_BIMA =   0x100000,

	DO      = DO_OVRO | DO_BIMA,

	ANY = A|B|C|D|E|H|L|I|BO|CO|DO,

	BIMA  = A_BIMA|B_BIMA|C_BIMA|D_BIMA|E_BIMA|BO_BIMA|CO_BIMA|DO_BIMA,
	OVRO  = A_OVRO|B_OVRO|C_OVRO|D_OVRO|E_OVRO|BO_OVRO|CO_OVRO|DO_OVRO,
	CARMA = BIMA|OVRO,
	SZA   = H_SZA|L_SZA|I_SZA|BP_SZA|AP_SZA
      };

      //-----------------------------------------------------------------------
      // Struct to encapsulate a pad location
      //-----------------------------------------------------------------------

      struct PadLocation {

	unsigned flags_;    // A bitmask of antennas that occupy this
			    // pad in each configuration
	unsigned padNumber_;// The CARMA pad number (NB: these are NOT
			    // consecutive!)
	sza::util::Length east_;  // The east location of this pad
				  // (relative to pad 32)
	sza::util::Length north_; // The north location of this pad
				  // (relative to pad 32)
	sza::util::Length up_;    // The vertical location of this pad
				  // (relative to pad 32)
	
	CarmaConfig::Antenna ant_;// The antenna currently located on this pad

	bool validity_;           // Utility to store validity
				  // information about this pad -- not
				  // used for anything at the moment,
				  // outside of plotting on
				  // ConfigPlotter

	PadLocation() {};
	virtual ~PadLocation() {};

	PadLocation(unsigned padNumber, 
		    double eastInMeters, double northInMeters, 
		    double upInMeters, 
		    unsigned occupationFlags) {

	  padNumber_ = padNumber;
	  east_.setMeters(eastInMeters);
	  north_.setMeters(northInMeters);
	  up_.setMeters(upInMeters);
	  flags_ = occupationFlags;
	  validity_ = true;
	}

	PadLocation(const PadLocation& location) {
	  *this = location;
	}

        PadLocation(PadLocation& location) {
	  *this = location;
	}

	void operator=(const PadLocation& location) {
	  *this = (PadLocation&) location;
	}

	void operator=(PadLocation& location) {
	  east_      = location.east_;
	  north_     = location.north_;
	  up_        = location.up_;
	  flags_     = location.flags_;
	  padNumber_ = location.padNumber_;
	  ant_       = location.ant_;
	  validity_  = location.validity_;
	}

	// Allows cout << PadLocation

	friend std::ostream& operator<<(std::ostream& os, PadLocation& pad);

	// Return true if this antenna is shadowed by the passed ant,
	// in the current pointing position
	
	bool isShadowed(Angle& az, Angle& el, PadLocation& pad);

	// More general method that allows for shadowing percentages
	// to be specified

	bool isShadowed(Angle& az0, Angle& el0, PadLocation& pad, 
			bool useSweptVolumeDiameter,
			Percent& percent, bool isPercentOfDiameter, bool verbose=false);

	double shadowedArea(double radiusShadowedAnt, double radiusShadowingAnt, double sep);
  
	std::string listConfigs();

	void addConf(std::ostringstream& os, bool& prev, 
		     unsigned flags, unsigned conf);
      };

      // Constructor.

      CarmaConfig();

      // Destructor.

      virtual ~CarmaConfig();

      CarmaConfig(const CarmaConfig& conf);
      CarmaConfig(CarmaConfig& conf);
      void operator=(const CarmaConfig& conf);
      void operator=(CarmaConfig& conf);

      //-----------------------------------------------------------------------
      // Utility functions
      //-----------------------------------------------------------------------

      // Return true if padNumber is occupied for the configuration
      // specified
      
      bool isOccupied(unsigned padNumber, unsigned configFlag);

      // Return an Antenna struct populated with the correct
      // information for this antenna type

      static Antenna getAntennaInfo(unsigned configFlag);
      
      // Associate a pad number with an antenna number in the passed
      // configuration of pads

      void associatePadAndAntenna(std::vector<PadLocation>& pads, 
				  unsigned padNumber, unsigned antNumber);

      // Same as above, but for current configuration

      void associatePadAndAntenna(unsigned padNumber, unsigned antNumber);

      //-----------------------------------------------------------------------
      // Return known configurations
      //-----------------------------------------------------------------------

      std::vector<PadLocation> getConfiguration(unsigned flag);

      std::vector<PadLocation> getConfiguration(std::string name);

      // Return known SZA configurations

      std::vector<PadLocation> getSzaImagingConfiguration();
      std::vector<PadLocation> getSzaLowDecConfiguration();
      std::vector<PadLocation> getSzaHighDecConfiguration();
      std::vector<PadLocation> getSzaBPConfiguration();
      std::vector<PadLocation> getSzaAPConfiguration();

      // Associate pads with antenna numbers for known SZA
      // configurations

      void associateSzaImagingConfiguration(std::vector<PadLocation>& pads);
      void associateSzaLowDecConfiguration(std::vector<PadLocation>& pads);
      void associateSzaHighDecConfiguration(std::vector<PadLocation>& pads);
      void associateSzaAPConfiguration(std::vector<PadLocation>& pads);
      void associateSzaBPConfiguration(std::vector<PadLocation>& pads);

      // Sort a configuration by antenna number

      std::vector<PadLocation> sortByAntNumber(std::vector<PadLocation>& pads);
      
      //-----------------------------------------------------------------------
      // Create an arbitrary array out of existing pad locations
      //-----------------------------------------------------------------------

      void initializeCurrentConfiguration();
      void setCurrentConfiguration(unsigned flag);
      void setCurrentConfiguration(std::string name);
      void setCurrentConfiguration(ArrayConfig::Type type);
      void addPad(unsigned padNumber, unsigned antType, int antNumber=-1);
      void removePad(unsigned padNumber);
      void addPad(std::vector<PadLocation>& pads);
      void removePad(std::vector<PadLocation>& pads);
      std::vector<PadLocation> getCurrentConfiguration();
      std::vector<PadLocation> getAllPads();

      static unsigned confNameToFlag(std::string name);
      static unsigned confTypeToFlag(ArrayConfig::Type type);
      static std::string confFlagToName(unsigned flag);
      static ArrayConfig::Type confFlagToType(unsigned flag);

      std::string confName() {
	return confFlagToName(currentConfFlag_);
      }

      ArrayConfig::Type confType() {
	return confFlagToType(currentConfFlag_);
      }
      
      Mutex guard_;
      unsigned currentConfFlag_;

      PadLocation findNearest(Length east, Length north);

    private:

      bool padLabels_;

      PadLocation getPadByNumber(unsigned padNumber);

      void initializeCarmaPadLocations();

      // The full array of known pad locations

      std::vector<PadLocation> padLocations_;

      // A user-defined configuration, built out of known pad
      // locations

    public:
      std::vector<PadLocation> currentConfiguration_;

    }; // End class CarmaConfig

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CARMACONFIG_H
