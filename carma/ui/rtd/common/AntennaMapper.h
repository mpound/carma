// $Id: AntennaMapper.h,v 1.1 2013/11/05 00:20:29 eml Exp $

#ifndef CARMA_UI_RTD_ANTENNAMAPPER_H
#define CARMA_UI_RTD_ANTENNAMAPPER_H

/**
 * @file AntennaMapper.h
 * 
 * Tagged: Tue Sep 24 14:08:19 PDT 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/11/05 00:20:29 $
 * 
 * @author username: Command not found.
 */
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/SignalPathSubsystem.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/Frequency.h"
#include "carma/szautil/Length.h"

#include <map>
#include <vector>

namespace carma {
  namespace ui {
    namespace rtd {

      // Class to maintain a mapping of Antennas and subarrays

      class AntennaMapper {
      public:

	enum AntennaType {
	  ANT_UNKNOWN = 0x0,
	  ANT_OVRO    = 0x1,
	  ANT_BIMA    = 0x2,
	  ANT_SZA     = 0x4
	};

	// A class for managing a single antenna

	class Antenna {

	public:

	  AntennaType type_;
	  unsigned carmaAntNo_;
	  unsigned typeAntNo_;
	  unsigned subarrayNo_;
	  sza::util::Frequency frequency_;
	  sza::util::Length diameter_;

	  std::vector<Antenna*>* antennasBySubarray_;
	  std::vector<Antenna*>* antennasByType_;

	  Antenna(unsigned carmaAntNo) {

	    carmaAntNo_ = carmaAntNo;
	    subarrayNo_ = 0;

	    if(carmaAntNo_ < 7) {
	      type_ = ANT_OVRO;
	      typeAntNo_ = carmaAntNo_;
	      diameter_.setMeters(10.4);
	    } else if(carmaAntNo_ < 16) {
	      type_ = ANT_BIMA;
	      typeAntNo_ = carmaAntNo_ - 6;
	      diameter_.setMeters(6.1);
	    } else {
	      type_ = ANT_SZA;
	      typeAntNo_ = carmaAntNo_ - 15;
	      diameter_.setMeters(3.5);
	    }
	  };
	  
	  ~Antenna() {};

	  sza::util::Angle beamFwhm(sza::util::Frequency freq) {
	    sza::util::Angle fwhm;
	    fwhm.setRadians(1.2 * freq.centimeters() / diameter_.centimeters());
	    return fwhm;
	  };

	};

	/**
	 * Constructor.
	 */
	AntennaMapper(carma::monitor::CarmaMonitorSystem& cms);

	/**
	 * Destructor.
	 */
	virtual ~AntennaMapper();

	void rebuildSubarrayMaps();
	void initializeTypeMaps();
	bool mapsNeedRebuilding();
	void printSubarrayMaps();

	Antenna* getAntenna(AntennaType type, unsigned iAnt);

	friend std::ostream& operator<<(std::ostream& os, AntennaType& type);

      public:

	carma::monitor::SignalPathSubsystem::Mapping* mappingMs_;
	carma::monitor::CarmaMonitorSystem* cms_;

	std::vector<Antenna*> antennas_;
	static const unsigned nAntenna_;
	static const unsigned nOvro_;
	static const unsigned nBima_;
	static const unsigned nSza_;

	std::vector<Antenna*> subarray0Map_;
	std::vector<Antenna*> subarray1Map_;
	std::vector<Antenna*> subarray2Map_;
	std::vector<Antenna*> subarray3Map_;
	std::vector<Antenna*> subarray4Map_;
	std::vector<Antenna*> subarray5Map_;

	std::vector<Antenna*> szaMap_;
	std::vector<Antenna*> bimaMap_;
	std::vector<Antenna*> ovroMap_;
	
	std::map<unsigned,    std::vector<Antenna*>* > antennaMapsBySubarray_;
	std::map<AntennaType, std::vector<Antenna*>* > antennaMapsByType_;

      }; // End class AntennaMapper

      std::ostream& operator<<(std::ostream& os, AntennaMapper::AntennaType& type);
	
    } // End namespace rtd
  } // End namespace ui
} // End namespace carma



#endif // End #ifndef CARMA_UI_RTD_ANTENNAMAPPER_H
