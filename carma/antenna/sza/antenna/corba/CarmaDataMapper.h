// $Id: CarmaDataMapper.h,v 1.2 2010/12/13 20:52:26 eml Exp $

#ifndef SZA_ANTENNA_CORBA_CARMADATAMAPPER_H
#define SZA_ANTENNA_CORBA_CARMADATAMAPPER_H

/**
 * @file CarmaDataMapper.h
 * 
 * Tagged: Mon Sep 14 11:27:13 PDT 2009
 * 
 * @version: $Revision: 1.2 $, $Date: 2010/12/13 20:52:26 $
 * 
 * @author username: Command not found.
 */
#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/regtemplate.h"

#include "carma/szautil/String.h"

#include <map>

namespace sza {
  namespace util {
    class ArrayDataFrameManager;
  }
}

namespace sza {
  namespace antenna {
    namespace corba {

      struct SzaRegister {
	ArrRegMap* arrRegMap_;
	RegMapBlock* block_;
	int index_;
	
	SzaRegister() {
	  arrRegMap_ = 0;
	  block_     = 0;
	  index_     = -1;
	}
	
	SzaRegister(const SzaRegister& reg) {
	  *this = reg;
	}
	
	SzaRegister(SzaRegister& reg) {
	  *this = reg;
	}
	
	void operator=(const SzaRegister& reg) {
	  *this = (SzaRegister&)reg;
	}
	  
	  
	void operator=(SzaRegister& reg) {
	  arrRegMap_ = reg.arrRegMap_;
	  block_     = reg.block_;
	  index_     = reg.index_;
	}
	    
        friend std::ostream& operator<<(std::ostream& os, SzaRegister& reg);
      };

      class CarmaDataMapper {
      public:

	/**
	 * Constructor.
	 */
	CarmaDataMapper();

	/**
	 * Destructor.
	 */
	virtual ~CarmaDataMapper();

	static const unsigned nSlAnt_;
	static const unsigned nSlBase_;
	static const unsigned nBandMax_;
	static const unsigned nChan_;
	static const unsigned nSlInput_;
	static const unsigned nWbInput_;
	static const unsigned nAntTotal_;

	ArrayTemplate* getCarmaTemplate();
	ArrayMap* newCarmaArrayMap(void);
	void printArrayMap();
	std::map<std::string, std::string> getCarmaToSzaMap();
	void constructCarmaToSzaMap();
	void constructCarmaToTagIdMap(std::string file);
	std::map<unsigned, SzaRegister> getTagIdToSzaRegisterMap(sza::util::ArrayDataFrameManager* fm);

	void addIndexedRegs(sza::util::ArrayDataFrameManager* fm,
			    std::map<unsigned, SzaRegister>& retMap);

	void addFullyQualifiedRegs(sza::util::ArrayDataFrameManager* fm,
				   std::map<unsigned, SzaRegister>& retMap);

	void addPartiallyQualifiedRegs(sza::util::ArrayDataFrameManager* fm,
				       std::map<unsigned, SzaRegister>& retMap);
	
	void addTemplatizedRegs(sza::util::ArrayDataFrameManager* fm,
				std::map<unsigned, SzaRegister>& retMap);

	void addBlock(std::string carmaName, std::string szaName, unsigned index,
		      sza::util::ArrayDataFrameManager* fm,
		      std::map<unsigned, SzaRegister>& retMap);

	void getRanges(std::string str, bool& oneRange, 
		       sza::util::String& pref1, unsigned& start1, unsigned& stop1, 
		       sza::util::String& pref2, unsigned& start2, unsigned& stop2, 
		       sza::util::String& remainder);

      public:

	std::map<std::string, std::string> carmaToSzaMap_;

	bool tagIdToCarmaMapInitialized_;
	std::map<unsigned, std::string> tagIdToCarmaMap_;
	std::map<std::string, unsigned> carmaToTagIdMap_;

	// Antenna template

	static RegBlockTemp carmaCommonDrive_[];
	static RegBlockTemp carmaCommonLimit_[];
	static RegBlockTemp carmaCommonLo_[];
	static RegBlockTemp carmaCommonLocation_[];
	static RegBlockTemp carmaCommonPoint_[];
	static RegBlockTemp carmaCommonTrack_[];
	static RegBlockTemp carmaCommonRx_[];
	static RegBlockTemp carmaAntennaIf_[];

	static RegBlockTemp szaTracker_[];
	static RegBlockTemp szaCaltert_[];
	static RegBlockTemp szaThermal_[];
	static RegBlockTemp szaIfmod_[];
	static RegBlockTemp szaYig_[];
	static RegBlockTemp szaVaractor_[];
	static RegBlockTemp szaRx_[];
	static RegBlockTemp szaPmac_[];

	static RegBoardTemp szaAntennaBoards_[];
	static RegBoardTemp bimaAntennaBoards_[];
	static RegBoardTemp ovroAntennaBoards_[];

	static RegTemplate szaAntennaTemplate_;
	static RegTemplate bimaAntennaTemplate_;
	static RegTemplate ovroAntennaTemplate_;

	// Array Info template

	static RegBlockTemp carmaInfo_[];
	static RegBlockTemp carmaWeather_[];
	static RegBlockTemp carmaDelays_[];

	static RegBoardTemp carmaArrayBoards_[];
	static RegTemplate  carmaArrayTemplate_;

	// Sldc template

	static RegBlockTemp carmaSldcBand_[];
	static RegBoardTemp carmaSldcBoards_[];
	static RegTemplate  carmaSldcTemplate_;

	// Wbdc template

	static RegBlockTemp carmaWbdcBand_[];
	static RegBoardTemp carmaWbdcBoards_[];
	static RegTemplate  carmaWbdcTemplate_;

	// Correlator template

	static RegBlockTemp carmaCorrBand_[];
	static RegBlockTemp carmaCorrInfo_[];
	static RegBoardTemp carmaCorrBoards_[];
	static RegTemplate  carmaCorrTemplate_;
	
	// Subarray template

	static RegBlockTemp carmaSubarrayInfo_[];
	static RegBoardTemp carmaSubarrayBoards_[];
	static RegTemplate  carmaSubarrayTemplate_;

	// AntennaInfo template

	static RegBlockTemp carmaAntennaInfo_[];
	static RegBoardTemp carmaControlBoards_[];
	static RegTemplate  carmaControlTemplate_;

	// The whole thing

	static RegTemp carmaRegTemplates_[];
	static ArrayTemplate carmaTemplate_;

      }; // End class CarmaDataMapper

      std::ostream& operator<<(std::ostream& os, SzaRegister& reg);

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CARMADATAMAPPER_H
