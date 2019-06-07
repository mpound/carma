// $Id: SzaMonitorSystemMap.h,v 1.5 2012/08/14 22:03:11 eml Exp $

#ifndef SZA_ANTENNA_CORBA_SZAMONITORSYSTEMMAP_H
#define SZA_ANTENNA_CORBA_SZAMONITORSYSTEMMAP_H

/**
 * @file SzaMonitorSystemMap.h
 * 
 * Tagged: Fri Apr 15 16:41:55 PDT 2011
 * 
 * @version: $Revision: 1.5 $, $Date: 2012/08/14 22:03:11 $
 * 
 * @author username: Command not found.
 */
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/MonitorPointIterator.h"
#include "carma/monitor/monitorPointSpecializations.h"

#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/arraymap.h"

#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/BitMask.h"
#include "carma/szautil/String.h"

#define MAX_CARMA_STRING_LENGTH 80
#define MP_PACK_FN(fn) void (fn)(carma::monitor::MonitorPoint* mp, unsigned nSamp, sza::util::ArrayDataFrameManager* dfm, \
				 ArrRegMap* aregmap, RegMapBlock* blk, void* ptr)

namespace sza {
  namespace antenna {
    namespace corba {

      class SzaMonitorSystemMap {
      public:

	//=======================================================================
	// Struct to encapsulate a single register (CARMA monitor point)
	//=======================================================================
	
	struct SzaBlkTemp {
	  SzaBlkTemp();
	  void packData(sza::util::BitMask& bitMask);
	  void addRegister(carma::monitor::MonitorPoint* mp);
	  
	  carma::monitor::MonitorPoint* mp_;
	  ArrRegMap* aregmap_;
	  RegMapBlock* blk_;
	  sza::util::ArrayDataFrameManager* dfm_;
	  unsigned nSamp_;
	  MP_PACK_FN(*packFn_);
	  void* ptr_;
	};

	//=======================================================================
	// Struct to encapsulate a board of registers (container of CARMA
	// monitor points)
	//=======================================================================
	
	struct SzaBrdTemp {
	  SzaBrdTemp();
	  virtual ~SzaBrdTemp();

	  // Method for adding a register associated with a CARMA monitor point

	  void addRegister(std::string blockName, carma::monitor::MonitorPoint* mp, ArrayMap* szaArrayMap=0);
	  
	  // Method for adding a register not associated with a CARMA monitor point
	  
	  void addRegister(std::string blockName, unsigned flags, unsigned nEl1, unsigned nEl2, std::string units="unknown");

	  unsigned nBlock_;
	  std::map<std::string, SzaBlkTemp*> blockMap_;
	  std::vector<RegBlockTemp> blockVec_;
	};

	//=======================================================================
	// Struct to encapsulate a group of boards (container of containers)
	//=======================================================================

	struct SzaRegmapTemp {

	  // The number of boards encountered

	  unsigned nBoard_;
	  std::map<std::string, SzaBrdTemp*> boardMap_;
  
	  SzaRegmapTemp();
	  virtual ~SzaRegmapTemp();
	  void addRegister(std::string boardName, std::string blockName, carma::monitor::MonitorPoint* mp, ArrayMap* szaArrayMap=0);

	  // Method for adding a register not associated with a CARMA monitor point

	  void addRegister(std::string boardName, std::string blockName, unsigned flags, 
			   unsigned nEl1, unsigned nEl2, std::string units="unknown");
	};

	//=======================================================================
	// Methods of SzaMonitorSystemMap itself
	//=======================================================================

	// True when this object has been initialized from the CARMA monitor
	// hierarchy

	bool initialized_;

	// The number of register maps in this array map

	unsigned nRegmap_;

	// The map of register maps encountered in the CARMA monitor stream

	std::map<std::string, SzaRegmapTemp*> regmapMap_;

	// The array template that corresponds to this hierarchy

	ArrayTemplate arrayTemplate_;
	bool arrayTemplateIsInitialized_;

	// An array map generated from arrayTemplate_

	ArrayMap* arrayMap_;
	bool registerPointersAreInitialized_;

	unsigned nSlAnt_;
	unsigned nSlBase_;
	unsigned iSlBandStart_;
	unsigned nSlBandMax_;

	unsigned nWbAnt_;
	unsigned nWbBase_;
	unsigned iWbBandStart_;
	unsigned nWbBandMax_;

	unsigned nChan_;

	unsigned nAntTotal_;
  
	ArrayMap* szaArrayMap_;

	// The total number of validity flags required to represent
	// validity states for all registers in this map.  Note that
	// this is not in general equal to the total number of
	// registers, since multi-element registers can have a single
	// validity flag (strings) or multiple validity flags.

	unsigned nValidityFlags_;
	unsigned nRegs_;

	sza::util::BitMask validityBitMask_;

	//------------------------------------------------------------
	// Methods of SzaMonitorSystemMap
	//------------------------------------------------------------

	SzaMonitorSystemMap();
	virtual ~SzaMonitorSystemMap();

	static RegMapBlock* getSzaBlock(ArrayMap* arrayMap, sza::util::String& regmapName, sza::util::String& boardName, 
					sza::util::String& blockName);

	static bool addSzaFlags(carma::monitor::MonitorPoint* mp, unsigned& flags, ArrayMap* szaArrayMap=0);
	static void addCarmaFlags(carma::monitor::MonitorPoint* mp, unsigned& flags);

	static void parseIntoRegisterStrings(carma::monitor::MonitorPoint* mp, 
					     sza::util::String& regmapName, 
					     sza::util::String& boardName, 
					     sza::util::String& blockName);

	void constructArrayMapFromCarmaMonitorSystem(std::vector<carma::monitor::MonitorPoint*>& mpVec);

	void addWbCorrelatorRegisters(bool coherenceMonitor=false);
	void addSlCorrelatorRegisters(bool coherenceMonitor=false);

	void addCorrelatorBoard(std::string regmapName, std::string boardName, unsigned nAnt, bool coherenceMonitor);

	ArrayMap* getArrayMap();
	ArrayTemplate* getArrayTemplate();
	ArrayTemplate* getFakeArrayTemplate();

	void print();
	void setupRegisterPointers(sza::util::ArrayDataFrameManager& dfm);
	void packData();
	unsigned initializeValidityBitMask();
	void initializeValidityBitIndices();
	unsigned char* getValidityPtr();

	static MP_PACK_FN(packBool);
	static MP_PACK_FN(packByte);
	static MP_PACK_FN(packShort);
	static MP_PACK_FN(packInt);
	static MP_PACK_FN(packFloat);
	static MP_PACK_FN(packComplexFloat);
	static MP_PACK_FN(packDouble);
	static MP_PACK_FN(packString);

	// Method for adding a register not associated with a CARMA monitor point

	void addRegister(std::string regmapName, std::string boardName, std::string blockName, unsigned flags, 
			 unsigned nEl1=1, unsigned nEl2=0, std::string units="unknown");

	unsigned longestStringLen_;

      private:
  
	void addRegister(std::string regmapName, std::string boardName, std::string blockName, 
			 carma::monitor::MonitorPoint* mp);

	void generateArrayTemplate();
	void generateArrayMap();

	static unsigned carmaMpToNel(carma::monitor::MonitorPoint* mp);
	static unsigned carmaMpToFlags(carma::monitor::MonitorPoint* mp, ArrayMap* szaArrayMap);

      }; // End class SzaMonitorSystemMap

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_SZAMONITORSYSTEMMAP_H
