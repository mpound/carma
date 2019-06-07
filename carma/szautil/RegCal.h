#ifndef SZA_UTIL_REGCAL_H
#define SZA_UTIL_REGCAL_H

/**
 * @file RegCal.h
 * 
 * Tagged: Fri Oct  1 22:25:34 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/ArrayMapBase.h"
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/RegDescription.h"
#include "carma/szautil/RegisterSet.h"

#include "carma/szaarrayutils/regset.h"
#include "carma/szaarrayutils/regdata.h"
#include "carma/szaarrayutils/input.h"

#include <string>
#include <vector>

namespace sza {
  namespace util {
    
    class MonitorDataType;
    
    /*
     * This module provides facilities for converting selected 32-bit int
     * archived data to physical double precision values. Calibration
     * offsets and scale factors to be applied during this process are
     * taken from a RegCal object. Such objects are initialized from text
     * input streams. In addition, integrated registers are divided by the
     * value of the frame.nsnap register to yield mean values per
     * snapshot. This requires that the frame.nsnap register slot contain
     * valid data (for this reason MonitorStream objects silently select
     * this register if it isn't selected for monitoring).
     *
     * Some archive values such as bit-masks are inherently 32 bit
     * integers and will presumably be promptly returned to that form.
     * Assuming that the calibration file doesn't specify a non-unity
     * scale factor or a non-zero offset, we can guarantee that the
     * conversion to and from a double is lossless because ANSI/ISO C
     * mandates that a double must be able to exactly represent any
     * number of at least 10 decimal digits.
     *
     * The calibration of scalar registers is implemented as:
     *
     *  reg[i] = offset + factor * reg[i].
     *
     * The calibration of complex registers is implemented as:
     *
     *  real = reg[i]
     *  imag = reg[i+1]
     *  reg[i]   = offset + factor * real;
     *  reg[i+1] = offset + factor * (imag/imag_gain + real*sin(phi))/cos(phi);
     */
    class RegCal {
    public:
      
      //------------------------------------------------------------
      // RegCal struct
      //------------------------------------------------------------
      
      /**
       * The following structure contains a double precision array having
       * the same dimension as an archive frame. calibrate_regdata()
       * returns its results in a container of this type.
       */
      struct RegCalData {
	unsigned nSlot_;            // The number of elements in slots_[] 
	std::vector<double> slots_; // The array of monitored registers 
	bool empty_;                // True until the first successful
	// call to calibrateRegData()
	
	inline bool isEmpty() {
	  return empty_;
	}
	
	// The following functions can be used to retrieve calibrated
	// data from a RegCalData array. The reg argument should be a
	// register specification that convers the register index
	// range index..index+n-1.
	
	void getCalChar(RegDescription* reg, char* data, CoordRange* range=0);
	void getCalUchar(RegDescription* reg, unsigned char* data, CoordRange* range=0);
	void getCalUshort(RegDescription* reg, unsigned short* data, CoordRange* range=0);
	void getCalShort(RegDescription* reg, short* data, CoordRange* range=0);
	void getCalUint(RegDescription* reg, unsigned* data, CoordRange* range=0);
	void getCalInt(RegDescription* reg, int* data, CoordRange* range=0);
	void getCalUlong(RegDescription* reg, unsigned long *data, CoordRange* range=0);
	void getCalLong(RegDescription* reg, long* data, CoordRange* range=0);
	void getCalFloat(RegDescription* reg, float* data, CoordRange* range=0);
	void getCalDouble(RegDescription* reg, double* data, CoordRange* range=0);
	void getCalDate(RegDescription* reg, sza::util::RegDate::Data* data, CoordRange* range=0);
	void getCalComplexFloat(RegDescription* reg, 
				sza::util::Complex<float>::Data* data, 
				CoordRange* range=0);
	
	void getCalFloat(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalDouble(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalUshort(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalShort(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalUint(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalInt(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalChar(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalUchar(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalUlong(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalLong(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalDate(RegDescription* reg, sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);
	void getCalComplexFloat(RegDescription* reg, sza::util::MonitorDataType* data, 
				sza::util::RegAxisRange& range);
	
	void getCalDouble(RegDescription* reg, double* data, RegAxisRange& iRange);
	
	void getCalString(RegDescription* reg, char *string, unsigned length);
	
	// An argument validation function used by the getCal_...()
	// functions.
	
	void checkCalArgs(std::string caller, RegDescription* reg, 
			  void* data, CoordRange* range=0);
	
	RegCalData(ArrayMap* arrayMap, bool archivedOnly=false);
	~RegCalData();
      };
      
      //------------------------------------------------------------
      // RegCalSlot struct
      //------------------------------------------------------------
      
      /**
       * The calibration of a scalar register is:
       *
       * reg[i] = offset + factor * reg[i].
       *
       * The calibration of a complex register is:
       *
       * real = reg[i]
       * imag = reg[i+1]
       * reg[i]   = offset + factor * real;
       * reg[i+1] = offset + factor * (imag/imag_gain + real*sin(phi))/cos(phi);
       */
      struct RegCalSlot {
	
	double offset_;         // The calibration offset of the
	// register
	double factor_;         // The calibration multiplier of the
	// register
	
	// The following are only relevant to complex registers 
	
	double imagGain_;       // The gain of the imaginary channel
	// wrt the real
	double sinPhi_,cosPhi_; // Sin and cos of the imaginary
	// channel phase offset
	
	// Constructor 
	
	RegCalSlot();
	
	// Destructor
	
	~RegCalSlot();
	
	// Reset members to default values
	
	void reset();
	
      };
      
      //------------------------------------------------------------
      // RegCal class methods
      //------------------------------------------------------------
      
      /**
       * Constructor.
       */
      RegCal(ArrayMap* arrayMap=NULL, bool archivedOnly=false);
      
      /**
       * Copy constructor
       */
      RegCal(RegCal& regCal);
      
      /**
       * Destructor.
       */
      virtual ~RegCal();
      
      /**
       * Load calibration parameters from a file. Note that this is just a
       * convenient wrapper around load_cal_stream() that creates a
       * temporary input stream and attaches it to the specified file.
       */
      void loadCalFile(std::string dir, std::string name, bool doThrow=true);
      
      /**
       * Load calibration parameters from an input stream.
       */
      void loadCalStream(InputStream *stream, bool doThrow=true);
      
      /*
       * Calibrate selected registers while copying them from a raw archive
       * array of 32-bit unsigned integers to a parallel double precision array.
       */
      void calibrateRegData(RegisterSet* registerSet,
			    RegRawData* raw);
      
      void calibrateRegData(RegisterSet& regset,
			    ArrayDataFrameManager& fm);
      
      void calibrateRegData(RegSet* registerSet,
			    ArrayDataFrameManager* fm);
      
      /*
       * Reset all calibration multipliers to 1.0 and zero all calibration
       * offsets.
       */
      void reset();
      
      /**
       * Print cal factors for the requested register
       */
      void printCalFactors(std::vector<RegDescription>& regs);
      
      /**
       * Get a pointer to the calibrated data
       */
      inline RegCal::RegCalData* calData() {
	return calData_;
      }
      
      // Raw access to the slot pointer

      double* getSlotPtr(unsigned iSlot);

      // Raw access to the calibration information

      RegCal::RegCalSlot getRegCalSlot(unsigned iSlot);

    private:
      
      // An array map
      
      ArrayMapBase arrayMapBase_;
      ArrayMap* arrayMap_;
      
      // True if this class is managing cal factors for archived
      // register only, false if for the whole array map
      
      bool archivedOnly_;
      
      unsigned int nSlot_; // The number of register calibration slots 
      std::vector<RegCalSlot> slots_; // An array of nslot sets of
      // calibration parameters
      RegCalData* calData_; // The calibrated register data
      int nsnapSlot_;       // The register slot of the nsnap register 
      int nsnapByteOffset_; // The byte offset in the array map of the
      // nsnap register
      
      /**
       * Read the calibration parameters for a given selection of register
       * elements.
       */
      void readCalGroup(InputStream* stream, std::vector<RegDescription>& regs);
      
      /**
       * Skip to the start of the next register name in a calibration stream,
       * ignoring intervening calibration parameters. This can be used to skip
       * an errant entry.
       */
      void skipCalGroup(InputStream* stream);
      
      /**
       * Read a single set of register calibration parameters and
       * record them as the calibration of a given range of registers.
       */
      void readCalRecord(RegMapBlock *block, 
			 InputStream *stream,
			 double pars[4]);
      
      /**
       * Install parameters read from a cal record as the calibation
       * factors for a range of slots
       */
      void installCalPars(double pars[4], RegMapBlock* blk, 
			  unsigned ia, unsigned ib);
      
    }; // End class RegCal
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REGCAL_H
