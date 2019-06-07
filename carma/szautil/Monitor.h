#ifndef SZA_UTIL_MONITOR_H
#define SZA_UTIL_MONITOR_H

/**
 * @file Monitor.h
 * 
 * Tagged: Mon Apr 19 21:15:22 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <map>
#include <string>
#include <vector>

#include "carma/szaarrayutils/monitor_stream.h"

#include "carma/szautil/DataType.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/RegDescription.h"
#include "carma/szautil/MonitorDataType.h"

namespace sza {
  namespace util {
    
    class Monitor {
    public:
      
      /**
       * Constructor. Generic.
       */
      Monitor(std::string arcDir, std::string calFile, std::string host,
	      std::string start, bool startWasSet, 
	      std::string stop,  bool stopWasSet,
	      std::string regFile);
      
      /**
       * Constructor for just reading from the archive
       */
      Monitor(std::string arcDir, std::string calFile,
	      std::string startMjd, std::string stopMjd);
      
      /**
       * Constructor for just reading from the real-time controller
       */
      Monitor(std::string host, std::string calFile);

      /**
       * Destructor.
       */
      virtual ~Monitor();
      
      /**
       * Add a register to the set of registers to be monitored
       */
      void addRegister(std::string regmapName, 
		       std::string boardName, 
		       std::string regname, 
		       RegAspect aspect=REG_PLAIN, 
		       MonitorDataType::FormatType=MonitorDataType::FM_UNKNOWN, 
		       char* formatString=0,
		       MonitorDataType::FormatType=MonitorDataType::FM_UNKNOWN, 
		       CoordRange* range=0);

      /**
       * Parse a register specification string
       */
      void addRegister(std::string regSpec);

      /**
       * Read the next frame of data from the stream.
       */
      sza::array::MsReadState readNextFrame();

      /**
       * Methods for extracting the data into arrays of different data
       * types
       */
      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       unsigned* data, CoordRange* range);

      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       int* data, CoordRange* range);

      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       unsigned long* data, CoordRange* range);

      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       long* data, CoordRange* range);

      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       float* data, CoordRange* range);

      void getRegister(std::string regmapName, std::string boardName, 
		       std::string regname,
		       double* data, CoordRange* range);

      void printDoubleRegs();
      void printUnsignedIntRegs();

      /**
       * Method to sequentially read all data frames and return the value of
       * selected registers according to type.
       */
      void readRegs();

      unsigned countFrames();

      /**
       * Method to sequentially read all data frames and return the value of
       * selected registers as doubles.
       */
      std::vector<std::vector<double> > readRegsAsDoubles();
      std::vector<std::vector<std::vector<MonitorDataType> > > readRegsAsDataTypes();

      /**
       * Print the laatest values of all selected regs, according to
       * type.
       */
      void printRegs();
      void printRegs2();

      /**
       * Get the vector of laatest values of all selected regs, cast
       * as doubles.
       */
      std::vector<double> getRegsAsDoubles();
      std::vector<std::vector<MonitorDataType> > getRegsAsDataTypes();
      
      std::vector<sza::util::RegDescription> selectedRegs() {
	return selectedRegs_;
      }

      std::vector<sza::util::MonitorDataType::FormatType> selectedFormats() {
	return selectedFormats_;
      }

      std::vector<sza::util::MonitorDataType::FormatType> nativeFormats() {
	return nativeFormats_;
      }

      /**
       * Return a register value as a double
       */
      double getRegAsDouble(MonitorDataType val, 
			    RegAspect aspect);

      /**
       * Run this object in interactive mode.
       */
      void run();
      void runTest();

      /**
       * Return a reference to the calibrated cata for the last read
       * frame
       */
      double* getCalSlotPtr(unsigned iSlot=0);
	
      void reinitialize();

    private:

      enum KeywordType {
	ADD_REGISTER,
	READ,
	UNKNOWN
      };

      struct Keyword {
	std::string keyword;
	KeywordType type;
      };

      static Keyword keywords[];
      static int nKey_;

      struct Format {
	std::string format;
	MonitorDataType::FormatType type;
      };

      static Monitor::Format formats[];
      static int nFormat_;

      enum StreamType {
	MS_FILE = 0x1,
	MS_NET  = 0x2,
	MS_BOTH = MS_FILE | MS_NET
      };

      Monitor::StreamType type_;

      OutputStream* outputStream_;
      char fmtString_[100];

      sza::array::MonitorStream* ms_;

      std::map<std::string, std::map<std::string, std::map<std::string, RegDescription* > > > regMap_;

      std::map<std::string, std::map<std::string, std::map<std::string, MonitorDataType::FormatType > > > regSelFormat_;

      std::map<std::string, std::map<std::string, std::map<std::string, MonitorDataType::FormatType > > > regNatFormat_;

      std::map<std::string, std::map<std::string, std::map<std::string, RegAspect > > > regAspect_;

      std::vector<RegDescription> selectedRegs_;
      std::vector<RegAxisRange> regAxisRanges_;
      std::vector<RegAspect> aspects_;
      std::vector<MonitorDataType::FormatType> selectedFormats_;
      std::vector<MonitorDataType::FormatType> nativeFormats_;
      std::vector<MonitorSelection> selections_;
      std::vector<std::string> formatStrings_;
      std::vector<bool> hasFormatString_;

      std::string calFile_;
      std::string host_;
      std::string regFile_;
      bool readRegSpecFromFile_;

      bool initialized_;

      bool hybrid_;

      /**
       * Constructor. Generic.
       */
      void privateConstructor(std::string arcDir, std::string calFile, 
			      std::string host,
			      std::string start, bool startWasSet, 
			      std::string stop,  bool stopWasSet,
			      std::string regFile);
      
      void setMonitoringInterval(unsigned interval);

      void sendMonitoringInterval();

      void sendRegisterSelection();

      void loadCalFile();

      void changeMonitorStream();

      /**
       * Parse a date string.
       */
      double parseDateAndTime(std::string utc);

      /**
       * Read a keyword from an input stream
       */
      KeywordType readKeyword(InputStream* stream);

      /**
       * Read a format from a stream
       */
      MonitorDataType::FormatType readFormat(InputStream* stream);
      char* readFormatString(InputStream* stream);

      /**
       * Read to the end of the current line, and consume any terminators
       */
      void skipToNextLine(InputStream* stream);

      /**
       * Add a register to the set of registers to be monitored.  In this
       * case the register specification is read from an input stream.
       */
      void addRegister(RegDescription& desc, MonitorDataType::FormatType format, 
		       char* formatString);

      /**.......................................................................
       * Add a vector of registers
       */
      void addRegisters(std::vector<RegDescription>& regs, 
			MonitorDataType::FormatType format,
			char* formatString);

      /**
       * Add a register to the set of registers to be monitored.  In this
       * case the register specification is read from an input stream.
       */
      void addRegister(InputStream* stream);

      void getRegister(RegDescription* desc,
		       MonitorDataType* data, CoordRange* range);

      void getSingleRegisterVal(RegDescription* desc,
				MonitorDataType* data, CoordRange* range);

      /**
       * Fill a vector with registers values from the latest frame
       */
      void getRegisterVals(std::vector<MonitorDataType>& data,
			   RegDescription* desc, 
			   MonitorDataType* val,
			   RegAxisRange& regAxisRange);

    public:
      static MonitorDataType::FormatType 
	formatOf(RegDescription& reg, 
		 MonitorDataType::FormatType format=MonitorDataType::FM_UNKNOWN);


    private:
      std::string formatStringOf(MonitorDataType::FormatType format,
				 RegAspect aspect);

      void outputReg(MonitorDataType val, RegAspect aspect, char* formatPtr);
      MonitorDataType formatReg(MonitorDataType val, RegAspect aspect, std::string& formatStr);

      /**
       * Format a vector of registers
       */
      void formatRegs(std::vector<MonitorDataType>& vals, 
		      MonitorDataType& val,
		      RegAspect aspect, std::string& formatStr);

      /**
       * Convert a date to the appropriate type
       */
      void convertDateToType(MonitorDataType& val);
      
      /**
       * Convert any aspect of a complex float to the appropriate type
       */
      void convertComplexFloatToType(MonitorDataType& val, RegAspect aspect);

      void doubleToType(double dVal, MonitorDataType& val);

    }; // End class Monitor
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MONITOR_H
