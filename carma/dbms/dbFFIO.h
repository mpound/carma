#ifndef carma_monitor_dbFFIO_h
#define carma_monitor_dbFFIO_h
/**
 * @file
 * Class(es) to read & write dbms flat file information in ASCII or binary.
 */

#include <string.h>
#include <vector>
#include <fstream>
#include <complex>
#include <iostream>
#include <stdio.h>
#include <string>

namespace carma {
  namespace dbms {

class dbFFIO {
 public:
  dbFFIO();
  virtual ~dbFFIO();

// Write the flat file header:
//  <length of header in bytes> framecount signature
// Binary files don't use header length. 
  virtual void writeFFHeader(const long frameCount, const std::string &sig)=0;

  // Write instantaneous average to file.
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, short v,
				      int iSample)=0;
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, int v,
				      int iSample)=0;
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, long v,
				      int iSample)=0;
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, float v,
				      int iSample) = 0;
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, double v,
				      int iSample) = 0;
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity,
				      const std::complex<float> &v,
				      int iSample)=0;
  // Strings don't have iSamples, but it's easier to provide the argument.
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity,
				      const std::string &v, int dmy=0)=0;

  /////////////////////   Reads  ///////////////////////////////////////
  // Returns values obtained when file was opened.
  bool readFFHeader(long &frameCount, std::string &sig);

  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      short &v, int &iSample)=0;
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      int &v, int &iSample)=0;

  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      long &v, int &iSample)=0;
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      float &v, int &iSample)=0;
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      double &v, int &iSample)=0;
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      std::complex<float> &v, int &iSample)=0;
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      std::string &v)=0;

////////////////////////////////////////////////////////////////
//		Write instantaneous average to file.
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     short v,
				     short maxValue, short minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     int v,
				     int maxValue, int minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     long v,
				     long maxValue, long minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, float v,
				     float maxValue, float minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, double v,
				     double maxValue, double minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     const std::complex<float> &v,
				     const std::complex<float> maxValue,
				     const std::complex<float> minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples)=0;
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     const std::string &v,
				     short nValidSamples, int nTotalSamples)=0;

  /////////////////////   Reads  ///////////////////////////////////////


  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       short &v,
				       short &maxValue, short &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       int &v,
				       int &maxValue, int &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       long &v,
				       long &maxValue, long &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       float &v,
				       float &maxValue, float &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       double &v,
				       double &maxValue, double &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       std::complex<float> &maxValue,
				       std::complex<float> &minValue,
				       std::complex<float> &v,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       std::string &v,
 				       short &nValidSamples,
				       int &nTotalSamples)=0;

////////////////////////////////////////////////////////////////
  // Types of records that are known.
  enum RECORD_TYPE { RECORD_UNKNOWN=-1, RECORD_SHORT, RECORD_INTEGER,
		     RECORD_LONG, RECORD_FLOAT, RECORD_DOUBLE,
		     RECORD_COMPLEX, RECORD_STRING
  };

  //////////////// File I/O  ////////////////
  virtual bool open(const std::string &fileName, bool isWrite)=0;
  bool open(const std::string &fileName, std::ios_base::openmode openmode);
  virtual bool close();
  virtual void flush();
  inline bool isWrite(){return isWrite_ && isOpen();}
  inline bool isRead(){return !isWrite_ && isOpen();}
  inline bool eof()const {return file_.eof();}
  bool isOpen(){return file_.is_open();}
  virtual bool getRecordInfo(RECORD_TYPE &mvt,
			     unsigned &recordCount)=0;
  static std::string valuetypeToString(int valuetype);
 protected:
  // Fill input buffer.
  virtual bool fillBuffer() = 0;
  // Write output buffer.
  virtual void writeBuffer() = 0;

 protected:
  std::string fileName_;	// Name of opened file.
  std::string askedFileName_;	// Name of file that was asked for.
  std::fstream file_;
  bool	 isWrite_;
  // Flat file header info.
  long	frameCount_;
  std::string sig_;
};

/*
 * The format of the ASCII files generated should be the same as those
 * produced by monitor/MonitorPointAccumulatorT.h and friends. The
 * formating statements were taken from the original code.
 * One difference is that a complete line is built before it is written.
 * This should help minimize partially written lines in the output file.
 */

class dbFFIOa : public dbFFIO {
 public:
  dbFFIOa();
  virtual ~dbFFIOa();

  virtual void writeFFHeader(const long frameCount, const std::string &sig);

  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, short v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, int v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, long v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, float v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity, double v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity,
				      const std::complex<float> &v,
				      int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
				      short blanking, short validity,
				      const std::string &v, int dmy=0);

  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      short &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      int &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      long &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      float &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      double &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      std::complex<float> &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
					short &blanking, short &validity,
					std::string &v);
////////////////////////////////////////////////////////////////
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, short v,
				     short maxValue, short minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, int v,
				     int maxValue, int minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, long v,
				     long maxValue, long minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, float v,
				     float maxValue, float minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity, double v,
				     double maxValue, double minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     const std::complex<float> &v,
				     const std::complex<float> maxValue,
				     const std::complex<float> minValue,
				     int iSample,
				     short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
				     short blanking, short validity,
				     const std::string &v,
				     short nValidSamples, int nTotalSamples);

  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       short &v,
				       short &maxValue, short &minValue,
				       int &iSample,
				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       int &v,
				       int &maxValue, int &minValue,
				       int &iSample,
				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       long &v,
				       long &maxValue, long &minValue,
				       int &iSample,
				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       float &v,
				       float &maxValue, float &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       double &v,
				       double &maxValue, double &minValue,
				       int &iSample,
 				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       std::complex<float> &v,
				       std::complex<float> &maxValue,
				       std::complex<float> &minValue,
				       int &iSample,
				       short &nValidSamples,
				       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       std::string &v,
				       short &nValidSamples,
				       int &nTotalSamples);
  ////////////////////////////////////////////////////////////////
  virtual bool open(const std::string &fileName, bool write);
  virtual bool close();
  virtual bool getRecordInfo(RECORD_TYPE &mvt,
			     unsigned &recordCount);
 protected:
  virtual bool fillBuffer();
  virtual bool checkBuffer();
  virtual void writeBuffer();
  // This is taken from util/FileUtils.
  unsigned int getMonitorDataFlatFileHeaderLength(const unsigned& sigLength) {
    return (19 + sigLength);}

  void buildRecordFormat(const long frameCount, const long tagID,
			 short blanking, short validity, const char *fmt);

  bool getAverageProps(long &frameCount, long &tagID,
		       short &blanking, short &validity);

  // Convert tokens_ array into values.
  bool tokenToValue(unsigned int index, short &v);
  bool tokenToValue(unsigned int index, int &v);
  bool tokenToValue(unsigned int index, long int &v);
  bool tokenToValue(unsigned int index, float &v);
  bool tokenToValue(unsigned int index, double &v);
  bool tokenToValue(unsigned int index, std::complex<float> &v);
  bool tokenToValue(unsigned int index, std::string &v);
  bool getFFHeader();
 private:
  char	format_[128];	// Holds the format string.
  char	lineBuffer_[512];
  std::vector<const char *> tokens_;	// Used to break apart an input line.
};

/*
 * Read/Write binary flat files.
 */
class dbFFIOb : public dbFFIO {
 public:
  dbFFIOb();
  virtual ~dbFFIOb();
  virtual bool open(const std::string &fileName, bool write);
  virtual bool close();
  // Returns current record type and # of records.
  // For input files, returns the number of records of the current type
  // left to be processed. If recordCount would be 0, the next record
  // header is read and that information is returned. If false is returned,
  // no more records can be read or the file type is ASCII and therefore
  // the information isn't available.
  virtual bool getRecordInfo(RECORD_TYPE &mvt,
			     unsigned &recordCount);

  //  This has to be the first thing written.
  virtual void writeFFHeader(const long frameCount, const std::string &sig);

  /** Convert a binary file to an ASCII file.
   * isInstant - true if instantaneous averages will be written, false for
   * long term.
   */
  static bool copyToASCII(const std::string &infileName, const
			  std::string &outfileName, bool isInstant);
  ////////////////////////////////////////////////////////////////
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity, short v,
			       int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity, int v,
			       int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity, long v,
			       int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       float v, int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       double v, int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       const std::complex<float> &v,
			       int iSample);
  virtual void dumpInstAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       const std::string &v, int dmy=0);

  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       short &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       int &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       long &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       float &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       double &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       std::complex<float> &v, int &iSample);
  virtual bool readInstAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       std::string &v);
  ////////////////////////////////////////////////////////////////
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity, short v,
			       short maxValue, short minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity, int v,
			       int maxValue, int minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity, long v,
			       long maxValue, long minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity, float v,
			       float maxValue, float minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity, double v,
			       double maxValue, double minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       const std::complex<float> &v,
			       const std::complex<float> maxValue,
			       const std::complex<float> minValue,
			       int iSample,
			       short nValidSamples, int nTotalSamples);
  virtual void dumpLongAverage(const long frameCount, const long tagID,
			       short blanking, short validity,
			       const std::string &v,
			       short nValidSamples, int nTotalSamples);

  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       short &v,
			       short &maxValue, short &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       int &v,
			       int &maxValue, int &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       long &v,
			       long &maxValue, long &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       float &v,
			       float &maxValue, float &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       double &v,
			       double &maxValue, double &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       std::complex<float> &v,
			       std::complex<float> &maxValue,
			       std::complex<float> &minValue,
			       int &iSample,
			       short &nValidSamples,
			       int &nTotalSamples);
  virtual bool readLongAverage(long &frameCount, long &tagID,
			       short &blanking, short &validity,
			       std::string &v,
			       short &nValidSamples,
			       int &nTotalSamples);
  ////////////////////////////////////////////////////////////////

 protected:
  static bool copyInstantAverages(dbFFIOb &in, dbFFIOa &out);
  static bool copyLongAverages(dbFFIOb &in, dbFFIOa &out);

  virtual bool fillBuffer();
  // Make sure there are size bytes available for packing or unpacking.
  virtual bool checkBuffer(RECORD_TYPE mvt, int size);
  virtual bool checkBuffer(int size);
  virtual void writeBuffer();
  inline void pack(short v){pack(v, &byteArray_, &offset_);}
  inline void pack(unsigned short v){pack(v, &byteArray_, &offset_);}
  inline void pack(int v){pack(v, &byteArray_, &offset_);}
  inline void pack(unsigned v){pack(v, &byteArray_, &offset_);}
  inline void pack(long v){pack(v, &byteArray_, &offset_);}
  inline void pack(float v){pack(v, &byteArray_, &offset_);}
  inline void pack(double v){pack(v, &byteArray_, &offset_);}
  inline void pack(const std::complex<float> &v)
    {pack(v, &byteArray_, &offset_);}
  inline void pack(const std::string &v){pack(v, &byteArray_, &offset_);}

  inline void unpack(short &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(unsigned short &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(int &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(unsigned &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(long &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(float &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(double &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(std::complex<float> &v){unpack(v, byteArray_, &offset_);}
  inline void unpack(std::string &v){unpack(v, byteArray_, &offset_);}

 public:
  // These routines were pretty much copied from util/Serializable.h.

      /**
       *  Convenience method for serializing an int into
       *  the byte array
       */
      static inline void pack(int tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(int)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing an unsigned into
       *  the byte array
       */
      static inline void pack(unsigned tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(int)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing a char into
       *  the byte array
       */
      static inline void pack(char tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(char)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      static inline void pack(const std::string &v,
			      std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(char)" << std::endl;
        int size = v.size() + 1;
        memcpy(&(*byteArray)[*offset], v.c_str(), size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing a bool into
       *  the byte array
       */
      static inline void pack(bool tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(bool)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing an short into
       *  the byte array
       */
      static inline void pack(short tmp, std::vector<char>* byteArray,
			      int* offset) {
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      static inline void pack(unsigned short tmp, std::vector<char>* byteArray,
			      int* offset)
     {
        //	  std::cout << "pack(short)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing a long into
       *  the byte array
       */
      static inline void pack(long tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(long)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing an float into
       *  the byte array
       */
      static inline void pack(float tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(float)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing an double into
       *  the byte array
       */
      static inline void pack(double tmp, std::vector<char>* byteArray,
			      int* offset) {
        //	  std::cout << "pack(double)" << std::endl;
        int size = sizeof(tmp);
        memcpy(&(*byteArray)[*offset], &tmp, size);
        *offset += size;
      }

      /**
       *  Convenience method for serializing a complex<float> into
       *  the byte array
       */
      static inline void pack(const std::complex<float>& tmpc,
                       std::vector<char>* byteArray,
                       int* offset) {
        //int start = byteArray_.size();
        int size = sizeof(std::complex<float>);
        int size2 = static_cast< int >(size * .5);
        float tmp = tmpc.real();
        memcpy(&(*byteArray)[*offset], &tmp, size2);
        *offset += size2;

        tmp = tmpc.imag();
        memcpy(&(*byteArray)[*offset], &tmp, size2);
        *offset += size2;
      }

      static inline void unpack(std::string &val,
                         const std::vector<char>& byteArray,
                         int* offset) {
	val = &byteArray[*offset];
        *offset += val.size() + 1;
      }

      /**
       *  Convenience method to unpack a byteArray into a bool
       */
      static inline void unpack(bool& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a short
       */
      static inline void unpack(short& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      static inline void unpack(unsigned short& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a int
       */
      static inline void unpack(int& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a unsigned
       */
      static inline void unpack(unsigned& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a long
       */
      static inline void unpack(long& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a float
       */
      static inline void unpack(float& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }


      /**
       *  Convenience method to unpack a byteArray into a double
       */
      static inline void unpack(double& val,
                         const std::vector<char>& byteArray,
                         int* offset) {
        memcpy(&val, &byteArray[*offset], sizeof(val));
        *offset += sizeof(val);
      }

      /**
       *  Convenience method to unpack a byteArray into a complex<float>
       */
      static inline void unpack(std::complex<float>& tmpv,
                         const std::vector<char>& byteArray,
                         int* offset) {
        int size = sizeof(std::complex<float>);
        int size2 = static_cast< int >(size * .5);
        float real;
        float imag;
        memcpy(&real, &byteArray[*offset], size2);
        *offset += size2;
        memcpy(&imag, &byteArray[*offset], size2);
        *offset += size2;

        tmpv = std::complex<float>(real, imag);
      }

      /**
       *  Convenience method to unpack byte array into a vector of shorts.
       *  The short vector must be sized properly before calling this
       *  method.
       */
      static inline void unpack(std::vector<short>& tmpv,
                         const std::vector<char>& byteArray,
                         int* offset) {
        int size = tmpv.size() * sizeof(short);
        memcpy(&tmpv[0], &byteArray[*offset], size);
        //for (unsigned int idx = 0; idx < tmpv.size(); ++idx)
        //  tmpv[idx] = ntohf(tmpv[idx]);
        *offset += size;
      }

      /**
       *  Convenience method to unpack byte array into a vector of intss.
       *  The int vector must be sized properly before calling this
       *  method.
       */
      static inline void unpack(std::vector<int>& tmpv,
                         const std::vector<char>& byteArray,
                         int* offset) {
        int size = tmpv.size() * sizeof(int);
        memcpy(&tmpv[0], &byteArray[*offset], size);
        //for (unsigned int idx = 0; idx < tmpv.size(); ++idx)
        //  tmpv[idx] = ntohf(tmpv[idx]);
        *offset += size;
      }


      /**
       *  Convenience method to unpack byte array into a vector of floats.
       *  The float vector must be sized properly before calling this
       *  method.
       */
      static inline void unpack(std::vector<float>& tmpv,
                         const std::vector<char>& byteArray,
                         int* offset) {
        int size = tmpv.size() * sizeof(float);
        memcpy(&tmpv[0], &byteArray[*offset], size);
        *offset += size;
      }

      /**
       *  Convenience method to unpack byte array into a vector of
       *  complex. The vector must be sized properly before calling this
       *  method.
       */
      static inline void unpack(std::vector<std::complex<float> >& tmpv,
                         const std::vector<char>& byteArray,
                         int* offset) {
        int length = tmpv.size();
        int size2 = sizeof(float);

        float real;
        float imag;
        for (int idx = 0; idx < length; ++idx) {
          memcpy(&real, &byteArray[*offset], size2);
          *offset += size2;
          memcpy(&imag, &byteArray[*offset], size2);
          *offset += size2;
          tmpv[idx] = std::complex<float>(real, imag);
        }
      }

 private:
      void initb();
      // Begin a series of records.
      bool startRecordGroup(int mvt);
      bool endRecordGroup();		// End a series of records.
      //      std::string typeToString(int)const;
      // Called to track # of records & bytes processed.
      void incRecordInfo(int size){recordCount_++;recordBytes_ += size;}
      void decRecordInfo(int size){recordCount_--;recordBytes_ -= size;}
 private:
      // Holds serialized values.
      std::vector<char> byteArray_;
      int offset_;	// Index into array of where next request goes.
      // # of bytes read by last read + # bytes already there.
      int bytesInBuffer_;
      std::ostream::pos_type recordStart_;// Place in file where record starts.
      int		recordType_;	// Type of record being read/written
      unsigned	recordCount_;	// Count of # of records of this type.
      unsigned	recordBytes_;	// # of bytes in this group of records.
};

  }
}

#endif
