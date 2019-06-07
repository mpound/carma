/**

Handle flat file I/O for the database for ASCII or binary files.

**/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <cerrno>
#include "carma/util/Logger.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/ErrorException.h"
#include "carma/dbms/dbFFIO.h"
using namespace std;
using namespace carma::dbms;

/**
 * 
 * Binary file format:
 *   File Header:
 *     frameCount	- long
 *     signature	- string
 *   Record header 1:
 *     record type	- int
 *     num records	- long
 *     num bytes	- long
 *    record 1
 *     framecount	- long
 *     ...
 *   Record header 2:
 * 
 *  Strings are stored as:
 * 	unsigned	string length including the trailing '\0'.
 * 	char	string including the trailing '\0'.
 * 
 * Values are packed via pack/unpacked routines into/from an array of bytes.
 * The buffer is written or read as necessary.
 */

static const int FILEMODE = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH;

dbFFIO::dbFFIO()
{
  fileName_ = "";
  askedFileName_ = "";
  isWrite_ = false;
}


dbFFIO::~dbFFIO()
{
  //  cout << "dbFFIO::~dbFFIO() called.\n";
	close();
}

// Open a file for read/write, ASCII or binary. If the file is opened for
// writing, they are then given the ".write" suffix and renamed to the
// requested name after they are closed.
//  Returns true if the open succeeded.
bool dbFFIO::open(const std::string &fileName,
		  std::ios_base::openmode openmode)
{
	if(fileName == "")
		return false;
	askedFileName_ = fileName;

	fileName_ = askedFileName_;

	isWrite_ = (openmode & std::ios_base::out);
	if(isWrite_)
	{ fileName_ += ".write";
	}

	file_.open(fileName_.c_str(), openmode);
	if(!file_)
	{      string errmsg = strerror(errno);
	  if(isWrite_)
	  	throw CARMA_ERROR("Unable to open file " + fileName
                          + " for writing: " + errmsg);
	  else
	  	throw CARMA_ERROR("Unable to open file " + fileName
                          + " for reading: " + errmsg);
	}
	if(isWrite_)
	  chmod(fileName.c_str(), FILEMODE);

	return isOpen();
}

// Returns 0 if close was successful. Closing a non-opened file is not
// an error.
bool dbFFIO::close()
{bool status = true;

	if(isOpen())
	{   int err = 0;
		file_.close();
		if(isWrite_)
		{ err = rename(fileName_.c_str(), askedFileName_.c_str());
		  err = chmod(askedFileName_.c_str(),FILEMODE);
		}
		//		cout << "Closed " << askedFileName_ << endl;
		status = (err == 0);
	}
	return status;
}

// Flush buffer.
void dbFFIO::flush()
{
	if(isWrite())
	{	writeBuffer();
		file_.flush();
	}
}

// Returns the frameCount and signature read in when file was opened.
bool dbFFIO::readFFHeader(long &frameCount, std::string &sig)
{
  // These are read in when the file is opened.
  frameCount = frameCount_;
  sig = sig_;
  return true;
}


//string dbFFIO::valuetypeToString(RECORD_TYPE valuetype) const
string dbFFIO::valuetypeToString(int valuetype)
{
  switch (valuetype) {
  case RECORD_SHORT:         return "SHORT";
  case RECORD_INTEGER:       return "INTEGER";
  case RECORD_LONG:          return "LONG";
  case RECORD_FLOAT:         return "FLOAT";
  case RECORD_DOUBLE:        return "DOUBLE";
  case RECORD_COMPLEX:       return "COMPLEX";
  case RECORD_STRING:        return "STRING";
  default:                 return "Unknown recordtype";
  }
  return "dbFFIO::valuetypeToString() - can't get here";
}

////////////////////////////////////////////////////////////////
////			ASCII file I/O
////////////////////////////////////////////////////////////////
dbFFIOa::dbFFIOa() : dbFFIO()
{
	lineBuffer_[0] = 0;
}

dbFFIOa::~dbFFIOa()
{
	close();
}

bool dbFFIOa::getRecordInfo(RECORD_TYPE &, unsigned &)
{
	// No information is available.
	return false;
}

// Write the flat file header:
//	<length of header in bytes> framecount signature
void dbFFIOa::writeFFHeader(const long frameCount, const std::string &sig)
{ int sigLength = sig.length();
  int headerLength = getMonitorDataFlatFileHeaderLength(sigLength);
  static const char *FFHEADERFORMATW = "%5d\t%11ld\t%*s\n";

	if(isWrite())
	{  sprintf(lineBuffer_, FFHEADERFORMATW, headerLength,
		   frameCount, sigLength, sig.c_str());
		writeBuffer();
		frameCount_ = frameCount;
		sig_ = sig;
	}
}

// Create the format string for a record.
// Since everything outputs frameCount, tagID, blanking and validity,
// they are converted here.
// fmt - is the format string used to convert the rest of the arguments.
// A newline is appended.
void dbFFIOa::buildRecordFormat(const long frameCount, const long tagID,
				short blanking, short validity, const char *fmt)
{static const char *RECORDFORMAT0 = "%11ld\t%11ld\t%2d\t%2d\t%s\n";

 	sprintf(format_, RECORDFORMAT0,
		frameCount,tagID, blanking, validity, fmt);
}

////			dump instantaneous averages to file	////

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, short v,
			      int iSample)
{static const char *SHORTFORMAT = "%7d\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, SHORTFORMAT);
		sprintf(lineBuffer_, format_, v, iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, int v,
			      int iSample)
{static const char *INTFORMAT = "%12d\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, INTFORMAT);
		sprintf(lineBuffer_, format_, v, iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, long v,
			      int iSample)
{static const char *LONGFORMAT = "%12ld\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, LONGFORMAT);
		sprintf(lineBuffer_, format_, v, iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, float v,
			      int iSample)
{static const char *FLOATFORMAT = "%15.8e\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, FLOATFORMAT);
		sprintf(lineBuffer_, format_, v, iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, double v,
			      int iSample)
{static const char *DOUBLEFORMAT = "%23.16e\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, DOUBLEFORMAT);
		sprintf(lineBuffer_, format_, v, iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::complex<float> &v,
			      int iSample)
{static const char *COMPLEXFORMAT = "%15.8e\t%15.8e\t%2d";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, COMPLEXFORMAT);
		sprintf(lineBuffer_, format_, v.real(), v.imag(), iSample);
		writeBuffer();
	}
}

void dbFFIOa::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::string &v, int
				      )
{static const char *STRINGFORMAT = "%s";

	if(isWrite())
	{	buildRecordFormat(frameCount, tagID, blanking,
				  validity, STRINGFORMAT);
		sprintf(lineBuffer_, format_, v.c_str());
		writeBuffer();
	}
}

////////////////     Read ////////////////////////////////////
// Read the flat file header and store results for readFFHeader().
bool dbFFIOa::getFFHeader()
{ int siglen = -1;
 
	// Read next line.
	checkBuffer();
	if(tokens_.size() >= 1)
		siglen = strtol(tokens_[0], 0, 0);
	if(tokens_.size() >= 2)
		frameCount_ = strtol(tokens_[1], 0, 0);
	if(tokens_.size() >= 3)
		sig_ = tokens_[2];
	return true;
}

////////////////////////////////////////////////////////////////
// 		Convert contents of tokens_ to values.
bool dbFFIOa::tokenToValue(unsigned index, short &v)
{ long vl;
  bool err = tokenToValue(index, vl); 
  v = vl;
  return err;
}

bool dbFFIOa::tokenToValue(unsigned int index, int &v)
{ long vl;
  bool err = tokenToValue(index, vl); 
  v = vl;
  return err;
}

bool dbFFIOa::tokenToValue(unsigned int index, long &v)
{char *endptr;
 const char *nptr;

  if(tokens_.size() <= index)
    return false;
  nptr = tokens_[index];
  v = strtol(nptr, &endptr, 0);
  // Assume entire string is used in conversion.
  if((*nptr != '\0') && (*endptr == '\0'))
    return true;
  else
    { cout << "Bad token string: " << nptr << " (" << endptr << ")\n";
    return false;
    }
}

bool dbFFIOa::tokenToValue(unsigned int index, float &v)
{char *endptr;
  if(tokens_.size() <= index)
    return false;
  v = strtof(tokens_[index], &endptr);
  return true;
}

bool dbFFIOa::tokenToValue(unsigned int index, double &v)
{ char *endptr;
  if(tokens_.size() <= index)
    return 0;
  v = strtod(tokens_[index], &endptr);
  return true;
}

bool dbFFIOa::tokenToValue(unsigned int index, complex<float> &v)
{float real, imag;

 if(!tokenToValue(index++, real))
   return false;
 if(!tokenToValue(index, imag))
   return false;

 v.real() = real;
 v.imag() = imag;
 return true;
}

bool dbFFIOa::tokenToValue(unsigned int index, string &v)
{
  if(tokens_.size() <= index)
    return 0;
  v = tokens_[index];
  return true;
}

////////////////////////////////////////////////////////////////
// Retrieve frameCount, tagID, blanking and validity.
bool dbFFIOa::getAverageProps(long &frameCount, long &tagID,
			      short &blanking, short &validity)
{
  if(!tokenToValue(0, frameCount))
    return false;
  if(!tokenToValue(1, tagID))
    return false;
  if(!tokenToValue(2, blanking))
    return false;
  if(!tokenToValue(3, validity))
    return false;
  return true;
}

////////////////////////////////////////////////////////////////
/// Read various types of values.


bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      short &v, int &iSample)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(5, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      int &v, int &iSample)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(5, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				      long &v, int &iSample)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(5, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      float &v, int &iSample)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(5, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      double &v, int &iSample)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(5, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::complex<float> &v, int &iSample)
{ bool status; 

	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	status = tokenToValue(4, v);
	if(status)
	  status = tokenToValue(6, iSample);
	return status;
}

bool dbFFIOa::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::string &v)
{
	checkBuffer();
	if(!getAverageProps(frameCount, tagID, blanking, validity))
	  return false;
	bool err = tokenToValue(4, v);
	return err;
}

////////////////////////////////////////////////////////////////
////				Long averages		////////

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, short v,
			      short maxValue, short minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, int v,
			      int maxValue, int minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, long v,
			      long maxValue, long minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, float v,
			      float maxValue, float minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, double v,
			      double maxValue, double minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::complex<float> &v,
			      const std::complex<float> maxValue,
			      const std::complex<float> minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOa::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::string &v,
			      short nValidSamples, int nTotalSamples)
{
}

////

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      short &v,
			      short &maxValue, short &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      int &v,
			      int &maxValue, int &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      long &v,
			      long &maxValue, long &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      float &v,
			      float &maxValue, float &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      double &v,
			      double &maxValue, double &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
				      short &blanking, short &validity,
				       std::complex<float> &v,
				       std::complex<float> &maxValue,
				       std::complex<float> &minValue,
				       int &iSample,
				       short &nValidSamples,
				       int &nTotalSamples)
{
  return true;
}

bool dbFFIOa::readLongAverage(long &frameCount, long &tagID,
				       short &blanking, short &validity,
				       std::string &v,
				       short &nValidSamples,
				       int &nTotalSamples)
{
  return true;
}


////////////////////////////////////////////////////////////////


bool dbFFIOa::open(const std::string &fileName, bool write)
{std::ios_base::openmode mode = write ? std::ios_base::out : std::ios_base::in;
 bool opened;

	opened = dbFFIO::open(fileName, mode);
	if(opened && isRead())
		getFFHeader();
	return opened;
}

bool dbFFIOa::close()
{
	if(isOpen())
	{	if(isWrite())
		{	flush();
		}
		dbFFIO::close();	// Check for error?
	}
	return true;
}

// Write line buffer to file.
void dbFFIOa::writeBuffer()
{
	if(isWrite())
	{
#if 1
		int nchars = strlen(lineBuffer_);
		if(nchars)
		{	file_.write(lineBuffer_, nchars);
		}
#else
		file_ << lineBuffer_;
#endif
		lineBuffer_[0] = 0;
    }
}

// Read a new line.
bool dbFFIOa::fillBuffer()
{
  if(isRead() && ! eof())
  {	file_.getline(lineBuffer_, sizeof(lineBuffer_));
	return true;
  }
  return false;
}

// Read a line, then tokenize it.
bool dbFFIOa::checkBuffer()
{char *cptr;
 const char *tptr; 

  if(isWrite())
    return true;
  if(!isOpen())
    return false;

  if(!fillBuffer())
    return false;

  tokens_.resize(0);	// The size of tokens_ gives the number of fields.

  tptr = strtok_r(lineBuffer_, "\t", &cptr);
  while(tptr)
  {	tokens_.push_back(tptr);
	tptr = strtok_r(0, "\t", &cptr);
  }

  return true;
}

////////////////////////////////////////////////////////////////

// Size of I/O buffer in 1024 byte chunks. It has to be at least as large
// as the largest record (20-24 bytes for frame/numeric) and should be able to
// hold a few thousand of them for efficient buffering.
static const int BUFSIZE = 100;
////

static const int CHUNKSIZE=1024; // (2 typical disk sectors).
static const int BUFFERSIZE=BUFSIZE*CHUNKSIZE;
 
dbFFIOb::dbFFIOb() : dbFFIO()
{
  /* Reserve a little more, just in case. */
  byteArray_.reserve(BUFFERSIZE+128);
  byteArray_.resize(BUFFERSIZE);

  initb();
}

dbFFIOb::~dbFFIOb()
{
	close();
}

// Write flat file header.
void dbFFIOb::writeFFHeader(const long frameCount, const std::string &sig)
{
	pack(frameCount);
	pack(sig);
	frameCount_ = frameCount;
	sig_ = sig;
}

////////////////////////////////////////////////////////////////
////			Instantaneous Averages		    ////
////////////////////////////////////////////////////////////////

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, short v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_SHORT, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, int v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_INTEGER, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, long v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_LONG, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, float v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_FLOAT, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity, double v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_DOUBLE, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::complex<float> &v,
			      int iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_COMPLEX, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(v);
	pack(iSample);
	incRecordInfo(size);
}

void dbFFIOb::dumpInstAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::string &v, int
			      )
{ static const int size0 = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
      + sizeof(validity) + sizeof(unsigned);
  int strsize = v.size() + 1; // Includes space for trailing '\0'
  int size = size0 + strsize;

	// Make sure there is enough space. 
	if(!checkBuffer(RECORD_STRING, size))
		return;
	pack(frameCount);
	pack(tagID);
	pack(blanking);
	pack(validity);
	pack(strsize);	// Length of string (incl '0').
	pack(v);
	incRecordInfo(size);
}
////////////////////   reads ///////////////////////////////////////////

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      short &v, int &iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_SHORT, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      int &v, int &iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_INTEGER, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      long &v, int &iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_LONG, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      float &v, int &iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_FLOAT, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      double &v, int &iSample)
{ static const int size = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
    + sizeof(validity) + sizeof(v) + sizeof(iSample);

	if(!checkBuffer(RECORD_DOUBLE, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::complex<float> &v, int &iSample)
{ static const int size = sizeof(frameCount)+ sizeof(tagID)+sizeof(blanking)
    + sizeof(validity)+sizeof(v)+sizeof(iSample);

	if(!checkBuffer(RECORD_COMPLEX, size))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(v);
	unpack(iSample);
	decRecordInfo(size);
  return true;
}

bool dbFFIOb::readInstAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::string &v)
{ unsigned strsize;
  static const int size0 = sizeof(frameCount)+sizeof(tagID)+sizeof(blanking)
   + sizeof(validity) + sizeof(strsize);
  int size = size0;
  bool status;

	if(!checkBuffer(RECORD_STRING, size0))
		return false;
	unpack(frameCount);
	unpack(tagID);
	unpack(blanking);
	unpack(validity);
	unpack(strsize);
	if(checkBuffer(strsize))
	{	unpack(v);
		size += strsize;
		status = true;
	}
	else
		status = false;
	decRecordInfo(size);

  return status;
}

////////////////////////////////////////////////////////////////
////			Long term averages		    ////
////////////////////////////////////////////////////////////////

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, short v,
			      short maxValue, short minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, int v,
			      int maxValue, int minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, long v,
			      long maxValue, long minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, float v,
			      float maxValue, float minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity, double v,
			      double maxValue, double minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}
void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::complex<float> &v,
			      const std::complex<float> maxValue,
			      const std::complex<float> minValue,
			      int iSample,
			      short nValidSamples, int nTotalSamples)
{
}

void dbFFIOb::dumpLongAverage(const long frameCount, const long tagID,
			      short blanking, short validity,
			      const std::string &v,
			      short nValidSamples, int nTotalSamples)
{
}


//////////////

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      short &v,
			      short &maxValue, short &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      int &v,
			      int &maxValue, int &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      long &v,
			      long &maxValue, long &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      float &v,
			      float &maxValue, float &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      double &v,
			      double &maxValue, double &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::complex<float> &v,
			      std::complex<float> &maxValue,
			      std::complex<float> &minValue,
			      int &iSample,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

bool dbFFIOb::readLongAverage(long &frameCount, long &tagID,
			      short &blanking, short &validity,
			      std::string &v,
			      short &nValidSamples,
			      int &nTotalSamples)
{
  return true;
}

////////////////////////////////////////////////////////////////
  // Convert a binary file to an ASCII file.

bool dbFFIOb::copyInstantAverages(dbFFIOb &in, dbFFIOa &out)
{ dbFFIO::RECORD_TYPE mvt;
  unsigned recordCount;
  long frameCount, tagID;
  short blanking, validity;
  int iSample;


 // For each record type, read the records and write them.
 if(!in.getRecordInfo(mvt, recordCount))
	return false;

   while(recordCount > 0)
   {

     // cout << "Copying " << recordCount << " " << valuetypeToString(mvt) << endl;

     switch(mvt) {
     case RECORD_SHORT:
       {	for(unsigned i=0; i< recordCount; i++)
		{ short v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity,
						   v, iSample))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity,
						   v, iSample);
		}
	}
	break;
     case dbFFIO::RECORD_INTEGER:
       {	for(unsigned i=0; i< recordCount; i++)
		{ int v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity,
						   v, iSample))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity,
						   v, iSample);
		}
	}
	break;
     case dbFFIO::RECORD_LONG:
       {	for(unsigned i=0; i< recordCount; i++)
		{ long v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity,
						   v, iSample))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity,
						   v, iSample);
		}
	}
	break;
#if 1
     case RECORD_FLOAT:
       {	for(unsigned i=0; i< recordCount; i++)
		{ float v;
			if(!in.readInstAverage(frameCount, tagID,
					       blanking, validity,
					       v, iSample))
			  return false;
			out.dumpInstAverage(frameCount, tagID,
					    blanking, validity,
					    v, iSample);
		}
	}
	break;
#endif
     case RECORD_DOUBLE:
       {	for(unsigned i=0; i< recordCount; i++)
		{ double v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity,
						   v, iSample))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity,
						   v, iSample);
		}
	}
	break;
     case RECORD_COMPLEX:
       {	for(unsigned i=0; i< recordCount; i++)
		{ complex<float> v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity,
						   v, iSample))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity,
						   v, iSample);
		}
	}
	break;
     case RECORD_STRING:
       {	for(unsigned i=0; i< recordCount; i++)
		{ string v;
			if(!in.readInstAverage(frameCount, tagID,
						   blanking, validity, v))
			  {
			    cout << "Bad read?? " << i << endl;
			  return false;
			  }
			out.dumpInstAverage(frameCount, tagID,
						  blanking, validity, v);
		}
	}
	break;
     default:;
	cerr << "copyInstantaneousAverages: Unsupported record type: "
	     << dbFFIO::valuetypeToString(mvt) << "(" << mvt << ")\n";
	return false;
     }
     //     cout << "	did " << recordCount << " records\n";
     // Get record count for next record group.
     in.getRecordInfo(mvt, recordCount);
   }
	return true;
}

bool dbFFIOb::copyLongAverages(dbFFIOb &in, dbFFIOa &out)
{ RECORD_TYPE mvt;
  unsigned recordCount;
  long frameCount, tagID;
  short blanking, validity;
  int iSample, nTotalSamples;
  short nValidSamples;

 // For each record type, read the records and write them.
 if(!in.getRecordInfo(mvt, recordCount))
	return false;
  while(recordCount > 0)
   {
     switch(mvt) {
     case RECORD_SHORT:
       {	for(unsigned i=0; i< recordCount; i++)
		{ short v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
						   blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples))
			  return false;

			out.dumpLongAverage(frameCount, tagID,
						blanking, validity,
						v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples);
		}
	}
	break;

     case RECORD_INTEGER:
       {	for(unsigned i=0; i< recordCount; i++)
		{ int v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
						   blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples))
			  return false;

			out.dumpLongAverage(frameCount, tagID,
						blanking, validity,
						v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples);
		}
	}
	break;
     case RECORD_LONG:
       {	for(unsigned i=0; i< recordCount; i++)
		{ long v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
						   blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples))
			return false;

			out.dumpLongAverage(frameCount, tagID,
						blanking, validity,
						v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples);
		}
	}
	break;
     case RECORD_FLOAT:
       {	for(unsigned i=0; i< recordCount; i++)
		{ float v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
					       blanking, validity,
					       v,
					       maxValue, minValue,
					       iSample,
					       nValidSamples, nTotalSamples))
			    return false;

			out.dumpLongAverage(frameCount, tagID,
					    blanking, validity,
					    v,
					    maxValue, minValue,
					    iSample,
					    nValidSamples, nTotalSamples);
		}
	}
	break;
     case RECORD_DOUBLE:
       {	for(unsigned i=0; i< recordCount; i++)
		{ double v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
						   blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples))
			  return false;

			out.dumpLongAverage(frameCount, tagID,
						  blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples);
		}
	}
	break;
     case RECORD_COMPLEX:
       {	for(unsigned i=0; i< recordCount; i++)
		{ complex<float> v, maxValue, minValue;
			if(!in.readLongAverage(frameCount, tagID,
						   blanking, validity,
						   v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples))
			  return false;
			out.dumpLongAverage(frameCount, tagID,
						blanking, validity,
						v,
						maxValue, minValue,
						iSample,
						nValidSamples, nTotalSamples);

		}
	}
	break;
     case RECORD_STRING:
       {	for(unsigned i=0; i< recordCount; i++)
		{ string v;
			if(in.readLongAverage(frameCount, tagID,
						   blanking, validity, v,
						nValidSamples, nTotalSamples))
			  return false;
			out.dumpLongAverage(frameCount, tagID,
						  blanking, validity, v,
						nValidSamples, nTotalSamples);
		}
	}
	break;
     default:;
	cerr << "dbFFIOb::copyLongAverages: Unsupported monitor type.\n";
	return false;
     }
     // Get record count for next record group.
	in.getRecordInfo(mvt, recordCount);
   }
	return true;
}

// Copes a dbFFIOb file to a dbFFIOa file. 
bool dbFFIOb::copyToASCII(const std::string &infileName,
			  const std::string &outfileName, bool isInstant)

{ dbFFIOa out;
 dbFFIOb in; 
 long frameCount;
 string sig;

 if(!in.open(infileName, false))
   return false;
 if(!out.open(outfileName, true))
   {	in.close();
	return false;
   }

 // Copy flat file header.
 if(!in.readFFHeader(frameCount, sig))	// False probably means non dbFFIOb.
 {	in.close();
	out.close();
 }

 out.writeFFHeader(frameCount, sig);

	if(isInstant)
		copyInstantAverages(in, out);
	else
		copyLongAverages(in, out);

	//	cout << "Done copying " << infileName << endl;
  return true;
}

////////////////////////////////////////////////////////////////
// Initialize vars that can be reinitalized every open.
void dbFFIOb::initb()
{
  // For writes, this is the size of the buffer. For reads it will be
  // replaced with the number of data bytes actually in the buffer.
  bytesInBuffer_ = byteArray_.size();
  offset_ = 0;
  frameCount_ = 0;
  sig_ = "";
  recordType_ = RECORD_UNKNOWN;
  recordCount_ = 0;
  recordBytes_ = 0;
}

// Open a file for reading or writing.
// Returns true on success.
bool dbFFIOb::open(const std::string &fileName, bool write)
{ std::ios_base::openmode mode = write ? std::ios_base::out : std::ios_base::in;

	if(isOpen())
		return false;
	mode |=  std::ios_base::binary;
	int status = dbFFIO::open(fileName, mode);
	initb();
	if(isRead())
	{	bytesInBuffer_ = 0;
		if(fillBuffer())
		{	// Get flat file header.
		  unpack(frameCount_);
		  unpack(sig_);
		}
		else
		  status = false;
	}

	return status;
}

bool dbFFIOb::close()
{
	if(isOpen())
	{	if(isWrite())
		{	endRecordGroup();
			flush();
		}
		dbFFIO::close();
	}
	return true;
}

// Fill the read buffer.
// If there are unread bytes, move them to the beginning of the buffer
// before the read.
/* Notes:
	offset_		Index into byteArray_ of next free slot or byte to get.
			Set by (un)pack routines and a couple others.
	bytesInBuffer_	# of data bytes that are available for reading.
			Mostly set in fillBuffer
	These two give information about record groups and aren't related
	to buffering.
	recordCount_	# of records that have been written in current
			group or # of records that need to be read.
	recordBytes_	Size of group in bytes.
*/

bool dbFFIOb::fillBuffer()
{int bytesToMove;

	if(!isRead())
	  return false;

#if 1
	if(offset_ > BUFFERSIZE)
	  { cerr << "***fillBuffer************* Buffer overflow! Buffer size: "
		 << BUFFERSIZE << " offset_ = " << offset_ << endl;
	  }
#endif

	// Move any remaining bytes to front of buffer. Then (re)fill it.
	bytesToMove = bytesInBuffer_ - offset_;
	if(bytesToMove > 0)
	{ memcpy(&byteArray_[0], &byteArray_[offset_], bytesToMove);
//cout << "Copied " << bytesToMove << " bytes from offset " << offset_ << endl;
	}
	else
		bytesToMove = 0;

	// Fill buffer.
	int bsize = byteArray_.size();
	file_.read(&byteArray_[bytesToMove], bsize-bytesToMove);
	int bytesRead = file_.gcount();
	// # bytes read + # already in buffer.
	bytesInBuffer_ = bytesToMove + bytesRead;;
#if 0
	cout << "Read " << bytesRead << " bytes. Buffer has "
	     << bytesInBuffer_ << " bytes. Offset was " << offset_ << endl;
#endif
	if(offset_ > bsize)
		cerr << "dbFFIOb::fillBuffer: offset_ was " << offset_
		     << " but there were only " << bsize
		     << " bytes in the buffer!\n";
	if((bytesInBuffer_ <= 0) && !eof())
	{	cerr << "dbFFIOb::fillBuffer: Ran out of file.\n";
	}
	// Reset index.
	offset_ = 0;
	if(bytesInBuffer_ > 0)
		  return true;
	return false;
}

void dbFFIOb::writeBuffer()
{
	if(offset_ && isWrite())
	{	file_.write(&byteArray_[0], offset_);
#if 1
	if(offset_ > BUFFERSIZE)
	  { cout << "***writeBuffer************* Buffer overflow! Buffer size: "
		 << BUFFERSIZE << " offset_ = " << offset_ << endl;
	  }
#endif

		offset_ = 0;
	}
}

// Is there space left in the buffer for reading or writing?
// If not, either fill or empty the buffer.
bool dbFFIOb::checkBuffer(int size)
{ bool status = true;
  int bufsize = byteArray_.size();

	// Note: offset_ can change in writeBuffer & fillBuffer.

	if(isWrite() && ((offset_ + size) > bufsize))
	{	writeBuffer();
		status = ((offset_ + size) <= bufsize);
	}
	else
	if(isRead() && ((offset_ + size) > bytesInBuffer_))
	{      fillBuffer();
		status = ((offset_ + size) <= bytesInBuffer_);
	}

	return status;
}

// Make sure current record group matches the requested type and that
// there is enough space to read or write one record (of the given size).
bool dbFFIOb::checkBuffer(RECORD_TYPE mvt, int size)
{
  // If this is a different type of record, finish current and start new.
    if(mvt != recordType_)
    {	if(isRead())
	{	// For reads, it is an error to try to read a different
		// type of record while there are still old records.
		if(recordCount_ > 0)
			return false;
		else
		{	endRecordGroup();
			startRecordGroup(mvt);
		}
	}
	else
	if(isWrite())
	{	endRecordGroup();
		startRecordGroup(mvt);
	}
    }
    // Now do space check.
    bool status = checkBuffer(size);
	return status;
}

// Start a record group.
// If reading, unpack record type, count and # of bytes in group.
// For writing, remember current file position and write placeholders
// for record type, count and # of bytes.
bool dbFFIOb::startRecordGroup(int mvt)
{
	if(isRead())
	{static int size = sizeof(recordType_)+sizeof(recordCount_)+
			   sizeof(recordBytes_);
		if(checkBuffer(size))
		{	unpack(recordType_);
			unpack(recordCount_);
			unpack(recordBytes_);
#if 0
	cout << "New record group: " << valuetypeToString(recordType_)
	     << " (" << recordType_ << ") count: " << recordCount_
	     << " # bytes: " << recordBytes_ << endl;
#endif
		}
		else	// Problem reading file. (EOF probably).
		{
		  endRecordGroup();
		  return false;
		}
	}
	else
	if(isWrite())
	{	if(recordType_ >= 0)	// Finish any current record group.
			endRecordGroup();
		recordType_ = mvt;
		flush();		// Write out buffer so tellp works.
		recordStart_ = file_.tellp();	// Where in file this starts.
//		cout << "Current position is " << recordStart_ << endl;
		// flush clears buffer so there should be room.
		pack(recordType_);
		// Write placeholders.
		pack(recordCount_);
		pack(recordBytes_);
	}
	return true;
}

// Finish a record group.
// For writes, back up to start of record and write record type, count
// and # of bytes in group.
// For reads and writes: reset counters.
bool dbFFIOb::endRecordGroup()
{
	if(isWrite() && (recordType_ >= 0))
	{	flush();	// Empty buffer so we can use it.
		// Seek back to start of record then fill in information.
		file_.seekp(recordStart_);
		pack(recordType_);
		pack(recordCount_);
		pack(recordBytes_);
		flush();	// Write.
		file_.seekp(0, ios_base::end);	// Move back to end.
#if 0
		{ float avg = float(recordBytes_)/recordCount_;
		cout << "recordCount " << recordCount_
		     << " recordBytes " << recordBytes_ << " avg bytes/rec: "
		     << avg << endl;
		}
#endif
	}

	recordType_ = RECORD_UNKNOWN;
	recordCount_ = 0;
	recordBytes_ = 0;

	return true;
}

bool dbFFIOb::getRecordInfo(RECORD_TYPE &mvt,
			    unsigned &recordCount)
{
	// If called while reading and there aren't any records left,
	// read next header. 
        if(((recordType_ < 0)||(recordCount_<=0)) && isRead())
		startRecordGroup(0);

	mvt = static_cast<RECORD_TYPE>(recordType_);
	recordCount = recordCount_;
	return (recordCount_ > 0);
}
