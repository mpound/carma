#include "carma/szautil/Exception.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Monitor.h"

#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/astrom.h"
#include "carma/szaarrayutils/szaregs.h"

#include "carma/szautil/RegAxisRange.h"
#include "carma/szautil/RegParser.h"

#include <ctype.h> // isspace()
#include <iomanip>

using namespace sza::util;
using namespace sza::array;
using namespace std;

/**
 * Declare a static array of valid keywords
 */
Monitor::Keyword Monitor::keywords[] = {
  {"add",  Monitor::ADD_REGISTER},
  {"read", Monitor::READ}
};

int Monitor::nKey_ = sizeof(Monitor::keywords)/sizeof(Monitor::Keyword);

/**
 * Declare a static array of valid format args
 */
Monitor::Format Monitor::formats[] = {
  {"string",  MonitorDataType::FM_STRING},
  {"bool",    MonitorDataType::FM_BOOL},
  {"uchar",   MonitorDataType::FM_UCHAR},
  {"char",    MonitorDataType::FM_CHAR},
  {"float",   MonitorDataType::FM_FLOAT},
  {"double",  MonitorDataType::FM_DOUBLE},
  {"short",   MonitorDataType::FM_SHORT},
  {"ushort",  MonitorDataType::FM_USHORT},
  {"int",     MonitorDataType::FM_INT},
  {"uint",    MonitorDataType::FM_UINT},
  {"long",    MonitorDataType::FM_LONG},
  {"ulong",   MonitorDataType::FM_ULONG},
  {"date",    MonitorDataType::FM_DATE},
  {"complex", MonitorDataType::FM_COMPLEX_FLOAT},
};

int Monitor::nFormat_ = sizeof(Monitor::formats)/sizeof(Monitor::Format);

/**.......................................................................
 * Constructor.
 */
void Monitor::privateConstructor(string arcDir, string calFile, string host,
				 string start, bool startWasSet,
				 string stop,  bool stopWasSet,
				 string regFile)
{
  initialized_  = false;
  calFile_      = calFile;
  host_         = host;
  ms_           = 0;
  regFile_      = regFile;
  outputStream_ = 0;
  readRegSpecFromFile_ = regFile_.size() > 0;
  
  // Attempt to allocate the new output stream
  
  outputStream_ = new_OutputStream();
  
  if(!outputStream_ ||
     open_StringOutputStream(outputStream_, 1, fmtString_, 
			     sizeof(fmtString_))) {
    del_OutputStream(outputStream_);
    ThrowError("Unable to allocate a new stream");
  };
  
  // If no stop date was given, we will end by reading from the
  // real-time controller
  
  type_ = stopWasSet ? MS_FILE : MS_NET;
  
  // If a start date was explicitly given, we will start by reading
  // from the archive
  
  if(startWasSet) {
    unsigned lmask = static_cast<unsigned>(type_);
    unsigned rmask = static_cast<unsigned>(MS_FILE);
    type_ = static_cast<Monitor::StreamType>(lmask | rmask);
  }
  
  // Open the archive stream and return its handle.
  
  if(type_ & MS_FILE)  
    ms_ = new_FileMonitorStream((char*)arcDir.c_str(), 
				parseDateAndTime(start),
				parseDateAndTime(stop));
  else
    ms_ = new_NetMonitorStream((char*)host.c_str());
  
  if(ms_ == 0) {
    ThrowSimpleError("Unable to open the archive stream");
  }
  
  setMonitoringInterval(1);
}

/**.......................................................................
 * Constructor.
 */
Monitor::Monitor(string arcDir, string calFile, string host,
		 string startMjd, bool startWasSet,
		 string stopMjd,  bool stopWasSet,
		 string regFile) 
{
  privateConstructor(arcDir, calFile, host, startMjd, startWasSet,
		     stopMjd, stopWasSet, regFile);
} 

/**.......................................................................
 * Constructor.
 */
Monitor::Monitor(string arcDir, string calFile, 
		 string startMjd, string stopMjd) 
{
  privateConstructor(arcDir, calFile, "", startMjd, true, stopMjd, true, "");
}

/**.......................................................................
 * Constructor.
 */
Monitor::Monitor(string host, string calFile) 
{
  privateConstructor("", calFile, host, "", false, "", false, "");
}

/**.......................................................................
 * Destructor.
 */
Monitor::~Monitor() 
{
  if(ms_ != 0)
    ms_ = del_MonitorStream(ms_);
  
  if(outputStream_)
    del_OutputStream(outputStream_);
}

/**.......................................................................
 * Add a register to the set of registers to be monitored.
 */
void Monitor::addRegister(string regmapName, string boardName, string regName,
			  RegAspect aspect, 
			  MonitorDataType::FormatType selectedFormat, 
			  char* formatString,
			  MonitorDataType::FormatType nativeFormat, 
			  CoordRange* range)
{
  // And add the element to the selection list
  
  MonitorSelection sel(selections_.size(), regmapName, 
		       boardName, regName, range);
  
  selections_.push_back(sel);
  
  // Increase the size of the register selection
  
  selectedRegs_.resize(selections_.size());
  
  // And increase the size of the format vector
  
  selectedFormats_.push_back(selectedFormat);
  
  // FormatString
  
  MonitorDataType::FormatType type = selectedFormat==MonitorDataType::FM_UNKNOWN ? nativeFormat : selectedFormat;
  formatStrings_.push_back(formatString==0 ? formatStringOf(type, aspect) : formatString);
  
  // Store the native format of this register too
  
  nativeFormats_.push_back(nativeFormat);
  
  // And increase the size of the format vector
  
  aspects_.push_back(aspect);
  
  // Add a map element for this register
  
  regMap_[regmapName][boardName][regName] = 
    &selectedRegs_[selectedRegs_.size()-1];
  
  // Add a map element for this register's format
  
  regSelFormat_[regmapName][boardName][regName] = 
    selectedFormats_[selectedFormats_.size()-1];
  
  regNatFormat_[regmapName][boardName][regName] = 
    nativeFormats_[nativeFormats_.size()-1];
  
  regAspect_[regmapName][boardName][regName] = 
    aspects_[aspects_.size()-1];
}

/**.......................................................................
 * Add a vector of registers
 */
void Monitor::addRegisters(std::vector<RegDescription>& regs, MonitorDataType::FormatType format, char* formatString)
{
  for(unsigned iReg=0; iReg < regs.size(); iReg++) {
    addRegister(regs[iReg], formatOf(regs[iReg], format), formatString);
  }
}

/**.......................................................................
 * Add a register to the set of registers to be monitored.  In this
 * case the register specification is read from an input stream.
 */
void Monitor::addRegister(RegDescription& desc, MonitorDataType::FormatType format,
			  char* formatString)
{
  ArrRegMap*    aregmap = desc.arrayMap()->regmaps[desc.iRegMap()];
  RegMapBoard*  board   = aregmap->regmap->boards_[desc.iBoard()];
  RegMapBlock*  block   = board->blocks[desc.iBlock()];
  
  addRegister(aregmap->name, board->name, block->name_, desc.aspect(),
	      format, formatString, formatOf(desc), desc.getRangePtr());
}

/**.......................................................................
 * Add a register to the set of registers to be monitored.  In this
 * case the register specification is read from an input stream.
 */
void Monitor::addRegister(InputStream* stream) 
{
  // Read any leading whitespace
  
  if(input_skip_white(stream, 1, 0))
    ThrowError("Error skipping whitespace");
  
  RegParser parser;
  
  std::vector<RegDescription> regs = 
    parser.inputRegs(stream, true, ms_->arraymap, REG_INPUT_RANGE, true);
  
  switch(parser.validity()) {
  case REG_VALID:
    {
      // Add the register to our register map
      
      MonitorDataType::FormatType type = readFormat(stream);
      char* formatStr = readFormatString(stream);
      
      addRegisters(regs, type, formatStr);

      skipToNextLine(stream);
    }
    break;
    
    // A syntactically correct, but unknown register
    // specification was read.  We will simply skip to the end
    // of the line.
    
  case REG_UNKNOWN:
    input_error(stream, 1, "Error reading register specification.\n");
    skipToNextLine(stream);
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Read the next frame
 */
MsReadState Monitor::readNextFrame()
{
  LogStream errStr;
  MsReadState state;
  
  if(!initialized_)
    reinitialize();
  
  // Read the next frame of registers
  
  switch((state=ms_read_frame(ms_, 1))) {
  case MS_READ_AGAIN:  // We should never get this when the dowait
		       // argument or ms_read_frame() is 1, so treat
		       // this as an error.
    //    errStr.appendMessage(true, "Error while reading from the monitor stream");
    //    throw Error(errStr);
    break;
  case MS_READ_BREAK:// A stream is usually composed of many files,
		     // spliced together behind the scenes. The
		     // MS_READ_BREAK code is returned each time
		     // that the end of a file is reached. It is
		     // usually ignored - but see MS_READ_REGMAP
		     // below.
    break;
  case MS_READ_REGMAP:  // An incompatible register map was
			// encountered at the start of a new
			// file. The old register map has now been
			// deleted and a new one has has been
			// allocated in its place. Note that where
			// needed, the MS_READ_BREAK return code can
			// be interpretted as prior warning that
			// this might happen on the following read.
    
    // Look up and re-select the desired registers from the new
    // register map, and load its calibration parameters from the
    // specified calibration file.
    
    reinitialize();
    break;
  case MS_READ_ENDED:  // We have reached the end of the requested
		       // time range.
    if(type_ == MS_BOTH) {
      type_ = MS_NET;
      changeMonitorStream();
      state = MS_READ_AGAIN;
    }
    break;
  case MS_READ_DONE:   // A new frame of register values has been read 
    break;
  default:
    COUT("State was: " << state);
    break;
  };
  
  return state;
}

/**.......................................................................
 * Count frames
 */
unsigned Monitor::countFrames()
{
  unsigned count=0;
  sza::array::MsReadState state;

  if(!initialized_)
    reinitialize();
  
  // Count frames from the monitor stream

  while((state=ms_count_frame(ms_, 1)) != MS_READ_ENDED) {
    if(state == MS_READ_DONE) {
      ++count;
    }
  }
  
  return count;
}

/**.......................................................................
 * (Re)initialize
 */
void Monitor::reinitialize() 
{
  // Look up and select the registers that are to be retrieved, then
  // load calibration parameters from the specified calibration file.

  sendRegisterSelection();

  // Send the cal file information

  loadCalFile();
  
  // Set the interval at which we want to receive data frames
  
  if(type_ == MS_NET)
    sendMonitoringInterval();
  
  // And mark this object as initialized
  
  initialized_ = true;
}

/**.......................................................................
 * Select the registers we want to monitor.  Note that we cannot do
 * this until ms_ has been successfully created.
 */
void Monitor::sendRegisterSelection() 
{
  if(ms_ == 0)
    return;
  
  if(ms_select_regs(ms_, true, true, selections_, selectedRegs_) != MS_SEND_DONE) 
    ThrowError("Error sending register selection");
  
  // Now that we have filled selectedRegs_, cache the regAxisRange
  // corresponding to each register selection
  
  regAxisRanges_.resize(selectedRegs_.size());
  for(unsigned iReg=0; iReg < selectedRegs_.size(); iReg++) {
    RegAxisRange range(selectedRegs_[iReg]);
    regAxisRanges_[iReg] = range;
  }
}  

/**.......................................................................
 * Select the registers we want to monitor.  Note that we cannot do
 * this until ms_ has been successfully created.
 */
void Monitor::loadCalFile()
{
  LogStream errStr;
  
  if(ms_ == 0)
    return;
  
  if(ms_load_cal_file(ms_, "", (char*)calFile_.c_str())) {
    errStr.appendMessage(true, "Error loading cal file");
    throw Error(errStr);
  }
}  

/**.......................................................................
 * Set the monitoring interval
 */
void Monitor::setMonitoringInterval(unsigned interval) 
{
  LogStream errStr;
  
  if(ms_ == 0)
    return;
  
  ms_->interval = interval;
}

/**.......................................................................
 * Send the monitoring interval to the source of the data frames.
 */
void Monitor::sendMonitoringInterval() 
{
  LogStream errStr;
  
  if(ms_ == 0)
    return;
  
  if(ms_queue_interval(ms_, ms_->interval) != MS_SEND_AGAIN ||
     ms_send_msg(ms_, 1) != MS_SEND_DONE) {
    errStr.appendMessage(true, "Failed to set interval");
    throw Error(errStr);
  }
}

/**.......................................................................
 * Read a register from the latest frame as whatever type
 */
void Monitor::getRegister(RegDescription* desc, 
			  MonitorDataType* val,
			  CoordRange* range)
{
  int ierr;
  
  if(desc==0)
    ThrowError("NULL register description");
  
  CoordRange coordRange;
  
  //  RegAxisRange axisRange(*desc, range);
  
  RegAxisRange axisRange(*desc, range);
  
  // Iterate over all elements of the requested register
  
  for(axisRange.reset(); !axisRange.isEnd(); ++axisRange) {
    
    coordRange = axisRange.currentCoordRange();
    
    // Read complex floats and dates in their native formats
    
    switch(val->nativeFormat) {
    case MonitorDataType::FM_DATE:
      ierr = ms_get_date(ms_, desc, &val->val.data_.date, &coordRange);
      break;
    case MonitorDataType::FM_COMPLEX_FLOAT:
      ierr = ms_get_complex_float(ms_, desc, &val->val.data_.cf, &coordRange);
      break;
      
      // Else read it as whatever type the user wanted
      
    default:
      switch(val->selectedFormat) {
      case MonitorDataType::FM_BOOL:
      case MonitorDataType::FM_CHAR:
      case MonitorDataType::FM_STRING:
	ierr = ms_get_char(ms_, desc, &val->val.data_.c, &coordRange);
	break;
      case MonitorDataType::FM_UCHAR:
	ierr = ms_get_uchar(ms_, desc, &val->val.data_.uc, &coordRange);
	break;
      case MonitorDataType::FM_INT:
	ierr = ms_get_int(ms_, desc, &val->val.data_.i, &coordRange);
	break;
      case MonitorDataType::FM_UINT:
	ierr = ms_get_uint(ms_, desc, &val->val.data_.ui, &coordRange);
	break;
      case MonitorDataType::FM_LONG:
	ierr = ms_get_long(ms_, desc, &val->val.data_.l, &coordRange);
	break;
      case MonitorDataType::FM_ULONG:
	ierr = ms_get_ulong(ms_, desc, &val->val.data_.ul, &coordRange);
	break;
      case MonitorDataType::FM_FLOAT:
	ierr = ms_get_float(ms_, desc, &val->val.data_.f, &coordRange);
	break;
      case MonitorDataType::FM_DOUBLE:
	ierr = ms_get_double(ms_, desc, &val->val.data_.d, &coordRange);
	break;
      case MonitorDataType::FM_DATE:
	ierr = ms_get_date(ms_, desc, &val->val.data_.date, &coordRange);
	break;
      case MonitorDataType::FM_COMPLEX_FLOAT:
	ierr = ms_get_complex_float(ms_, desc, &val->val.data_.cf, &coordRange);
	break;
      default:
	break;
      }
    }
  }
  
  // If an error occurred, throw it now.
  
  if(ierr==1)
    ThrowError("Error reading data");
}

/**.......................................................................
 * Read a register from the latest frame as whatever type
 */
void Monitor::getSingleRegisterVal(RegDescription* desc, 
				   MonitorDataType* val,
				   CoordRange* coordRange)
{
  int ierr;
  
  if(desc==0)
    ThrowError("NULL register description");
  
  // Read complex floats and dates in their native formats
  
  switch(val->nativeFormat) {
  case MonitorDataType::FM_DATE:
    ierr = ms_get_date(ms_, desc, &val->val.data_.date, coordRange);
    break;
  case MonitorDataType::FM_COMPLEX_FLOAT:
    ierr = ms_get_complex_float(ms_, desc, &val->val.data_.cf, coordRange);
    break;
    
    // Else read it as whatever type the user wanted
    
  default:
    switch(val->selectedFormat) {
    case MonitorDataType::FM_BOOL:
    case MonitorDataType::FM_CHAR:
    case MonitorDataType::FM_STRING:
      ierr = ms_get_char(ms_, desc, &val->val.data_.c, coordRange);
      break;
    case MonitorDataType::FM_INT:
      ierr = ms_get_int(ms_, desc, &val->val.data_.i, coordRange);
      break;
    case MonitorDataType::FM_UINT:
      ierr = ms_get_uint(ms_, desc, &val->val.data_.ui, coordRange);
      break;
    case MonitorDataType::FM_LONG:
      ierr = ms_get_long(ms_, desc, &val->val.data_.l, coordRange);
      break;
    case MonitorDataType::FM_ULONG:
      ierr = ms_get_ulong(ms_, desc, &val->val.data_.ul, coordRange);
      break;
    case MonitorDataType::FM_FLOAT:
      ierr = ms_get_float(ms_, desc, &val->val.data_.f, coordRange);
      break;
    case MonitorDataType::FM_DOUBLE:
      ierr = ms_get_double(ms_, desc, &val->val.data_.d, coordRange);
      break;
    case MonitorDataType::FM_DATE:
      ierr = ms_get_date(ms_, desc, &val->val.data_.date, coordRange);
      break;
    case MonitorDataType::FM_COMPLEX_FLOAT:
      ierr = ms_get_complex_float(ms_, desc, &val->val.data_.cf, coordRange);
      break;
    default:
      break;
    }
  }
  
  // If an error occurred, throw it now.
  
  if(ierr==1)
    ThrowError("Error reading data");
}

/**.......................................................................
 * Read a register from the latest frame as whatever type
 */
void Monitor::getRegisterVals(std::vector<MonitorDataType>& data,
			      RegDescription* desc, 
			      MonitorDataType* val,
			      RegAxisRange& range)
{
  int ierr;
  
  // Resize the data vector to accomodate all elements of the selected register
  
  data.resize(range.nEl());
  
  if(desc==0)
    ThrowError("NULL register description");
  
  // Read complex floats and dates in their native formats
  
  fflush(stdout);

  switch(val->nativeFormat) {
  case MonitorDataType::FM_DATE:
    ierr = ms_get_date(ms_, desc, &data[0], range);
    break;
  case MonitorDataType::FM_COMPLEX_FLOAT:
    ierr = ms_get_complex_float(ms_, desc, &data[0], range);
    break;
    
    // Else read it as whatever type the user wanted
    
  default:
    switch(val->selectedFormat) {
    case MonitorDataType::FM_BOOL:
    case MonitorDataType::FM_UCHAR:
      ierr = ms_get_uchar(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_CHAR:
    case MonitorDataType::FM_STRING:
      ierr = ms_get_char(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_INT:
      ierr = ms_get_int(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_UINT:
      ierr = ms_get_uint(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_LONG:
      ierr = ms_get_long(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_ULONG:
      ierr = ms_get_ulong(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_FLOAT:
      ierr = ms_get_float(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_DOUBLE:
      ierr = ms_get_double(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_DATE:
      ierr = ms_get_date(ms_, desc, &data[0], range);
      break;
    case MonitorDataType::FM_COMPLEX_FLOAT:
      ierr = ms_get_complex_float(ms_, desc, &data[0], range);
      break;
    default:
      break;
    }
  }
  
  // If an error occurred, throw it now.
  
  if(ierr==1)
    ThrowError("Error reading data");
}

/**.......................................................................
 * Read a register from the latest frame as unsigned ints.
 */
void Monitor::
getRegister(std::string regMapName, std::string boardName, std::string blockName,
	    unsigned* data, CoordRange* range)
{
  RegDescription* desc=0;
  
  desc = regMap_[regMapName][boardName][blockName];
  
  if(desc == 0)
    return;
  
  if(ms_get_uint(ms_, desc, data, range)) 
    ThrowError("Error reading register data");
}

/**.......................................................................
 * Read a register from the latest frame as unsigned longs.
 */
void Monitor::
getRegister(std::string regMapName, std::string boardName, std::string blockName,
	    unsigned long* data, CoordRange* range)
  
{
  RegDescription* desc=0;
  
  desc = regMap_[regMapName][boardName][blockName];
  
  if(desc==0)
    return;
  
  if(ms_get_ulong(ms_, desc, data, range)) 
    ThrowError("Error reading register data");
}

/**.......................................................................
 * Read a register from the latest frame as longs.
 */
void Monitor::
getRegister(std::string regMapName, std::string boardName, std::string blockName,
	    long* data, CoordRange* range)
{
  RegDescription* desc=0;
  
  desc = regMap_[regMapName][boardName][blockName];
  
  if(desc == 0)
    return;
  
  if(ms_get_long(ms_, desc, data, range))
    ThrowError("Error reading register data");
}

/**.......................................................................
 * Read a register from the latest frame as floats.
 */
void Monitor::
getRegister(std::string regMapName, std::string boardName, std::string blockName,
	    float* data, CoordRange* range)
{
  RegDescription* desc=0;
  
  desc = regMap_[regMapName][boardName][blockName];
  
  if(desc == 0)
    return;
  
  if(ms_get_float(ms_, desc, data, range)) 
    ThrowError("Error reading register data");
}

/**.......................................................................
 * Read a register from the latest frame as doubles
 */
void Monitor::
getRegister(std::string regMapName, std::string boardName, std::string blockName,
	    double* data, CoordRange* range)
{
  RegDescription* desc=0;
  
  desc = regMap_[regMapName][boardName][blockName];
  
  if(desc == 0)
    return;
  
  if(ms_get_double(ms_, desc, data, range)) 
    ThrowError("Error reading register data");
}

/**.......................................................................
 * Print the latest value of all selected regs, as doubles
 */
void Monitor::printDoubleRegs()
{
  double val;
  Coord coord;
  CoordRange coordRange;
  
  for(unsigned ireg=0; ireg < selectedRegs_.size(); ireg++) {
    
    RegDescription* reg = &selectedRegs_[ireg];
    RegAxisRange range(*reg);
    
    for(range.reset(); !range.isEnd(); ++range) {
      
      coord = range.currentCoord();
      
      coordRange.setStartCoord(coord);
      coordRange.setStopCoord(coord);
      
      if(ms_get_double(ms_, reg, &val, &coordRange))
	ThrowError("Error reading register data");
      
      // Now print the value
      
      cout << val << " ";
    }
  }
  cout << endl;
}

/**.......................................................................
 * Print the latest value of all selected regs, as unsigned ints
 */
void Monitor::printUnsignedIntRegs()
{
  unsigned val;
  Coord coord;
  CoordRange coordRange;
  
  for(unsigned ireg=0; ireg < selectedRegs_.size(); ireg++) {
    
    RegDescription* reg = &selectedRegs_[ireg];
    RegAxisRange range(*reg);
    
    for(range.reset(); !range.isEnd(); ++range) {
      
      coord = range.currentCoord();
      
      coordRange.setStartCoord(coord);
      coordRange.setStopCoord(coord);
      
      if(ms_get_uint(ms_, reg, &val, &coordRange))
	ThrowError("Error reading register data");
      
      // Now print the value
      
      cout << val << " ";
    }
  }
  cout << endl;
}

void Monitor::printRegs()
{
  LogStream errStr;
  MonitorDataType val, tmpVal;
  Coord startCoord, stopCoord;
  CoordRange coordRange;
  RegDate date;
  Complex<float> cf;
  RegAspect aspect;
  
  // Iterate over all register selections
  
  for(unsigned iReg=0; iReg < selectedRegs_.size(); iReg++) {
    
    MonitorDataType::FormatType selectedFormat = selectedFormats_[iReg];
    MonitorDataType::FormatType nativeFormat   = nativeFormats_[iReg];
    char* formatPtr = (char*)formatStrings_[iReg].c_str();
    
    // Read each register according to type
    
    val.selectedFormat = selectedFormat;
    val.nativeFormat   = nativeFormat;
    
    aspect = aspects_[iReg];
    
    // Print the registers one at a time
    
    RegDescription* reg = &selectedRegs_[iReg];
    RegAxisRange range(*reg);
    
    for(range.reset(); !range.isEnd(); ++range) {
      
      clr_StringOutputStream(outputStream_);
      
      startCoord = range.currentCoord();
      coordRange.setStartCoord(startCoord);
      coordRange.setStopCoord(startCoord);
      
      getSingleRegisterVal(reg, &val, &coordRange);
      
      // Now print the value
      
      Complex<float>::Data* dPtr;
      
      if(val.nativeFormat == MonitorDataType::FM_COMPLEX_FLOAT) {
	
	switch(aspect) {
	case REG_PLAIN:
	  {
	    dPtr = &val.val.data_.cf;
	    
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    outputReg(tmpVal, REG_REAL, formatPtr);
	    
	    cout << (dPtr->imag_ < 0 ? " - i " : " + i ");
	    dPtr->imag_ = fabs(dPtr->imag_);
	    
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    outputReg(tmpVal, REG_IMAG, formatPtr);
	  }
	  break;
	default:
	  {
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    outputReg(tmpVal, aspect, formatPtr);
	  }
	  break;
	}
	
      } else 
	outputReg(val, aspect, formatPtr);
    }
    cout << " ";
  }
  cout << endl;
}

/**.......................................................................
 * Return a vector of register values, as datatypes
 */
std::vector<std::vector<MonitorDataType> > 
Monitor::getRegsAsDataTypes()
{
  LogStream errStr;
  MonitorDataType val, tmpVal;
  Coord startCoord, stopCoord;
  CoordRange coordRange;
  RegDate date;
  Complex<float> cf;
  RegAspect aspect;
  std::ostringstream os;
  static unsigned counter = 0;
  
  // The return matrix will contain one vector per register
  // description, with the size of each vector equal to the number of
  // register indices specified
  
  std::vector<std::vector<MonitorDataType> > outputVals;
  
  // Iterate over all register selections
  
  for(unsigned iReg=0; iReg < selectedRegs_.size(); iReg++) {
   
    // Get the selecoted and native formats for this register
    
    MonitorDataType::FormatType selectedFormat = selectedFormats_[iReg];
    MonitorDataType::FormatType nativeFormat   = nativeFormats_[iReg];

    char* formatPtr = (char*)formatStrings_[iReg].c_str();
    std::string& formatStr = formatStrings_[iReg];

    // A vector which will be created for each register returned
    
    std::vector<MonitorDataType> regVals;
    
    // Read each register according to type
    
    val.selectedFormat = selectedFormat;
    val.nativeFormat   = nativeFormat;
    
    aspect = aspects_[iReg];
    
    // Format the registers one at a time
    
    RegDescription* reg = &selectedRegs_[iReg];
    RegAxisRange& range = regAxisRanges_[iReg];
    
    // Get the register values now
    
    getRegisterVals(regVals, reg, &val, range);

    // Every 100 frames, print the first register if it is the date
    
    if(iReg==0 && val.nativeFormat==MonitorDataType::FM_DATE && counter%100 == 0) {
      RegDate date;
      *date.data() = regVals[0].val.data_.date;
      cout << "Reading " << date << endl;
      fflush(stdout);
    }
    
    // Now format the values
    
    Complex<float>::Data* dPtr;
    
    // If the native format is a string or a char, and the output
    // format will be a string, push the characters onto a string
    // stream for later reconstruction
    
    if((val.nativeFormat == MonitorDataType::FM_STRING || 
	val.nativeFormat == MonitorDataType::FM_CHAR ||
	val.nativeFormat == MonitorDataType::FM_UCHAR)
       && val.selectedFormat == MonitorDataType::FM_STRING) {
      
      os.str("");
      for(unsigned i=0; i < regVals.size(); i++) {
	os << (char)(regVals[i].val.data_.c);
      }
      
      // Else treat this register normally
      
    } else {
      
      // Else if a complex float
      
      if(val.nativeFormat == MonitorDataType::FM_COMPLEX_FLOAT) {
	
	switch(aspect) {
	  
	  // Just format regs as whatever was requested
	  
	case REG_PLAIN:
	  formatRegs(regVals, val, aspect, formatStr);
	  break;
	  
	  // Fomat all other versions of complex registers (phases,
	  // amplitudes, etc) as doubles
	  
	default:
	  {
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    formatRegs(regVals, tmpVal, aspect, formatStr);
	  }
	  
	  break;
	}
	
	// Else not complex -- just format according to the user's
	// wishes
	
      } else {
	formatRegs(regVals, val, aspect, formatStr);
      }
    }
    
    // If we just read the last byte of a string, cap it off, and insert into the vector
    
    if(val.selectedFormat == MonitorDataType::FM_STRING) {
      os << ends;
      
      val.stringVal = os.str();
      val.formatStr      = formatStr;
      val.aspect         = aspect;
      val.selectedFormat = selectedFormat;
      val.nativeFormat   = nativeFormat;
      
      regVals.resize(0);
      regVals.push_back(val);
    }
    
    // And insert the new vector into the output vector
    
    outputVals.push_back(regVals);
  }
  
  counter++;
  
  return outputVals;
}

void Monitor::printRegs2()
{
  LogStream errStr;
  MonitorDataType val, tmpVal;
  Coord startCoord, stopCoord;
  CoordRange coordRange;
  RegDate date;
  Complex<float> cf;
  RegAspect aspect;
  std::ostringstream os;
  
  // Iterate over all register selections
  
  for(unsigned iReg=0; iReg < selectedRegs_.size(); iReg++) {
    
    MonitorDataType::FormatType selectedFormat = selectedFormats_[iReg];
    MonitorDataType::FormatType nativeFormat   = nativeFormats_[iReg];
    char* formatPtr = (char*)formatStrings_[iReg].c_str();
    std::string& formatStr = formatStrings_[iReg];
    
    // Read each register according to type
    
    val.selectedFormat = selectedFormat;
    val.nativeFormat   = nativeFormat;
    
    aspect = aspects_[iReg];
    
    // Print the registers one at a time
    
    RegDescription* reg = &selectedRegs_[iReg];
    RegAxisRange range(*reg);
    
    bool first=true;
    for(range.reset(); !range.isEnd(); ++range) {
      
      clr_StringOutputStream(outputStream_);
      
      startCoord = range.currentCoord();
      coordRange.setStartCoord(startCoord);
      coordRange.setStopCoord(startCoord);
      
      getRegister(reg, &val, &coordRange);
      
      // Now format the value
      
      Complex<float>::Data* dPtr;
      
      if((val.nativeFormat == MonitorDataType::FM_STRING || 
	  val.nativeFormat == MonitorDataType::FM_CHAR)
	 && val.selectedFormat == MonitorDataType::FM_STRING) {

	if(first) {
	  os.str("");
	  first = false;
	} 
	os << (char)val.val.data_.c;
      } else {
    
	if(val.nativeFormat == MonitorDataType::FM_COMPLEX_FLOAT) {
	  
	  switch(aspect) {
	  case REG_PLAIN:
	    {
	      dPtr = &val.val.data_.cf;
	      
	      tmpVal = val;
	      if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
		tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	      tmpVal = formatReg(tmpVal, REG_REAL, formatStr);
	      tmpVal.print();
	      
	      cout << (dPtr->imag_ < 0 ? " - i " : " + i ");
	      dPtr->imag_ = fabs(dPtr->imag_);
	      
	      tmpVal = val;
	      if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
		tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	      tmpVal = formatReg(tmpVal, REG_IMAG, formatStr);
	      tmpVal.print();
	    }
	    break;
	  default:
	    {
	      tmpVal = val;
	      if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
		tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	      val = formatReg(tmpVal, aspect, formatStr);
	      val.print();
	    }
	    break;
	  }
	  
	} else {
	  val = formatReg(val, aspect, formatStr);
	  val.print();
	}
      }

      // If we just read the last byte of a string, cap it off, and insert into the vector
    
      if(val.selectedFormat == MonitorDataType::FM_STRING) {
	os << ends;
	
	// If we just read the last byte of a string, cap it off, and
	// insert into the vector
	
	os << ends;
	val.stringVal = os.str();
	val.formatStr = formatStr;
	val.aspect = aspect;
	val.print();
      }
    }
  }
  std::cout << std::endl;
}

/**.......................................................................
 * Get the vector of laatest values of all selected regs, cast as
 * doubles.
 */
std::vector<double> Monitor::getRegsAsDoubles()
{
  LogStream errStr;
  MonitorDataType val, tmpVal;
  Coord startCoord, stopCoord;
  CoordRange coordRange;
  RegDate date;
  Complex<float> cf;
  RegAspect aspect;
  
  std::vector<double> outputVals;
  
  // Iterate over all register selections
  
  for(unsigned iReg=0; iReg < selectedRegs_.size(); iReg++) {
    
    MonitorDataType::FormatType selectedFormat = selectedFormats_[iReg];
    MonitorDataType::FormatType nativeFormat   = nativeFormats_[iReg];
    char* formatPtr = (char*)formatStrings_[iReg].c_str();
    
    // Read each register according to type
    
    val.selectedFormat = MonitorDataType::FM_DOUBLE;
    val.nativeFormat   = nativeFormat;
    
    aspect = aspects_[iReg];
    
    // Print the registers one at a time
    
    RegDescription* reg = &selectedRegs_[iReg];
    RegAxisRange range(*reg);
    
    for(range.reset(); !range.isEnd(); ++range) {
      
      clr_StringOutputStream(outputStream_);
      
      startCoord = range.currentCoord();
      coordRange.setStartCoord(startCoord);
      coordRange.setStopCoord(startCoord);
      
      getRegister(reg, &val, &coordRange);
      
      // Now print the value
      
      Complex<float>::Data* dPtr;
      
      if(val.nativeFormat == MonitorDataType::FM_COMPLEX_FLOAT) {
	
	switch(aspect) {
	case REG_PLAIN:
	  {
	    dPtr = &val.val.data_.cf;
	    
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    
	    outputVals.push_back(getRegAsDouble(tmpVal, REG_REAL));
	    
	    cout << (dPtr->imag_ < 0 ? " - i " : " + i ");
	    dPtr->imag_ = fabs(dPtr->imag_);
	    
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    
	    outputVals.push_back(getRegAsDouble(tmpVal, REG_IMAG));
	    
	  }
	  break;
	default:
	  {
	    tmpVal = val;
	    if(tmpVal.selectedFormat==MonitorDataType::FM_COMPLEX_FLOAT)
	      tmpVal.selectedFormat = MonitorDataType::FM_DOUBLE;
	    
	    outputVals.push_back(getRegAsDouble(tmpVal, aspect));
	    
	  }
	  break;
	}
	
      } else {
	outputVals.push_back(getRegAsDouble(val, aspect));
      }
      
    }
  }
  
  return outputVals;
}

void Monitor::outputReg(MonitorDataType val, 
			RegAspect aspect, char* formatPtr)
{
  Complex<float> cf;
  RegDate date;
  
  // First, complex floats and dates must be converted to appropriate
  // double values
  
  switch(val.nativeFormat) {
    
    // Convert complex floats
    
  case MonitorDataType::FM_COMPLEX_FLOAT:
    convertComplexFloatToType(val, aspect);
    break;
    
    // Convert dates
    
  case MonitorDataType::FM_DATE:
    convertDateToType(val);
    break;
  default:
    break;
  }
  
  // Now print
  
  switch(val.selectedFormat) {
  case MonitorDataType::FM_BOOL:
    fprintf(stdout, formatPtr, (bool)val.val.data_.c);
    break;
  case MonitorDataType::FM_CHAR:
  case MonitorDataType::FM_STRING:
    cout << (char)val.val.data_.c;
    break;
  case MonitorDataType::FM_INT:
    fprintf(stdout, formatPtr, val.val.data_.i);
    break;
  case MonitorDataType::FM_UINT:
    fprintf(stdout, formatPtr, val.val.data_.ui);
    break;
  case MonitorDataType::FM_LONG:
    fprintf(stdout, formatPtr, val.val.data_.l);
    break;
  case MonitorDataType::FM_ULONG:
    fprintf(stdout, formatPtr, val.val.data_.ul);
    break;
  case MonitorDataType::FM_DOUBLE:
    fprintf(stdout, formatPtr, val.val.data_.d);
    break;
  case MonitorDataType::FM_FLOAT:
    fprintf(stdout, formatPtr, val.val.data_.f);
    break;
  case MonitorDataType::FM_DATE:
    *date.data() = val.val.data_.date;
    cout << date;
    break;
    
    // Complex floats should be printed specially, unless they are derived aspects
    
  case MonitorDataType::FM_COMPLEX_FLOAT:
    switch(aspect) {
    case REG_PLAIN:
      *cf.data() = val.val.data_.cf;
      cout << cf;
      break;
    default:
      fprintf(stdout, formatPtr, val.val.data_.d);
      break;
    }
    break;
    
  default:
    break;
  }
  
  // Put a space after entries which are not to be printed as
  // strings
  
  if(val.selectedFormat != MonitorDataType::FM_STRING)
    cout << " ";
}

/**.......................................................................
 * Re-format a data type, based on the native format, the aspect, and
 * the selected format
 */
MonitorDataType Monitor::formatReg(MonitorDataType val, 
				   RegAspect aspect, std::string& formatStr)
{
  Complex<float> cf;
  RegDate date;
  
  // First, complex floats and dates must be converted to appropriate
  // double values
  
  switch(val.nativeFormat) {
    
    // Convert complex floats
    
  case MonitorDataType::FM_COMPLEX_FLOAT:
    convertComplexFloatToType(val, aspect);
    break;
    
    // Convert dates
    
  case MonitorDataType::FM_DATE:
    convertDateToType(val);
    break;
  default:
    break;
  }
  
  val.formatStr = formatStr;
  
  return val;
}

/**.......................................................................
 * Re-format a data type, based on the native format, the aspect, and
 * the selected format
 */
void Monitor::formatRegs(std::vector<MonitorDataType>& vals, 
			 MonitorDataType& val,
			 RegAspect aspect, std::string& formatStr)
{
  Complex<float> cf;
  RegDate date;

  for(unsigned i=0; i < vals.size(); i++) {
    vals[i].formatStr = formatStr;
    vals[i].selectedFormat = val.selectedFormat;
    vals[i].nativeFormat   = val.nativeFormat;
    vals[i].aspect         = val.aspect;
  }
  
  // First, complex floats and dates must be converted to appropriate
  // double values
  
  switch(val.nativeFormat) {
    
    // Convert complex floats
    
  case MonitorDataType::FM_COMPLEX_FLOAT:
    for(unsigned i=0; i < vals.size(); i++) 
      convertComplexFloatToType(vals[i], aspect);
    break;
    
    // Convert dates
    
  case MonitorDataType::FM_DATE:
    for(unsigned i=0; i < vals.size(); i++) 
      convertDateToType(vals[i]);
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Output a register as a double
 */
double Monitor::getRegAsDouble(MonitorDataType val, 
			       RegAspect aspect)
{
  Complex<float> cf;
  RegDate date;
  
  // Ensure that the value is cast to a double 
  
  val.selectedFormat = MonitorDataType::FM_DOUBLE;
  
  // First, complex floats and dates must be converted to appropriate
  // double values
  
  switch(val.nativeFormat) {
    
    // Convert complex floats
    
  case MonitorDataType::FM_COMPLEX_FLOAT:
    convertComplexFloatToType(val, aspect);
    break;
    
    // Convert dates
    
  case MonitorDataType::FM_DATE:
    convertDateToType(val);
    break;
  default:
    break;
  }
  
  // Now return the value as a double
  
  return val.val.data_.d;
}

void Monitor::convertDateToType(MonitorDataType& val)
{
  RegDate date;
  double dVal;
  
  if(val.selectedFormat != MonitorDataType::FM_DATE) {
    *date.data() = val.val.data_.date;
    dVal = date.mjd();
  } else
    return;
  
  doubleToType(dVal, val);
}

/**.......................................................................
 * Convert any aspect of a complex float to the appropriate type
 */
void Monitor::convertComplexFloatToType(MonitorDataType& val, RegAspect aspect)
{
  Complex<float>cf;
  
  *cf.data() = val.val.data_.cf;
  
  double dVal;
  
  switch (aspect) {
  case REG_REAL:
    dVal = cf.real();
    break;
  case REG_IMAG:
    dVal = cf.imag();
    break;
  case REG_AMP:
    dVal = cf.amp();
    break;
  case REG_PHASE:
    dVal = cf.phaseInDegrees();
    break;
  default:
    return;
    break;
  }
  
  doubleToType(dVal, val);
}

/**.......................................................................
 * Convert back from a double value to whatever type was requested
 */
void Monitor::doubleToType(double dVal, MonitorDataType& val)
{
  switch (val.selectedFormat) {
  case MonitorDataType::FM_BOOL:
  case MonitorDataType::FM_CHAR:
  case MonitorDataType::FM_STRING:
    val.val.data_.c = (char)dVal;
    break;
  case MonitorDataType::FM_INT:
    val.val.data_.i = (int)dVal;
    break;
  case MonitorDataType::FM_UINT:
    val.val.data_.ui = (unsigned int)dVal;
    break;
  case MonitorDataType::FM_LONG:
    val.val.data_.l = (long)dVal;
    break;
  case MonitorDataType::FM_ULONG:
    val.val.data_.ul = (unsigned long)dVal;
    break;
  case MonitorDataType::FM_FLOAT:
    val.val.data_.f = (float)dVal;
    break;
  case MonitorDataType::FM_DOUBLE:
    val.val.data_.d = (double)dVal;
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Method to sequentially read all data frames and print the value of
 * selected registers according to type.
 */
void Monitor::readRegs() 
{ 
  MsReadState state;
  
  while((state=readNextFrame()) != MS_READ_ENDED) {
    
    if(state==MS_READ_DONE) 
      printRegs2();
  }
}

/**.......................................................................
 * Method to sequentially read all data frames and return the value of
 * selected registers as doubles.
 */
std::vector<std::vector<double> > Monitor::readRegsAsDoubles() 
{ 
  MsReadState state;
  std::vector<std::vector<double> > regVals;
  
  while((state=readNextFrame()) != MS_READ_ENDED) {
    if(state==MS_READ_DONE) {
      regVals.push_back(getRegsAsDoubles());
    }
  }
  return regVals;
}

/**.......................................................................
 * Method to sequentially read all data frames and return the value of
 * selected registers as arbitrary data types.
 */
std::vector<std::vector<std::vector<MonitorDataType> > > 
Monitor::readRegsAsDataTypes() 
{ 
  MsReadState state;
  std::vector<std::vector<std::vector<MonitorDataType> > > regVals;
  
  while((state=readNextFrame()) != MS_READ_ENDED) {
    
    if(state==MS_READ_DONE) {
      regVals.push_back(getRegsAsDataTypes());
    }
  }
  
  return regVals;
}

/**.......................................................................
 * If we are doing a hybrid read, change monitor streams at the end of
 * the requested time range.
 */
void Monitor::changeMonitorStream() 
{ 
  LogStream errStr;
  MonitorStream* ms = 0;
  
  ms = new_NetMonitorStream((char*)host_.c_str());
  
  if(ms_ == 0) {
    errStr.appendMessage(true, "Unable to open the archive stream");
    throw Error(errStr);
  }
  
  setMonitoringInterval(ms_->interval);
  
  ms_ = del_MonitorStream(ms_);
  
  ms_ = ms;
  
  initialized_ = false;
}

/**.......................................................................
 * Parse a register specification string
 */
void Monitor::addRegister(string regSpec)
{
  InputStream *stream=NULL; // The stream to parse from
  int start;                // The index of the first non-space
  // character
  LogStream errStr;
  double mjd;
  
  // Find the location of the first non-space character in the string.
  
  for(start=0; isspace((int) (regSpec.c_str())[start]); start++);
  
  // If the string is empty then return the wildcard register designator.
  
  if(regSpec.c_str()[start] == '\0') 
    errStr.appendMessage(true, "Empty string");
  
  // Attach an input stream to the specification string.
  
  if(!errStr.isError())
    if((stream = new_InputStream())==NULL) 
      errStr.appendMessage(true, "Insufficient memory for new stream.");
  
  // Now connect the new stream to the input string.
  
  if(!errStr.isError())
    if(open_StringInputStream(stream, 0, (char*)regSpec.c_str() + start)) 
      errStr.appendMessage(true, "Stream failure.");
  
  // Now call the stream addRegister method
  
  if(!errStr.isError())
    addRegister(stream);
  
  // The stream is now redundant.
  
  close_InputStream(stream);
  
  // Report errors.
  
  if(errStr.isError()) 
    throw Error(errStr);
}

/**.......................................................................
 * Parse a date string of the form dd-mmm-yyyy:hh:mm:ss into a double
 * Modified Julian date
 */
double Monitor::parseDateAndTime(string utc)
{
  InputStream *stream=NULL; // The stream to parse from
  int start;                // The index of the first non-space
  // character
  LogStream errStr;
  double mjd;
  
  // Find the location of the first non-space character in the string.
  
  for(start=0; isspace((int) (utc.c_str())[start]); start++);
  
  // If the string is empty then return the wildcard utc designator.
  
  if(utc.c_str()[start] == '\0') 
    return 0.0;
  
  // Attach an input stream to the specification string.
  
  if((stream = new_InputStream())==NULL) 
    errStr.appendMessage(true, "Insufficient memory for new stream.");
  
  // Now connect the new stream to the input string.
  
  if(open_StringInputStream(stream, 0, (char*)utc.c_str() + start)) 
    errStr.appendMessage(true, "Stream failure.");
  
  // Parse the date and time from the string.
  
  if(!errStr.isError())
    if(input_utc(stream, 1, 0, &mjd))
      errStr.appendMessage(true, "Error extracting utc");
  
  // The stream is now redundant.
  
  close_InputStream(stream);
  
  // Report errors.
  
  if(errStr.isError()) 
    throw Error(errStr);
  
  return mjd;
}

/**.......................................................................
 * Run in interactive mode.
 */
void Monitor::run()
{
  InputStream* stream=0;
  LogStream errStr;
  FdSet fdSet;
  
  stream = new_InputStream();
  
  if(stream==0) {
    errStr.appendMessage(true, "Unable to allocate stream");
    del_InputStream(stream);
    throw Error(errStr);
  }
  
  // Open the appropriate file
  
  if(readRegSpecFromFile_) {
    if(open_FileInputStream(stream, "", (char*)regFile_.c_str())) {
      errStr.appendMessage(true, "Unable to allocate stream");
      stream = del_InputStream(stream);
      throw Error(errStr);
    }
  } else {
    if(open_StdioInputStream(stream, 0, stdin)) {
      errStr.appendMessage(true, "Unable to allocate stream");
      stream = del_InputStream(stream);
      throw Error(errStr);
    }
  }
  
  // Now loop checking for valid keywords
  
  bool stop = false;
  
  while(!stop) {
    switch(readKeyword(stream)) {
    case ADD_REGISTER:
      try {
	addRegister(stream);
      } catch(Exception& err) {
	cerr << err.what() << endl;
	skipToNextLine(stream);
      }
      break;
    case READ:
      stop = true;
      break;
    default:
      input_error(stream, 1, "Unrecognized keyword: %s\n", stream->work);
      skipToNextLine(stream);
      break;
    }
  }
  readRegs();
}

/**.......................................................................
 * Read a keyword from a stream
 */
Monitor::KeywordType Monitor::readKeyword(InputStream* stream)
{
  LogStream errStr;
  
  // If EOF was encountered, terminate keyword parsing as if a "read"
  // keyword had ben encountered.
  
  if(stream->nextc == EOF)
    return READ;
  
  // Read the keyword, skipping any leading or trailing whitespaces
  
  if(input_skip_white(stream, 1, 0) || input_word(stream, 0, 1)) {
    errStr.appendMessage(true, "Error reading keyword");
    throw Error(errStr);
  }
  
  // Else check for known keywords
  
  for(unsigned ikey=0; ikey < nKey_; ikey++)
    if(keywords[ikey].keyword.compare(stream->work)==0)
      return keywords[ikey].type;
  
  return UNKNOWN;
}

/**.......................................................................
 * Read a format from a stream
 */
MonitorDataType::FormatType Monitor::readFormat(InputStream* stream)
{
  if(stream->nextc != EOF && stream->nextc != '\n') {
    if(input_skip_space(stream, 1, 0)) {
      ThrowError("Error reading format");
    } 
  }
  
  // See if a format was specified.  If the next char is not a space,
  // there is no more to be read.
  
  if(stream->nextc==EOF || stream->nextc=='\n') {
    return MonitorDataType::FM_UNKNOWN;
  }
  
  // Read the format, skipping any leading whitespace
  
  if(input_word(stream, 0, 1)) {
    ThrowError("Error reading format");
  } 
  
  // Else check for known formats
  
  for(unsigned iformat=0; iformat < nFormat_; iformat++)
    if(formats[iformat].format.compare(stream->work)==0) {
      return formats[iformat].type;
    }
  
  return MonitorDataType::FM_UNKNOWN;
}

/**.......................................................................
 * Read a format string
 */
char* Monitor::readFormatString(InputStream* stream)
{
  LogStream errStr;
  std::string formatString;
  
  // Read any white space remaining before the next token, or the end
  // of the line
  
  if(stream->nextc != EOF && stream->nextc != '\n')
    if(input_skip_space(stream, 1, 0)) {
      errStr.appendMessage(true, "Error skipping whitespace");
      throw Error(errStr);
    } 
  
  // See if a format was specified.  If the next char is not a space,
  // there is no more to be read.
  
  if(stream->nextc==EOF || stream->nextc=='\n')
    return 0;
  
  // Read the format
  
  if(input_quoted_string(stream, 1)) {
    errStr.appendMessage(true, "Error reading format string");
    throw Error(errStr);
  } 
  
  return stream->work;
}

/**.......................................................................
 * Skip to the beginning of the next line
 */
void Monitor::skipToNextLine(InputStream* stream) 
{
  LogStream errStr;
  
  if(input_skip_past_eol(stream, 1) || input_skip_white(stream, 1, 0)) {
    errStr.appendMessage(true, "Error skipping to the next line");
    throw Error(errStr);
  }
}

MonitorDataType::FormatType Monitor::formatOf(RegDescription& reg, MonitorDataType::FormatType format)
{
  if(format != MonitorDataType::FM_UNKNOWN)
    return format;
  
  switch (sza::util::DataType::typeOf(reg.block())) {
  case sza::util::DataType::UCHAR:
    return MonitorDataType::FM_UCHAR;
    break;
  case sza::util::DataType::CHAR:
    return MonitorDataType::FM_CHAR;
    break;
  case sza::util::DataType::USHORT:
    return MonitorDataType::FM_USHORT;
    break;
  case sza::util::DataType::SHORT:
    return MonitorDataType::FM_SHORT;
    break;
  case sza::util::DataType::UINT:
    return MonitorDataType::FM_UINT;
    break;
  case sza::util::DataType::INT:
    return MonitorDataType::FM_INT;
    break;
  case sza::util::DataType::ULONG:
    return MonitorDataType::FM_ULONG;
    break;
  case sza::util::DataType::LONG:
    return MonitorDataType::FM_LONG;
    break;
  case sza::util::DataType::FLOAT:
    return MonitorDataType::FM_FLOAT;
    break;
  case sza::util::DataType::DOUBLE:
    return MonitorDataType::FM_DOUBLE;
    break;
  case sza::util::DataType::DATE:
    return MonitorDataType::FM_DATE;
    break;
  case sza::util::DataType::COMPLEX_FLOAT:
    
    switch (reg.aspect()) {
    case REG_PLAIN:
      return MonitorDataType::FM_COMPLEX_FLOAT;
      break;
    default:
      return MonitorDataType::FM_COMPLEX_FLOAT;
      break;
    }
    
    break;
  default:
    return MonitorDataType::FM_UNKNOWN;
    break;
  }
}

std::string Monitor::formatStringOf(MonitorDataType::FormatType format, RegAspect aspect)
{
  switch(format) {
  case MonitorDataType::FM_BOOL:
  case MonitorDataType::FM_CHAR:
  case MonitorDataType::FM_UCHAR:
    return std::string("%d");
    break;
  case MonitorDataType::FM_STRING:
    return std::string("%s");
    break;
  case MonitorDataType::FM_SHORT:
  case MonitorDataType::FM_USHORT:
    return std::string("%d");
    break;
  case MonitorDataType::FM_INT:
  case MonitorDataType::FM_UINT:
    return std::string("%d");
    break;
  case MonitorDataType::FM_LONG:
  case MonitorDataType::FM_ULONG:
    return std::string("%ld");
    break;
  case MonitorDataType::FM_FLOAT:
  case MonitorDataType::FM_DOUBLE:
  case MonitorDataType::FM_COMPLEX_FLOAT:
    return std::string("%8.8f");
    break;
  case MonitorDataType::FM_DATE:
    return std::string("%s");
    break;
  default:
    break;
  }
}

/**.......................................................................
 * Return a reference to the calibrated cata for the last read frame
 */
double* Monitor::getCalSlotPtr(unsigned iSlot)
{
  return ms_->regCal->getSlotPtr(iSlot);
}
